#' Group V-Fold Cross-Validation
#'
#' Group V-fold cross-validation creates splits of the data based
#'  on some grouping variable (which may have more than a single row
#'  associated with it). The function can create as many splits as
#'  there are unique values of the grouping variable or it can
#'  create a smaller set of splits where more than one value is left
#'  out at a time.
#'
#' @param data A data frame.
#' @param group A signle character value for the column of the
#'  data that will be used to create the splits. 
#' @param v The number of partitions of the data set. If let
#'  `NULL`, `v` will be set to the number of unique values
#'  in the group.
#' @param ... Not currently used. 
#' @export
#' @return An tibble with classes `group_vfold_cv`,
#'  `rset`, `tbl_df`, `tbl`, and `data.frame`.
#'  The results include a column for the data split objects and an
#'  identification variable.
#' @examples
#' set.seed(3527)
#' test_data <- data.frame(id = sort(sample(1:20, size = 80, replace = TRUE)))
#' test_data$dat <- runif(nrow(test_data))
#' 
#' set.seed(5144)
#' split_by_id <- group_vfold_cv(test_data, group = "id")
#' 
#' get_id_left_out <- function(x)
#'   unique(assessment(x)$id)
#' 
#' library(purrr)
#' table(map_int(split_by_id$splits, get_id_left_out))
#' 
#' set.seed(5144)
#' split_by_some_id <- group_vfold_cv(test_data, group = "id", v = 7)
#' held_out <- map(split_by_some_id$splits, get_id_left_out)
#' table(unlist(held_out))
#' # number held out per resample:
#' map_int(held_out, length)
#' @export
group_vfold_cv <- function(data, group = NULL, v = NULL, ...) {
  if (is.null(group) || !is.character(group) || length(group) != 1)
    stop(
      "`group` should be a single character value for the column ",
      "that will be used for splitting.",
      call. = FALSE
    )
  if (!any(names(data) == group))
    stop("`group` should be a column in `data`.", call. = FALSE)
  
  split_objs <- group_vfold_splits(data = data, group = group, v = v)
  
  ## We remove the holdout indicies since it will save space and we can 
  ## derive them later when they are needed. 
  
  split_objs$splits <- map(split_objs$splits, rm_out)
  
  ## Save some overall information
  
  cv_att <- list(v = v, group = group)
  
  new_rset(splits = split_objs$splits, 
           ids = split_objs[, grepl("^id", names(split_objs))],
           attrib = cv_att, 
           subclass = c("group_vfold_cv", "rset")) 
}

#' @importFrom dplyr %>%
group_vfold_splits <- function(data, group, v = NULL) {
  uni_groups <- unique(getElement(data, group))
  max_v <- length(uni_groups)
  
  if (is.null(v)) {
    v <- max_v
  } else {
    if (v > max_v)
      stop("`v` should be less than ", max_v, call. = FALSE)
  }
  data_ind <- data.frame(..index = 1:nrow(data), ..group = getElement(data, group))
  keys <- data.frame(..group = uni_groups)
  
  n <- nrow(keys)
  keys$..folds <- sample(rep(1:v, length.out = n))
  data_ind <- data_ind %>% 
    full_join(keys, by = "..group") %>%
    arrange(..index)
  indices <- split(data_ind$..index, data_ind$..folds)
  indices <- lapply(indices, vfold_complement, n = nrow(data))
  split_objs <-
    purrr::map(indices,
               make_splits,
               data = data,
               class = "group_vfold_split")
  tibble::tibble(splits = split_objs, 
                 id = names0(length(split_objs), "Resample"))
}

#' @export
print.group_vfold_cv <- function(x, ...) {
  cat("#", pretty(x), "\n")
  class(x) <- class(x)[!(class(x) %in% c("group_vfold_cv", "rset"))]
  print(x)
}

#' @importFrom utils globalVariables
utils::globalVariables("..index")
