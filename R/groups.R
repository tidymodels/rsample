#' Group V-Fold Cross-Validation
#'
#' Group V-fold cross-validation creates splits of the data based
#'  on some grouping variable (which may have more than a single row
#'  associated with it). The function can create as many splits as
#'  there are unique values of the grouping variable or it can
#'  create a smaller set of splits where more than one value is left
#'  out at a time. A common use of this kind of resampling is when you have
#'  repeated measures of the same subject.
#'
#' @param data A data frame.
#' @param group A variable in `data` (single character or name) used for
#'  grouping observations with the same value to either the analysis or
#'  assessment set within a fold.
#' @param v The number of partitions of the data set. If let
#'  `NULL`, `v` will be set to the number of unique values
#'  in the group.
#' @param balance If `v` is less than the number of unique groups, how should
#'  groups be combined into folds? If `"groups"`, the default, then groups are
#'  combined randomly to balance the number of groups in each fold.
#'  If `"observations"`, then groups are combined to balance the number of
#'  observations in each fold.
#' @param ... Not currently used.
#' @export
#' @return A tibble with classes `group_vfold_cv`,
#'  `rset`, `tbl_df`, `tbl`, and `data.frame`.
#'  The results include a column for the data split objects and an
#'  identification variable.
#' @examplesIf rlang::is_installed("modeldata")
#' data(Sacramento, package = "modeldata")
#'
#' set.seed(123)
#' group_vfold_cv(Sacramento, group = city, v = 5)
#' group_vfold_cv(Sacramento, group = city, v = 5, balance = "observations")
#'
#' @export
group_vfold_cv <- function(data, group = NULL, v = NULL, balance = c("groups", "observations"), ...) {
  if (!missing(group)) {
    group <- tidyselect::vars_select(names(data), !!enquo(group))
    if (length(group) == 0) {
      group <- NULL
    }
  }

  if (is.null(group) || !is.character(group) || length(group) != 1) {
    rlang::abort(
      "`group` should be a single character value for the column that will be used for splitting."
    )
  }
  if (!any(names(data) == group)) {
    rlang::abort("`group` should be a column in `data`.")
  }

  balance <- rlang::arg_match(balance)

  split_objs <- group_vfold_splits(data = data, group = group, v = v, balance = balance)

  ## We remove the holdout indices since it will save space and we can
  ## derive them later when they are needed.

  split_objs$splits <- map(split_objs$splits, rm_out)

  # Update `v` if not supplied directly
  if (is.null(v)) {
    v <- length(split_objs$splits)
  }

  ## Save some overall information

  cv_att <- list(v = v, group = group)

  new_rset(
    splits = split_objs$splits,
    ids = split_objs[, grepl("^id", names(split_objs))],
    attrib = cv_att,
    subclass = c("group_vfold_cv", "rset")
  )
}

group_vfold_splits <- function(data, group, v = NULL, balance = c("groups", "observations")) {

  balance <- rlang::arg_match(balance)


  uni_groups <- unique(getElement(data, group))
  max_v <- length(uni_groups)

  if (is.null(v)) {
    v <- max_v
  } else {
    check_v(v = v, max_v = max_v, rows = "rows", call = rlang::caller_env())
  }
  data_ind <- data.frame(..index = 1:nrow(data), ..group = getElement(data, group))
  data_ind$..group <- as.character(data_ind$..group)

  if (balance == "groups") {
    keys <- data.frame(..group = uni_groups)
    keys$..folds <- sample(rep(1:v, length.out = max_v))
  } else if (balance == "observations") {
    while (vec_unique_count(data_ind$..group) > v) {
      freq_table <- vec_count(data_ind$..group)
      # Recategorize the largest group to be collapsed
      # as the smallest group to be kept
      group_to_keep <- vec_slice(freq_table, v)
      group_to_collapse <- vec_slice(freq_table, v + 1)
      collapse_lgl <- vec_in(data_ind$..group, group_to_collapse$key)
      vec_slice(data_ind$..group, collapse_lgl) <- group_to_keep$key
    }

    keys <- data.frame(..group = unique(data_ind$..group))
    n <- nrow(keys)
    keys$..folds <- sample(rep(1:v, length.out = n))
  }

  keys$..group <- as.character(keys$..group)

  data_ind <- data_ind %>%
    full_join(keys, by = "..group") %>%
    arrange(..index)
  indices <- split_unnamed(data_ind$..index, data_ind$..folds)
  indices <- lapply(indices, default_complement, n = nrow(data))
  split_objs <-
    purrr::map(indices,
      make_splits,
      data = data,
      class = "group_vfold_split"
    )
  tibble::tibble(
    splits = split_objs,
    id = names0(length(split_objs), "Resample")
  )
}
