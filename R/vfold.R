#' V-Fold Cross-Validation
#'
#' V-fold cross-validation randomly splits the data into V groups of roughly equal size (called "folds"). A resample of the analysis data consisted of V-1 of the folds while the assessment set contains the final fold. In basic V-fold cross-validation (i.e. no repeats), the number of resamples is equal to V.

#' @details
#' The `strata` argument causes the random sampling to be conducted *within the stratification variable*. The can help ensure that the number of data points in the analysis data is equivalent to the proportions in the original data set.
#'
#' When more than one repeat is requested, the basic V-fold cross-validation is conducted each time. For example, if three repeats are used with `v = 10`, there are a total of 30 splits which as three groups of 10 that are generated separately.
#'
#' @param data A data frame.
#' @param v The number of partitions of the data set.
#' @param repeats The number of times to repeat the V-fold partitioning.
#' @param strata A variable that is used to conduct stratified sampling to create the folds. This should be a single character value.
#' @param ... Not currently used.
#' @export
#' @return  A tibble with classes `vfold_cv`, `rset`, `tbl_df`, `tbl`, and `data.frame`. The results include a column for the data split objects and one or more identification variables. For a single repeats, there will be one column called `id` that has a character string with the fold identifier. For repeats, `id` is the repeat number and an additional column called `id2` that contains the fold information (within repeat).
#' @examples
#' vfold_cv(mtcars, v = 10)
#' vfold_cv(mtcars, v = 10, repeats = 2)
#'
#' library(purrr)
#' iris2 <- iris[1:130, ]
#'
#' set.seed(13)
#' folds1 <- vfold_cv(iris2, v = 5)
#' map_dbl(folds1$splits,
#'         function(x) {
#'           dat <- as.data.frame(x)$Species
#'           mean(dat == "virginica")
#'         })
#'
#' set.seed(13)
#' folds2 <- vfold_cv(iris2, strata = "Species", v = 5)
#' map_dbl(folds2$splits,
#'         function(x) {
#'           dat <- as.data.frame(x)$Species
#'           mean(dat == "virginica")
#'         })
#' @export
vfold_cv <- function(data, v = 10, repeats = 1, strata = NULL, ...) {

  strata_check(strata, names(data))

  if (repeats == 1) {
    split_objs <- vfold_splits(data = data, v = v, strata = strata)
  } else {
    for (i in 1:repeats) {
      tmp <- vfold_splits(data = data, v = v, strata = strata)
      tmp$id2 <- tmp$id
      tmp$id <- names0(repeats, "Repeat")[i]
      split_objs <- if (i == 1)
        tmp
      else
        rbind(split_objs, tmp)
    }
  }

  ## We remove the holdout indicies since it will save space and we can
  ## derive them later when they are needed.

  split_objs$splits <- map(split_objs$splits, rm_out)

  ## Save some overall information

  cv_att <- list(v = v, repeats = repeats, strata = !is.null(strata))

  new_rset(splits = split_objs$splits,
           ids = split_objs[, grepl("^id", names(split_objs))],
           attrib = cv_att,
           subclass = c("vfold_cv", "rset"))
}

# Get the indices of the analysis set from the assessment set
vfold_complement <- function(ind, n) {
  list(analysis = setdiff(1:n, ind),
       assessment = ind)
}

#' @importFrom tibble tibble
#' @importFrom purrr map
#' @importFrom dplyr bind_rows
vfold_splits <- function(data, v = 10, strata = NULL) {
  if (!is.numeric(v) || length(v) != 1)
    stop("`v` must be a single integer.", call. = FALSE)

  n <- nrow(data)
  if (is.null(strata)) {
    folds <- sample(rep(1:v, length.out = n))
    idx <- seq_len(n)
    indices <- split(idx, folds)
  } else {
    stratas <- tibble::tibble(idx = 1:n,
                              strata = make_strata(getElement(data, strata)))
    stratas <- split(stratas, stratas$strata)
    stratas <- purrr::map(stratas, add_vfolds, v = v)
    stratas <- dplyr::bind_rows(stratas)
    indices <- split(stratas$idx, stratas$folds)
  }

  indices <- lapply(indices, vfold_complement, n = n)

  split_objs <- purrr::map(indices, make_splits, data = data, class = "vfold_split")
  tibble::tibble(splits = split_objs,
                 id = names0(length(split_objs), "Fold"))
}

add_vfolds <- function(x, v) {
  x$folds <- sample(rep(1:v, length.out = nrow(x)))
  x
}

#' @export
print.vfold_cv <- function(x, ...) {
  cat("# ", pretty(x), "\n")
  class(x) <- class(x)[!(class(x) %in% c("vfold_cv", "rset"))]
  print(x)
}
