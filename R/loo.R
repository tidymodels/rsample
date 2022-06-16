#' Leave-One-Out Cross-Validation
#'
#' Leave-one-out (LOO) cross-validation uses one data point in the original
#'  set as the assessment data and all other data points as the analysis set. A
#'  LOO resampling set has as many resamples as rows in the original data set.
#' @inheritParams vfold_cv
#' @return An tibble with classes `loo_cv`, `rset`, `tbl_df`, `tbl`, and
#'  `data.frame`. The results include a column for the data split objects and
#'  one column called `id` that has a character string with the resample
#'  identifier.
#' @examples
#' loo_cv(mtcars)
#' @export
loo_cv <- function(data, ...) {
  split_objs <- vfold_splits(data = data, v = nrow(data))
  split_objs <-
    list(
      splits = map(split_objs$splits, change_class),
      id = paste0("Resample", seq_along(split_objs$id))
    )

  ## We remove the holdout indices since it will save space and we can
  ## derive them later when they are needed.

  split_objs$splits <- map(split_objs$splits, rm_out)

  new_rset(
    splits = split_objs$splits,
    ids = split_objs$id,
    subclass = c("loo_cv", "rset")
  )
}

change_class <- function(x) {
  class(x) <- c("rsplit", "loo_split")
  x
}
