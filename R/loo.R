#' Leave-One-Out Cross-Validation
#'
#' Leave-one-out (LOO) cross-validation uses one data point in the original set as the assessment data and all other data points as the analysis set. A LOO resampling set has ass many resamples as rows in the original data set. 
#'
#' @inheritParams vfold_cv
#' @return  An tibble with classes \code{loo_cv}, \code{rset}, \code{tbl_df}, \code{tbl}, and \code{data.frame}. The results include a column for the data split objects and one column called \code{id} that has a character string with the resample identifier.
#' @examples
#' loo_cv(mtcars)
#' @importFrom purrr map
#' @export
loo_cv <- function(data, ...) {
  split_objs <- vfold_splits(data = data, v = nrow(data))
  split_objs$splits <- map(split_objs$splits, change_class)
  split_objs$id <- paste0("Resample", seq_along(split_objs$id))
  class(split_objs) <- c("loo_cv", "rset", class(split_objs))
  split_objs
}

#' @export
print.loo_cv <- function(x, ...) {
  cat("# Leave-one-out cross-validation\n")
  class(x) <- class(x)[!(class(x) %in% c("loo_cv", "rset"))]
  print(x)
}

change_class <- function(x) {
  class(x) <- c("rsplit", "loo_split")
  x
}

