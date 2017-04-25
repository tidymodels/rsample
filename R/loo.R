#' Leave-One-Out Cross-Validation
#'
#'
#'
#' @inheritParams vfold_cv
#' @export
#' @return  An object with classes \code{"loo_cv"} and \code{"rset"}. The elements of the object include a tibble called \code{splits} that contains a column for the data split objects and one column called \code{id} that has a character string with the resample identifier.
#' @examples
#' loo_cv(mtcars)
#' @export
loo_cv <- function(data, ...) {
  split_objs <- vfold_splits(data = data, v = nrow(data))
  split_objs$id <- paste0("Resample", seq_along(split_objs$id))
  structure(list(splits = split_objs), 
            class = c("loo_cv", "rset"))
}

#' @export
print.loo_cv <- function(x, ...) {
  cat("Leave-one-out cross-validation\n")
}