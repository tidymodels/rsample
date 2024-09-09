#' Find Labels from rset Object
#'
#' Produce a vector of resampling labels (e.g. "Fold1") from
#'  an `rset` object. Currently, `nested_cv`
#'  is not supported.
#'
#' @param object An `rset` object
#' @param make_factor A logical for whether the results should be
#'  a character or a factor.
#' @param ... Not currently used.
#' @return A single character or factor vector.
#' @export
#' @examples
#' labels(vfold_cv(mtcars))
labels.rset <- function(object, make_factor = FALSE, ...) {
  if (inherits(object, "nested_cv")) {
    cli_abort("{.arg labels} not implemented for nested resampling")
  }
  if (make_factor) {
    as.factor(object$id)
  } else {
    as.character(object$id)
  }
}

#' @rdname labels.rset
#' @export
labels.vfold_cv <- function(object, make_factor = FALSE, ...) {
  if (inherits(object, "nested_cv")) {
    cli_abort("{.arg labels} not implemented for nested resampling")
  }
  is_repeated <- attr(object, "repeats") > 1
  if (is_repeated) {
    out <- as.character(paste(object$id, object$id2, sep = "."))
  } else {
    out <- as.character(object$id)
  }
  if (make_factor) {
    out <- as.factor(out)
  }
  out
}

#' Find Labels from rsplit Object
#'
#' Produce a tibble of identification variables so that single
#'  splits can be linked to a particular resample.
#'
#' @param object An `rsplit` object
#' @param ... Not currently used.
#' @return A tibble.
#' @seealso add_resample_id
#' @export
#' @examples
#' cv_splits <- vfold_cv(mtcars)
#' labels(cv_splits$splits[[1]])
labels.rsplit <- function(object, ...) {
  out <- if ("id" %in% names(object)) {
    object$id
  } else {
    tibble()
  }
  out
}

#' Augment a data set with resampling identifiers
#'
#' For a data set, `add_resample_id()` will add at least one new column that
#'  identifies which resample that the data came from. In most cases, a single
#'  column is added but for some resampling methods, two or more are added.
#' @param .data A data frame
#' @param split A single `rset` object.
#' @param dots A single logical: should the id columns be prefixed with a "."
#'  to avoid name conflicts with `.data`?
#' @return An updated data frame.
#' @examples
#' library(dplyr)
#'
#' set.seed(363)
#' car_folds <- vfold_cv(mtcars, repeats = 3)
#'
#' analysis(car_folds$splits[[1]]) %>%
#'   add_resample_id(car_folds$splits[[1]]) %>%
#'   head()
#'
#' car_bt <- bootstraps(mtcars)
#'
#' analysis(car_bt$splits[[1]]) %>%
#'   add_resample_id(car_bt$splits[[1]]) %>%
#'   head()
#' @seealso labels.rsplit
#' @export
add_resample_id <- function(.data, split, dots = FALSE) {
  if (!inherits(dots, "logical") || length(dots) > 1) {
    cli_abort("{.arg dots} should be a single logical.")
  }
  if (!inherits(.data, "data.frame")) {
    cli_abort("{.arg .data} should be a data frame.")
  }
  if (!inherits(split, "rsplit")) {
    cli_abort("{.arg split} should be a single 'rset' object.")
  }
  labs <- labels(split)

  if (!tibble::is_tibble(labs) && nrow(labs) == 1) {
    cli_abort("{.arg split} should be a single 'rset' object.")
  }

  if (dots) {
    colnames(labs) <- paste0(".", colnames(labs))
  }

  cbind(.data, labs)
}
