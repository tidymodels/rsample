#' Obtain a identifier for the resamples
#'
#' This function returns a hash (or NA) for an attribute that is created when
#' the `rset` was initially constructed. This can be used to compare with other
#' resampling objects to see if they are the same.
#' @param x An `rset` or `tune_results` object.
#' @param ... Not currently used.
#' @return A character value or `NA_character_` if the object was created prior
#' to rsample version 0.1.0.
#' @rdname get_fingerprint
#' @aliases .get_fingerprint
#' @examples
#' set.seed(1)
#' .get_fingerprint(vfold_cv(mtcars))
#'
#' set.seed(1)
#' .get_fingerprint(vfold_cv(mtcars))
#'
#' set.seed(2)
#' .get_fingerprint(vfold_cv(mtcars))
#'
#' set.seed(1)
#' .get_fingerprint(vfold_cv(mtcars, repeats = 2))
#' @export
.get_fingerprint <- function(x, ...) {
  UseMethod(".get_fingerprint")
}

#' @export
#' @rdname get_fingerprint
.get_fingerprint.default <- function(x, ...) {
  cls <- class(x)
  cli_abort("No method for objects of class{?es}: {cls}")
}

#' @export
#' @rdname get_fingerprint
.get_fingerprint.rset <- function(x, ...) {
  check_dots_empty()
  att <- attributes(x)
  if (any(names(att) == "fingerprint")) {
    res <- att$fingerprint
  } else {
    res <- NA_character_
  }
  res
}
