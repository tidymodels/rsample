#' Find Labels from rset Object
#'
#' Produce a vector of resampling labels (e.g. "Fold1") from
#'  an `rset` object. Currently, `nested_cv`
#'  is not supported.
#'
#' @param object An `rset` object
#' @param make_factor A logical for whether the results should be
#'  character or a factor.
#' @param ... Not currently used.
#' @return A single character or factor vector.
#' @export
#' @examples
#' labels(vfold_cv(mtcars))
labels.rset <- function(object, make_factor = FALSE, ...) {
  if(inherits(object, "nested_cv"))
    stop("`labels` not implemented for nested resampling",
         call. = FALSE)
  if (make_factor)
    as.factor(object$id) else
      as.character(object$id)
}

#' @rdname labels.rset
#' @export
labels.vfold_cv <- function(object, make_factor = FALSE, ...) {
  if(inherits(object, "nested_cv"))
    stop("`labels` not implemented for nested resampling",
         call. = FALSE)
  is_repeated <- attr(object, "repeats") > 1
  if (is_repeated) {
    out <- as.character(paste(object$id, object$id2, sep = "."))
  } else
    out <- as.character(object$id)
  if (make_factor)
    out <- as.factor(out)
  out
}
