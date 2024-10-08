#' Extract Predictor Names from Formula or Terms
#'
#' While [all.vars()] returns all variables used in a formula, this
#'  function only returns the variables explicitly used on the
#'  right-hand side (i.e., it will not resolve dots unless the
#'  object is terms with a data set specified).
#' @param object A model formula or [stats::terms()]
#'  object.
#' @param ... Arguments to pass to [all.vars()]
#' @return A character vector of names
#' @export
#' @examples
#' form_pred(y ~ x + z)
#' form_pred(terms(y ~ x + z))
#'
#' form_pred(y ~ x + log(z))
#' form_pred(log(y) ~ x + z)
#'
#' form_pred(y1 + y2 ~ x + z)
#' form_pred(log(y1) + y2 ~ x + z)
#'
#' # will fail:
#' # form_pred(y ~ .)
#'
#' form_pred(terms(mpg ~ (.)^2, data = mtcars))
#' form_pred(terms(~ (.)^2, data = mtcars))
#' @importFrom stats terms

form_pred <- function(object, ...) {
  if (inherits(object, "formula")) {
    object <- terms(object)
  }
  y_index <- attr(object, "response")

  ## If there is something on the lhs of the formula,
  ## remove it and get vars
  if (y_index != 0) {
    object[[2]] <- NULL
    object <- terms(object)
  }
  all.vars(object, ...)
}
