#' Wrapper Function for Preparing Recipes
#' 
#' When working with the \pkg{recipes} package, a simple recipe
#'  must be \emph{prepared} using the \code{\link[recipes]{prep}}
#'  function first. When using recipies with \pkg{rsample} it
#'  is helpful to have a function that can prepare a recipe
#'  across a series of \code{split} objects that are produced
#'  in this package. \code{prepper} is a wrapper function
#'  around \code{\link[recipes]{prep}} that can be used to do
#'  this. See the vignette on "Recipes and rsample" for an
#'  example.
#' @param split_obj An \code{rplit} object
#' @param recipe An untrained \code{recipe} object.
#' @param ... Arguments to pass to \code{\link[recipes]{prep}}
#'  such as \code{verbose} or \code{retain}.
#' @export
#' @importFrom recipes prep
prepper <- function(split_obj, recipe, ...) {
  prep(recipe, training = analysis(split_obj, recipe = FALSE), ...)
}
# alternate names: chef? dr_prepper?