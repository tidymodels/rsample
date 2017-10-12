#' Wrapper Function for Preparing Recipes
#' 
#' When working with the \pkg{recipes} package, a simple recipe
#'  must be *prepared* using the [recipes::prep()]
#'  function first. When using recipies with \pkg{rsample} it
#'  is helpful to have a function that can prepare a recipe
#'  across a series of `split` objects that are produced
#'  in this package. `prepper` is a wrapper function
#'  around [recipes::prep()] that can be used to do
#'  this. See the vignette on "Recipes and rsample" for an
#'  example.
#' @param split_obj An `rplit` object
#' @param recipe An untrained `recipe` object.
#' @param ... Arguments to pass to [recipes::prep()]
#'  such as `verbose` or `retain`.
#' @export
#' @importFrom recipes prep
prepper <- function(split_obj, recipe, ...) {
  prep(recipe, training = analysis(split_obj, recipe = FALSE), ...)
}
# alternate names: chef? dr_prepper?
