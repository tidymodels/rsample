#' Inner split of the analysis set for fitting a post-processor
#' 
#' @param x An `rsplit` object.
#' @param split_args A list of arguments to be used for the inner split.
#' @param ... Not currently used.
#' @return An `rsplit` object.
#' @keywords internal
#' @export
inner_split <- function(x, ...) {
  UseMethod("inner_split")
}

# mc ---------------------------------------------------------------------

#' @rdname inner_split
#' @export
inner_split.mc_split <- function(x, split_args, ...) {
  check_dots_empty() 

  analysis_set <- analysis(x)
  
  split_inner <- mc_splits(
    analysis_set, 
    prop = split_args$prop, 
    times = 1,
    strata = split_args$strata, 
    breaks = split_args$breaks, 
    pool = split_args$pool
  )
  split_inner <- split_inner$splits[[1]]

  class_inner <- paste0(class(x)[1], "_inner")
  split_inner <- add_class(split_inner, class_inner)
  split_inner
}

#' @rdname inner_split
#' @export
inner_split.grouped_mc_split <- function(x, split_args, ...) {
  # FIXME update this class to `group_mc_split`
  check_dots_empty() 

  analysis_set <- analysis(x)
  
  split_inner <- group_mc_splits(
    analysis_set, 
    group = split_args$group,
    prop = split_args$prop, 
    times = 1,
    strata = split_args$strata, 
    pool = split_args$pool
  )
  split_inner <- split_inner$splits[[1]]

  class_inner <- paste0(class(x)[1], "_inner")
  split_inner <- add_class(split_inner, class_inner)
  split_inner
}
