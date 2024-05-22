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

  split_args$times <- 1
  split_inner <- rlang::inject(
    mc_splits(analysis_set, !!!split_args)
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

  split_args$times <- 1
  split_inner <- rlang::inject(
    group_mc_splits(analysis_set, !!!split_args)
  )
  split_inner <- split_inner$splits[[1]]

  class_inner <- paste0(class(x)[1], "_inner")
  split_inner <- add_class(split_inner, class_inner)
  split_inner
}


# vfold ------------------------------------------------------------------

#' @rdname inner_split
#' @export
inner_split.vfold_split <- function(x, split_args, ...) {
  check_dots_empty() 

  analysis_set <- analysis(x)
  
  # TODO should this be done outside of rsample, 
  # in workflows or tune?
  if (is.null(split_args$prop)) {
    split_args$prop <- 1 - 1/split_args$v
  }
  # use mc_splits for a random split
  split_args$times <- 1
  split_args$v <- NULL
  split_args$repeats <- NULL
  split_inner <- rlang::inject(
    mc_splits(analysis_set, !!!split_args)
  )
  split_inner <- split_inner$splits[[1]]

  class_inner <- paste0(class(x)[1], "_inner")
  class(split_inner) <- c(class_inner, class(x))
  split_inner
}

#' @rdname inner_split
#' @export
inner_split.group_vfold_split <- function(x, split_args, ...) {
  check_dots_empty() 

  analysis_set <- analysis(x)

  # TODO should this be done outside of rsample, 
  # in workflows or tune?
  if (is.null(split_args$prop)) {
    split_args$prop <- 1 - 1/split_args$v
  }
  
  # use group_mc_splits for a random split
  split_args$times <- 1
  split_args$v <- NULL
  split_args$repeats <- NULL
  split_args$balance <- NULL
  split_inner <- rlang::inject(
    group_mc_splits(analysis_set, !!!split_args)
  )
  split_inner <- split_inner$splits[[1]]

  class_inner <- paste0(class(x)[1], "_inner")
  class(split_inner) <- c(class_inner, class(x))
  split_inner
}

# clustering -------------------------------------------------------------

#' @rdname inner_split
#' @export
inner_split.clustering_split <- function(x, split_args, ...) {
  check_dots_empty() 

  analysis_set <- analysis(x)
  
  # TODO: reduce the number of clusters by 1 in tune?
  split_args$repeats <- 1
  split_inner <- rlang::inject(
    clustering_cv(analysis_set, !!!split_args)
  )
  split_inner <- split_inner$splits[[1]]

  class_inner <- paste0(class(x)[1], "_inner")
  class(split_inner) <- c(class_inner, class(x))
  split_inner
}


# apparent ---------------------------------------------------------------

#' @rdname inner_split
#' @export
inner_split.apparent_split <- function(x, ...) {
  check_dots_empty() 

  analysis_set <- analysis(x)
  
  split_inner <- apparent(analysis_set)
  split_inner <- split_inner$splits[[1]]

  class_inner <- paste0(class(x)[1], "_inner")
  class(split_inner) <- c(class_inner, class(x))
  split_inner
}
