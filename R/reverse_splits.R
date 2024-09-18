#' Reverse the analysis and assessment sets
#'
#' This functions "swaps" the analysis and assessment sets of either a single
#' `rsplit` or all `rsplit`s in the `splits` column of an `rset` object.
#'
#' @param x An `rset` or `rsplit` object.
#' @param ... Not currently used.
#'
#' @return An object of the same class as `x`
#'
#' @examples
#' set.seed(123)
#' starting_splits <- vfold_cv(mtcars, v = 3)
#' reverse_splits(starting_splits)
#' reverse_splits(starting_splits$splits[[1]])
#'
#' @rdname reverse_splits
#' @export
reverse_splits <- function(x, ...) {
  UseMethod("reverse_splits")
}

#' @rdname reverse_splits
#' @export
reverse_splits.default <- function(x, ...) {
  cli_abort(
    "{.arg x} must be either an {.cls rsplit} or an {.cls rset} object."
  )
}

#' @rdname reverse_splits
#' @export
reverse_splits.permutations <- function(x, ...) {
  cli_abort(
    "Permutations cannot have their splits reversed."
  )
}

#' @rdname reverse_splits
#' @export
reverse_splits.perm_split <- reverse_splits.permutations

#' @rdname reverse_splits
#' @export
reverse_splits.rsplit <- function(x, ...) {

  rlang::check_dots_empty()

  out_splits <- list(
    analysis = as.integer(x, data = "assessment"),
    assessment = as.integer(x, data = "analysis")
  )
  out_splits <- make_splits(out_splits, x$data)
  class(out_splits) <- class(x)
  out_splits

}

#' @rdname reverse_splits
#' @export
reverse_splits.rset <- function(x, ...) {

  rlang::check_dots_empty()

  x$splits <- purrr::map(x$splits, reverse_splits)

  x
}
