#' Manual resampling
#'
#' `manual_rset()` is used for constructing the most minimal rset possible. It
#' can be useful when you have custom rsplit objects built from
#' [make_splits()], or when you want to create a new rset from splits
#' contained within an existing rset.
#'
#' @param splits A list of `"rsplit"` objects. It is easiest to create these
#'   using [make_splits()].
#'
#' @param ids A character vector of ids. The length of `ids` must be the same
#'   as the length of `splits`.
#'
#' @export
#' @examples
#' df <- data.frame(x = c(1, 2, 3, 4, 5, 6))
#'
#' # Create an rset from custom indices
#' indices <- list(
#'   list(analysis = c(1L, 2L), assessment = 3L),
#'   list(analysis = c(4L, 5L), assessment = 6L)
#' )
#'
#' splits <- lapply(indices, make_splits, data = df)
#'
#' manual_rset(splits, c("Split 1", "Split 2"))
#'
#' # You can also use this to create an rset from a subset of an
#' # existing rset
#' resamples <- vfold_cv(mtcars)
#' best_split <- resamples[5,]
#' manual_rset(best_split$splits, best_split$id)
manual_rset <- function(splits, ids) {
  new_manual_rset(splits, ids)
}

new_manual_rset <- function(splits, ids) {
  new_rset(splits, ids, subclass = c("manual_rset", "rset"))
}

#' @export
print.manual_rset <- function(x, ...) {
  cat("#", pretty(x), "\n")
  class(x) <- class(x)[!(class(x) %in% c("manual_rset", "rset"))]
  print(x, ...)
}
