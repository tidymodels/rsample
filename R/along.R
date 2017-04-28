#' Compute Across Resamples
#'
#' @aliases along along.rset
#' @param x An object. 
#' @param ... Arguments to pass to \code{\link[purrr]{map}} such as a function. 
#' @export
#' @return  A list or vector of results the same length as \code{nrow(x$splits)}. 
along <- function (x, ...) UseMethod("along")

#' @rdname along
#' @param .elem A characterstring for which column of the \code{rset} object to compute over
#' @param .unlist A logical indicating if the results should be unlisted prior to returning the values.
#' @importFrom purrr map 
#' @export
along.rset <- function(x, ..., .elem  = "splits", .unlist = FALSE) {
  res <- map(x$splits[[.elem]], ...)
  ## set IDs in names?
  if(.unlist) 
    res <- unlist(res)
  res
}




