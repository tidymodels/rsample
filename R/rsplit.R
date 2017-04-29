## Add "analysis" and "assessment" classes that are shortcuts for `as.data.frame` method

rsplit <- function(data, in_id, out_id) {
  if (!is.data.frame(data) & !is.matrix(data)) {
    stop("`data` must be a data frame.", call. = FALSE)
  }
  if (!is.integer(in_id) | !is.integer(out_id)) {
    stop("`in_id and out_id` must be integer vectors.", call. = FALSE)
  }
  
  structure(
    list(
      data = data,
      in_id = in_id,
      out_id = out_id
    ),
    class = "rsplit"
  )
}

#' @export
print.rsplit <- function(x, ...) {
  cat("<", length(x$in_id), "/", length(x$out_id), 
      "/", nrow(x$data), ">\n",
      sep = ""
  )
}

#' @export
as.integer.rsplit <- function(x, data = "analysis", ...) {
  if(data == "analysis") x$in_id else x$out_id
}

#' @export
as.data.frame.rsplit <- function(x, data = "analysis", ...) {
  x$data[as.integer(x, data = data), , drop = FALSE]
}

#' @export
dim.rsplit <- function(x, ...) {
  c(analysis = length(x$in_id), 
    assessment = length(x$out_id), 
    n = nrow(x$data), 
    p = ncol(x$data))
}

## Where is this used?
#' #' @importFrom tibble obj_sum
#' #' @method obj_sum rsplit
#' #' @export
#' obj_sum.rsplit <- function(x, ...) {
#'   dims <- dim(x)
#'   paste0(
#'     "rsplit [", 
#'     paste0(big_mark(dim(tt)[-4]), collapse="/"), 
#'     " x ", 
#'     big_mark(dims["p"]), "]")
#' }
