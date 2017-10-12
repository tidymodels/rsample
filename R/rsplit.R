rsplit <- function(data, in_id, out_id) {
  if (!is.data.frame(data) & !is.matrix(data))
    stop("`data` must be a data frame.", call. = FALSE)
  
  if (!is.integer(in_id) | any(in_id < 1))
    stop("`in_id` must be a positive integer vector.", call. = FALSE)
  
  if(!all(is.na(out_id))) {
    if (!is.integer(out_id) | any(out_id < 1))
      stop("`out_id` must be a positive integer vector.", call. = FALSE)
  }
  
  if (length(in_id) == 0)
    stop("At least one row should be selected for the analysis set.", 
         call. = FALSE)
  
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
  out_char <-
    if (all(is.na(x$out_id)))
      paste(length(complement(x)))
  else
    paste(length(x$out_id))
  
  cat("<",
      length(x$in_id), "/",
      out_char, "/",
      nrow(x$data), ">\n",
      sep = "")
}

#' @export
as.integer.rsplit <- 
  function(x, data = c("analysis", "assessment"), ...) {
    data <- match.arg(data)
    if (data == "analysis")
      out <- x$in_id
    else {
      out <- if (all(is.na(x$out_id)))
        complement(x)
      else
        x$out_id
    }
    out
  }


#' Convert an `rsplit` object to a data frame
#' 
#' The analysis or assessment code can be returned as a data
#'   frame (as dictated by the `data` argument) using
#'   `as.data.frame.rsplit`. `analysis` and 
#'   `assessment` are shortcuts. 
#' @param x An `rsplit` object.
#' @param row.names `NULL` or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional A logical: should the column names of the data be checked for legality?
#' @param data Either "analysis" or "assessment" to specify which data are returned. 
#' @param ...	Additional arguments to be passed to or from methods. Not currently used. 
#' @export
as.data.frame.rsplit <-
  function(x,
           row.names = NULL,
           optional = FALSE,
           data = "analysis",
           ...) {
    
  if (!is.null(row.names))
    warning( "`row.names` is kept for consistency with the ",
             "underlying class but non-NULL values will be ", 
             "ignored.", call. = FALSE)
  if (optional)
    warning( "`optional` is kept for consistency with the ",
             "underlying class but TRUE values will be ", 
             "ignored.", call. = FALSE)
  x$data[as.integer(x, data = data, ...), , drop = FALSE]
}

#' @rdname as.data.frame.rsplit  
#' @export
analysis <- function(x, ...)
  as.data.frame(x, data = "analysis", ...)
#' @rdname as.data.frame.rsplit  
#' @export
assessment <- function(x, ...)
  as.data.frame(x, data = "assessment", ...)

#' @export
dim.rsplit <- function(x, ...) {
  c(
    analysis = length(x$in_id),
    assessment = length(complement(x)),
    n = nrow(x$data),
    p = ncol(x$data)
  )
}


#' @importFrom tibble obj_sum
#' @method obj_sum rsplit
#' @export
obj_sum.rsplit <- function(x, ...) {
  out_char <-
    if (all(is.na(x$out_id)))
      paste(length(complement(x)))
  else
    paste(length(x$out_id))
  
  paste0("rsplit [",
         length(x$in_id), "/",
         out_char, "/",
         nrow(x$data), "]")
}



