make_splits <- function(ind, data, class = NULL) {
  res <- rsplit(data, ind$analysis,  ind$assessment)
  if(!is.null(class))
    res <- add_class(res, class)
  res
}

get_rsplit <- function(object, id = object$splits$id[1]) {
  if(length(id) > 1 || !is.character(id))
    stop("`id` should be a single character value.")
  ind <- which(id == object$splits$id)
  if(length(ind) == 0)
    stop("That `id` value was not found", call. = FALSE)
  getElement(object$splits, "splits")[ind][[1]]
}

merge_lists <- function(a, b) list(analysis = a, assessment = b)

#' @export
dim.rset <- function(x, ...) {
  dims <- purrr::map(x$splits$splits, dim)
  # dims <- dplyr::bind_rows(dims) # seg faults
  dims <- do.call("rbind", dims)
  dims <- tibble::as_tibble(dims)
  dims <- cbind(dims, x$splits[, grepl("^id", colnames(x$splits))])
  dims
} 


#' Extract the resampling information
#' 
#' The convenience function can be used to extract the data structure that contains the resampling splits or a specific element of that structure. 
#' @param x An `rset` object
#' @param  .elem A character value or \code{NULL}. If \code{NULL}, then the entire tibble is returned. Values of the argument can be any column in the tibble.  
#' @return A tibble, vector, or list depending on the value of \code{.elem}. 
#' @examples  
#' set.seed(36522)
#' bt <- bootstraps(mtcars, times = 3)
#' splits(bt)
#' splits(bt, "id")
#' splits(bt, "splits")
#' @export
splits <- function(x, .elem = NULL) {
  if(!is.null(.elem)) {
    if(!.elem %in% names(x$splits))
      stop("`", .elem, "` is not in the `splits` tibble", call. = FALSE)
    res <- x$splits[[.elem]]
  } else res <- x$splits
  res
}

names0 <- function (num, prefix = "x") {
  if (num < 1) 
    stop("`num` should be > 0", call. = FALSE)
  ind <- format(1:num)
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
}


add_class <- function(x, cls) {
  class(x) <- c(class(x), cls)
  x
}


## add an assignment operator like `rownames(x)<- ...`  that can be multivariate and add new objects via cbind'ing
## make a class? 
