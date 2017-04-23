make_splits <- function(ind, data) {
  rsplit(data, ind$analysis,  ind$assessment)
}

#' @export
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
  dims$id <- x$splits$id
  dims
} 