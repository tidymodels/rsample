make_splits <- function(ind, data, class = NULL) {
  res <- rsplit(data, ind$analysis,  ind$assessment)
  if(!is.null(class))
    res <- add_class(res, class)
  res
}

merge_lists <- function(a, b) list(analysis = a, assessment = b)

dim_rset <- function(x, ...) {
  dims <- purrr::map(x$splits, dim)
  dims <- do.call("rbind", dims)
  dims <- tibble::as_tibble(dims)
  id_cols <- grep("^id", colnames(x), value = TRUE)
  for(i in seq_along(id_cols))
    dims[id_cols[i]] <- getElement(x, id_cols[i])
  dims
} 
## Note, column subsetting doesn't work with expanded classes


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
