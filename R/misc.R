make_splits <- function(ind, data, class = NULL) {
  res <- rsplit(data, ind$analysis,  ind$assessment)
  if (!is.null(class))
    res <- add_class(res, class)
  res
}

merge_lists <- function(a, b) list(analysis = a, assessment = b)

dim_rset <- function(x, ...) {
  dims <- purrr::map(x$splits, dim)
  dims <- do.call("rbind", dims)
  dims <- tibble::as_tibble(dims)
  id_cols <- grep("^id", colnames(x), value = TRUE)
  for (i in seq_along(id_cols))
    dims[id_cols[i]] <- getElement(x, id_cols[i])
  dims
} 

names0 <- function (num, prefix = "x") {
  if (num < 1) 
    stop("`num` should be > 0", call. = FALSE)
  ind <- format(1:num)
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
}

add_class <- function(x, cls, at_end = TRUE) {
  class(x) <- if (at_end)
    c(class(x), cls)
  else
    c(cls, class(x))
  x
}

strata_check <- function(strata, vars) {
  if (!is.null(strata)) {
    if (!is.character(strata) | length(strata) != 1)
      stop("`strata` should be a single character value", call. = FALSE)
    if (!(strata %in% vars))
      stop(strata, " is not in `data`")
  }  
  invisible(NULL)
}

#' @importFrom tibble is_tibble as_tibble tibble
#' @importFrom dplyr bind_cols
# `splits`` should be either a list or a tibble with a single column
#  called "splits"
# `ids`` should be either a character vector or a tibble with 
#  one or more columns that begin with "id" 
new_rset <-  function(splits, ids, attrib = NULL, 
                      subclass = character()) {
  stopifnot(is.list(splits))
  if (!is_tibble(ids)) {
    ids <- tibble(id = ids)
  } else {
    if (!all(grepl("^id", names(ids))))
      stop("The `ids` tibble column names should start with 'id'",
           call. = FALSE)
  }
  either_type <- function(x)
    is.character(x) | is.factor(x)
  ch_check <- vapply(ids, either_type, c(logical = TRUE))
  if(!all(ch_check))
    stop("All ID columns should be character or factor ",
         "vectors.", call. = FALSE)
  
  if (!is_tibble(splits)) {
    splits <- tibble(splits = splits)
  } else {
    if(ncol(splits) > 1 | names(splits)[1] != "splits")
      stop("The `splits` tibble should have a single column ", 
           "named `splits`.", call. = FALSE)
  }

  if (nrow(ids) != nrow(splits))
    stop("Split and ID vectors have different lengths.",
         call. = FALSE)  
  res <- bind_cols(splits, ids)
  
  if (!is.null(attrib)) {
    if (any(names(attrib) == ""))
      stop("`attrib` should be a fully named list.",
           call. = FALSE)
    for (i in names(attrib))
      attr(res, i) <- attrib[[i]]
  }
  
  if (length(subclass) > 0)
    res <- add_class(res, cls = subclass, at_end = FALSE)
  
  res
}
