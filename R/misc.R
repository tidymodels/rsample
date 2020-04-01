#' Constructor for split objects
#' @param ind A list of integers with names "analysis" and "assessment".
#' @param data A data frame.
#' @param class An optional class to give the object.
#' @keywords internal
#' @export
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
  for (i in seq_along(id_cols)) {
    dims[id_cols[i]] <- getElement(x, id_cols[i])
  }
  dims
}

names0 <- function(num, prefix = "x") {
  if (num < 1) {
    rlang::abort("`num` should be > 0")
  }
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
    if (!is.character(strata) | length(strata) != 1) {
      rlang::abort("`strata` should be a single character value.")
    }
    if (!(strata %in% vars)) {
      rlang::abort(strata, " is not in `data`.")
    }
  }
  invisible(NULL)
}

#' Constructor for new rset objects
#' @param splits A list column of `rsplits` or a tibble with a single column
#'  called "splits" with a list column of `rsplits`.
#' @param ids A character vector or a tibble with one or more columns that
#' begin with "id".
#' @param attrib An optional named list of attributes to add to the object.
#' @param subclass A character vector of subclasses to add.
#' @return An `rset` object.
#' @keywords internal
#' @export
new_rset <-  function(splits, ids, attrib = NULL,
                      subclass = character()) {
  stopifnot(is.list(splits))
  if (!is_tibble(ids)) {
    ids <- tibble(id = ids)
  } else {
    if (!all(grepl("^id", names(ids)))) {
        rlang::abort("The `ids` tibble column names should start with 'id'.")
    }
  }
  either_type <- function(x)
    is.character(x) | is.factor(x)
  ch_check <- vapply(ids, either_type, c(logical = TRUE))
  if (!all(ch_check)) {
    rlang::abort("All ID columns should be character or factor vectors.")
  }

  if (!is_tibble(splits)) {
    splits <- tibble(splits = splits)
  } else {
    if (ncol(splits) > 1 | names(splits)[1] != "splits") {
      rlang::abort(
       "The `splits` tibble should have a single column named `splits`."
      )
    }
  }

  if (nrow(ids) != nrow(splits)) {
    rlang::abort("Split and ID vectors have different lengths.")
  }

  # Create another element to the splits that is a tibble containing
  # an identifier for each id column so that, in isolation, the resample
  # id can be known just based on the `rsplit` object. This can then be
  # accessed using the `labels` method for `rsplits`

  splits$splits <- map2(splits$splits, split(ids, 1:nrow(ids)), add_id)

  res <- bind_cols(splits, ids)

  if (!is.null(attrib)) {
    if (any(names(attrib) == "")) {
      rlang::abort("`attrib` should be a fully named list.")
    }
    for (i in names(attrib)) {
      attr(res, i) <- attrib[[i]]
    }
  }

  if (length(subclass) > 0) {
    res <- add_class(res, cls = subclass, at_end = FALSE)
  }

  res
}


add_id <- function(split, id) {
  split$id <- id
  split
}
