#' Constructor for new rset objects
#' @param splits A list column of `rsplits` or a tibble with a single column
#'  called "splits" with a list column of `rsplits`.
#' @param ids A character vector or a tibble with one or more columns that
#' begin with "id".
#' @param attrib An optional named list of attributes to add to the object.
#' @param subclass A character vector of subclasses to add.
#' @return An `rset` object.
#' @details Once the new `rset` is constructed, an additional attribute called
#' "fingerprint" is added that is a hash of the `rset`. This can be used to
#' make sure other objects have the exact same resamples.
#' @keywords internal
#' @export
new_rset <- function(splits, ids, attrib = NULL, subclass = character()) {
  stopifnot(is.list(splits))
  if (!is_tibble(ids)) {
    ids <- tibble(id = ids)
  } else {
    if (!all(grepl("^id", names(ids)))) {
      cli_abort("The {.code id} tibble column names should start with 'id'.")
    }
  }
  either_type <- function(x) {
    is.character(x) | is.factor(x)
  }
  ch_check <- vapply(ids, either_type, c(logical = TRUE))
  if (!all(ch_check)) {
    cli_abort("{.strong All} ID columns should be character or factor vectors.")
  }

  if (!is_tibble(splits)) {
    splits <- tibble(splits = splits)
  } else {
    if (ncol(splits) > 1 | names(splits)[1] != "splits") {
      cli_abort(
        "The {.var splits} tibble should have a single column named {.code splits}."
      )
    }
  }

  where_rsplits <- vapply(splits[["splits"]], is_rsplit, logical(1))

  if (!all(where_rsplits)) {
    cli_abort("Each element of {.arg splits} must be an {.cls rsplit} object.")
  }

  if (nrow(ids) != nrow(splits)) {
    cli_abort("Split and ID vectors have different lengths.")
  }

  # Create another element to the splits that is a tibble containing
  # an identifier for each id column so that, in isolation, the resample
  # id can be known just based on the `rsplit` object. This can then be
  # accessed using the `labels` method for `rsplits`

  splits$splits <- map2(
    splits$splits,
    split_unnamed(ids, rlang::seq2(1L, nrow(ids))),
    add_id
  )

  res <- bind_cols(splits, ids)

  if (!is.null(attrib)) {
    if (any(names(attrib) == "")) {
      cli_abort("{.arg attrib} should be a fully named list.")
    }
    for (i in names(attrib)) {
      attr(res, i) <- attrib[[i]]
    }
  }

  if (length(subclass) > 0) {
    res <- add_class(res, cls = subclass)
  }

  fingerprint <- map(res$splits, function(x) list(x$in_id, x$out_id))
  fingerprint <- rlang::hash(fingerprint)
  attr(res, "fingerprint") <- fingerprint

  res
}

add_id <- function(split, id) {
  split$id <- id
  split
}

# ------------------------------------------------------------------------------

#' @export
`[.rset` <- function(x, i, j, drop = FALSE, ...) {
  out <- NextMethod()
  rset_reconstruct(out, x)
}

# ------------------------------------------------------------------------------

#' @export
`names<-.rset` <- function(x, value) {
  out <- NextMethod()
  rset_reconstruct(out, x)
}
