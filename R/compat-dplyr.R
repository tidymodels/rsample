#' @importFrom dplyr dplyr_reconstruct
#' @export
dplyr_reconstruct.rset <- function(data, template) {
  # Use the default restore, which restores all attributes of
  # `template`, and names and row names of `data`.
  out <- NextMethod()

  # Strip off `rset` specific attributes and return a bare tibble.
  # Importantly this keeps other user defined attributes.
  rset_strip(out)
}

# ------------------------------------------------------------------------------

#' @importFrom dplyr dplyr_col_modify
#' @export
dplyr_col_modify.rset <- function(data, cols) {
  # Expect that the result here is a bare tibble,
  # since `dplyr_col_modify.data.frame()`
  # goes through `dplyr_reconstruct.rset()`.
  out <- NextMethod()

  if (touches_any_rset_columns(cols)) {
    out
  } else {
    rset_reconstruct(out, data)
  }
}

# This is `dplyr_reconstruct.data.frame()`
rset_reconstruct <- function (data, template) {
  attrs <- attributes(template)
  attrs$names <- names(data)
  attrs$row.names <- .row_names_info(data, type = 0L)
  attributes(data) <- attrs
  data
}

touches_any_rset_columns <- function(cols) {
  names <- names(cols)
  problems <- col_matches_splits(names) | col_starts_with_id(names)
  any(problems)
}

col_matches_splits <- function(x) {
  grepl("splits", x, fixed = TRUE)
}

col_starts_with_id <- function(x) {
  grepl("^id", x)
}
