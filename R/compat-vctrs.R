#' @import vctrs
NULL

# ------------------------------------------------------------------------------
# constructors

new_bootstraps <- function(x, times, apparent, strata) {
  new_rset2(x, times = times, apparent = apparent, strata = strata, class = "bootstraps")
}

new_rset2 <- function(x, ..., class = character()) {
  x <- unclass(x)
  names <- names(x)

  col_splits <- col_matches_splits(names)
  if (length(col_splits) != 1L) {
    rlang::abort("`x` must contain a single column named `splits`.")
  }

  splits <- x[[col_splits]]
  if (!is.list(splits)) {
    rlang::abort("`x$splits` must be a list.")
  }

  size <- length(splits)

  cols_id <- col_starts_with_id(names)
  if (length(cols_id) == 0L) {
    rlang::abort("`x` must contain at least 1 column that starts with `id`.")
  }

  x_id <- x[cols_id]
  x_id_ok <- map_lgl(x_id, ~is.character(.x) || is.factor(x))
  if (!all(x_id_ok)) {
    rlang::abort("All `x` columns that start with `id` must be characters or factors.")
  }

  tibble::new_tibble(x, nrow = size, ..., class = c(class, "rset"))
}

is_rset <- function(x) {
  inherits(x, "rset")
}

# ------------------------------------------------------------------------------
# bootstraps

#' @export
vec_ptype2.bootstraps.bootstraps <- function(x, y, ..., x_arg = "", y_arg = "") {
  bootstraps_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}
#' @export
vec_ptype2.bootstraps.tbl_df <- function(x, y, ..., x_arg = "", y_arg = "") {
  bootstraps_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}
#' @export
vec_ptype2.tbl_df.bootstraps <- function(x, y, ..., x_arg = "", y_arg = "") {
  bootstraps_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}
#' @export
vec_ptype2.bootstraps.data.frame <- function(x, y, ..., x_arg = "", y_arg = "") {
  bootstraps_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}
#' @export
vec_ptype2.data.frame.bootstraps <- function(x, y, ..., x_arg = "", y_arg = "") {
  bootstraps_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg)
}

bootstraps_ptype2 <- function(x, y, ..., x_arg, y_arg) {
  rset_ptype2(x, y, ..., x_arg = x_arg, y_arg = y_arg, ctor = new_bootstraps)
}


#' @export
vec_cast.bootstraps.bootstraps <- function(x, to, ..., x_arg = "", to_arg = "") {
  bootstraps_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.bootstraps.tbl_df <- function(x, to, ..., x_arg = "", to_arg = "") {
  bootstraps_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.tbl_df.bootstraps <- function(x, to, ..., x_arg = "", to_arg = "") {
  tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.bootstraps.data.frame <- function(x, to, ..., x_arg = "", to_arg = "") {
  bootstraps_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.data.frame.bootstraps <- function(x, to, ..., x_arg = "", to_arg = "") {
  df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}

bootstraps_cast <- function(x, to, ..., x_arg, to_arg) {
  rset_cast(x, to, x_arg = x_arg, to_arg = to_arg, ctor = new_bootstraps)
}

# ------------------------------------------------------------------------------

rset_ptype2 <- function(x, y, ..., x_arg, y_arg, ctor) {
  out <- df_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)

  if (is_rset(x) && is_rset(y)) {
    validate_identical_rset_attributes_ptype2(x, y, x_arg, y_arg)
  }

  if (is_rset(x)) {
    attrs <- peek_rset_attributes(x)
  } else if (is_rset(y)) {
    attrs <- peek_rset_attributes(y)
  } else {
    rlang::abort("Internal error: `x` or `y` must be an rset.")
  }

  rlang::exec(ctor, x = out, !!!attrs)
}

rset_cast <- function(x, to, ..., x_arg, to_arg, ctor) {
  out <- df_cast(x, to, x_arg = x_arg, to_arg = to_arg)
  attrs <- peek_rset_attributes(to)
  exec(ctor, x = out, !!!attrs)
}

# ------------------------------------------------------------------------------
# Attributes

rsample_rset_attributes <- list(
  bootstraps = c("times", "apparent", "strata")
)

peek_rset_attributes <- function(x) {
  cls <- class(x)[[1]]
  attribute_names <- rsample_rset_attributes[[cls]]

  if (is.null(attribute_names)) {
    rlang::abort("Unknown class to pull rset attributes for.")
  }

  out <- attributes(x)
  out <- vec_slice(out, attribute_names)

  out
}

validate_identical_rset_attributes_ptype2 <- function(x, y, x_arg, y_arg) {
  x_attributes <- peek_rset_attributes(x)
  y_attributes <- peek_rset_attributes(y)

  attribute_names <- names(x_attributes)

  for (attribute_name in attribute_names) {
    x_attribute <- x_attributes[[attribute_name]]
    y_attribute <- y_attributes[[attribute_name]]

    if (identical(x_attribute, y_attribute)) {
      next()
    }

    details <- paste0(
      "`x` and `y` must have the same rset attributes. `",
      attribute_name,
      "` is not identical."
    )

    stop_incompatible_type(
      x,
      y,
      x_arg = x_arg,
      y_arg = y_arg,
      details = details
    )
  }

  invisible()
}

validate_identical_rset_attributes_cast <- function(x, to, x_arg, to_arg) {
  x_attributes <- peek_rset_attributes(x)
  to_attributes <- peek_rset_attributes(to)

  attribute_names <- names(x_attributes)

  for (attribute_name in attribute_names) {
    x_attribute <- x_attributes[[attribute_name]]
    to_attribute <- to_attributes[[attribute_name]]

    if (identical(x_attribute, to_attribute)) {
      next()
    }

    details <- paste0(
      "`x` and `to` must have the same rset attributes. `",
      attribute_name,
      "` is not identical."
    )

    stop_incompatible_cast(
      x,
      to,
      x_arg = x_arg,
      to_arg = to_arg,
      details = details
    )
  }

  invisible()
}

# ------------------------------------------------------------------------------

col_matches_splits <- function(x) {
  grep("splits", x, fixed = TRUE)
}

col_starts_with_id <- function(x) {
  grep("^id", x)
}
