#' @importFrom vctrs vec_ptype2
#' @importFrom vctrs vec_ptype2.data.frame
NULL

#' @importFrom vctrs vec_cast
#' @importFrom vctrs vec_cast.data.frame
NULL

#' vctrs compatibility functions
#'
#' These functions are the extensions that allow rset objects to
#' work with vctrs.
#'
#' @param x,y Objects.
#' @param to Type to cast to.
#' @param ... Used to pass along error message information.
#' @inheritParams vec_ptype2
#'
#' @return
#'
#' See the corresponding vctrs function for the exact return value.
#'
#' @name vctrs-compat
#'
NULL

# Coercion ----------------------------------------------------------------

#' @export vec_ptype2.rset
#' @method vec_ptype2 rset
#' @rdname vctrs-compat
#' @export
vec_ptype2.rset <- function(x, y, ...) {
  UseMethod("vec_ptype2.rset", y)
}

# Common type of rset + data frame is the union of the columns as an rset

vec_ptype2_rset_data_frame <- function(x, y) {
  ptype <- as_tibble(vctrs::vec_ptype2(as.data.frame(x), y))
  extra_classes <- setdiff(class(x), base_classes)
  maybe_rset(ptype, extras = extra_classes, att = attributes(x))
}

#' @method vec_ptype2.rset data.frame
#' @export
vec_ptype2.rset.data.frame <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_ptype2_rset_data_frame(x, y)
}

#' @method vec_ptype2.data.frame rset
#' @export
vec_ptype2.data.frame.rset <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_ptype2_rset_data_frame(y, x)
}

# Conditionally registered in onLoad
vec_ptype2.rset.tbl_df <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_ptype2_rset_data_frame(x, y)
}

# Conditionally registered in onLoad
vec_ptype2.tbl_df.rset <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vec_ptype2_rset_data_frame(y, x)
}

# Common type of rset + rset is the union of the columns as an rset,
# but ONLY if the rset subtypes are the same (bootstrap / bootstrap)
# and ONLY if the rset specific attributes are identical

#' @method vec_ptype2.rset rset
#' @export
vec_ptype2.rset.rset <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  x_rset_type <- class(x)[[1]]
  y_rset_type <- class(y)[[1]]

  if (x_rset_type != y_rset_type) {
    vctrs::stop_incompatible_type(x, y, x_arg = x_arg, y_arg = y_arg)
  }

  validate_identical_rset_attributes(x, y)

  ptype <- vctrs::vec_ptype2(
    as.data.frame(x),
    as.data.frame(y),
    x_arg = x_arg,
    y_arg = y_arg
  )

  ptype <- as_tibble(ptype)

  x_extra_classes <- setdiff(class(x), base_classes)

  maybe_rset(ptype, extras = x_extra_classes, att = attributes(x))
}

#' @method vec_ptype2.rset default
#' @export
vec_ptype2.rset.default <- function(x, y, ..., x_arg = "x", y_arg = "y") {
  vctrs::vec_default_ptype2(x, y, x_arg = x_arg, y_arg = y_arg)
}

# Cast --------------------------------------------------------------------

#' @export vec_cast.rset
#' @method vec_cast rset
#' @rdname vctrs-compat
#' @export
vec_cast.rset <- function(x, to, ...) {
  UseMethod("vec_cast.rset")
}

vec_cast_data_frame_to_rset <- function(x, to, x_arg, to_arg) {
  out <- vctrs::vec_cast(x, as.data.frame(to), x_arg = x_arg, to_arg = to_arg)
  out <- as_tibble(out)

  extra_classes <- setdiff(class(to), base_classes)
  maybe_rset(out, extras = extra_classes, att = attributes(to))
}

#' @method vec_cast.rset data.frame
#' @export
vec_cast.rset.data.frame <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  vec_cast_data_frame_to_rset(x, to, x_arg, to_arg)
}

# Conditionally registered in onLoad
vec_cast.rset.tbl_df <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  vec_cast_data_frame_to_rset(x, to, x_arg, to_arg)
}

# Cast from `x` to `to` for two rsets is all the columns from `x` plus any
# extra columns in `to`,
# but ONLY if the rset subtypes are the same (bootstrap / bootstrap)
# and ONLY if the rset specific attributes are identical

#' @export
#' @method vec_cast.rset rset
vec_cast.rset.rset <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  x_rset_type <- class(x)[[1]]
  to_rset_type <- class(to)[[1]]

  if (x_rset_type != to_rset_type) {
    vctrs::stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg)
  }

  validate_identical_rset_attributes(x, to)

  out <- vctrs::vec_cast(
    as.data.frame(x),
    as.data.frame(to),
    x_arg = x_arg,
    to_arg = to_arg
  )

  out <- as_tibble(out)

  x_extra_classes <- setdiff(class(x), base_classes)

  maybe_rset(out, extras = x_extra_classes, att = attributes(x))
}

#' @export
#' @method vec_cast.rset default
vec_cast.rset.default <- function(x, to, ..., x_arg = "x", to_arg = "to") {
  vctrs::vec_default_cast(x, to, x_arg = x_arg, to_arg = to_arg)
}

# ------------------------------------------------------------------------------

validate_identical_rset_attributes <- function(x, y) {
  x_attrs <- attributes(x)
  x_attrs <- x_attrs[names(x_attrs) %in% rsample_att]
  x_attrs <- x_attrs[sort(names(x_attrs))]

  y_attrs <- attributes(y)
  y_attrs <- y_attrs[names(y_attrs) %in% rsample_att]
  y_attrs <- y_attrs[sort(names(y_attrs))]

  if (!identical(names(x_attrs), names(y_attrs))) {
    rlang::abort("`x` and `y` must have the same rset attribute names.")
  }

  common_attribute_names <- names(x_attrs)

  for (common_attribute_name in common_attribute_names) {
    x_attr <- x_attrs[common_attribute_name]
    y_attr <- y_attrs[common_attribute_name]

    if (!identical(x_attr, y_attr)) {
      msg <- paste0(
        "`x` and `y` must have the same rset attributes. `",
        common_attribute_name,
        "` is not identical."
      )
      rlang::abort(msg)
    }
  }

  invisible(x)
}
