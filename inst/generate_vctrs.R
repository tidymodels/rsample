# This file generates R/compat-vctrs-generated.R
# and is not a part of the rsample package
#
# It requires R > 4.1 to run

devtools::load_all()

template <- r"(# ------------------------------------------------------------------------------
# CHANGEME

#' @export
vec_restore.CHANGEME <- function(x, to, ...) {
  rset_reconstruct(x, to)
}

#' @export
vec_ptype2.CHANGEME.CHANGEME <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.CHANGEME.CHANGEME")
}
#' @export
vec_ptype2.CHANGEME.tbl_df <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.CHANGEME.tbl_df")
}
#' @export
vec_ptype2.tbl_df.CHANGEME <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.tbl_df.CHANGEME")
}
#' @export
vec_ptype2.CHANGEME.data.frame <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.CHANGEME.data.frame")
}
#' @export
vec_ptype2.data.frame.CHANGEME <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.data.frame.CHANGEME")
}

#' @export
vec_cast.CHANGEME.CHANGEME <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.CHANGEME.tbl_df <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.tbl_df.CHANGEME <- function(x, to, ..., x_arg = "", to_arg = "") {
  tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.CHANGEME.data.frame <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.data.frame.CHANGEME <- function(x, to, ..., x_arg = "", to_arg = "") {
  df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
})"

generated_template <- vapply(
  names(rset_subclasses),
  function(x) {
    # glue has weird edge cases when trying to interpolate into R code
    gsub("CHANGEME", x, template)
  },
  character(1)
)

writeLines(
  c(
    "# This file was generated, do not edit by hand",
    "# Please edit inst/generate_vctrs.R instead",
    generated_template
  ),
  "R/compat-vctrs-generated.R"
)
