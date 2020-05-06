#' @import vctrs
NULL

# ------------------------------------------------------------------------------
# bootstraps

# Use `vec_restore()` to restore to a fresh tibble, rather than to
# an rset. This ensures that `vec_slice()` and `vec_ptype()` return
# tibbles, and ensures that `vec_ptype2()` methods that deal with `bootstraps`
# are never called.

#' @export
vec_restore.bootstraps <- function(x, to, ...) {
  # Default restore method restores all attributes
  out <- NextMethod()

  # Now strip that back down to a bare tibble
  rset_strip(x)
}


#' @export
vec_ptype2.bootstraps.bootstraps <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.bootstraps.bootstraps")
}
#' @export
vec_ptype2.bootstraps.tbl_df <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.bootstraps.tbl_df")
}
#' @export
vec_ptype2.tbl_df.bootstraps <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.tbl_df.bootstraps")
}
#' @export
vec_ptype2.bootstraps.data.frame <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.bootstraps.data.frame")
}
#' @export
vec_ptype2.data.frame.bootstraps <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.data.frame.bootstraps")
}


#' @export
vec_cast.bootstraps.bootstraps <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.bootstraps.tbl_df <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.tbl_df.bootstraps <- function(x, to, ..., x_arg = "", to_arg = "") {
  tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.bootstraps.data.frame <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.data.frame.bootstraps <- function(x, to, ..., x_arg = "", to_arg = "") {
  df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}


# ------------------------------------------------------------------------------

stop_incompatible_cast_rset <- function(x, to, ..., x_arg, to_arg) {
  details <- "Can't cast to an rset because attributes are likely incompatible."
  stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg, details = details)
}

stop_never_called <- function(fn) {
  rlang::abort(paste0("Internal error: `", fn, "()` should never be called."))
}
