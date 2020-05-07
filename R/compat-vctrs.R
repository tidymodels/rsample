#' @import vctrs
NULL

# The cast, ptype2, and restore methods are all the same here, but we
# can't use inheritance because vctrs doesn't allow it in the ptype2 and
# cast methods.

# Use `vec_restore()` to restore to a fresh tibble, rather than to
# an rset. This ensures that `vec_slice()` and `vec_ptype()` return
# tibbles, and ensures that `vec_ptype2()` methods that deal with `bootstraps`
# are never called.

# ------------------------------------------------------------------------------
# bootstraps

#' @export
vec_restore.bootstraps <- function(x, to, ...) {
  out <- NextMethod()
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
# vfold_cv

#' @export
vec_restore.vfold_cv <- function(x, to, ...) {
  out <- NextMethod()
  rset_strip(x)
}


#' @export
vec_ptype2.vfold_cv.vfold_cv <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.vfold_cv.vfold_cv")
}
#' @export
vec_ptype2.vfold_cv.tbl_df <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.vfold_cv.tbl_df")
}
#' @export
vec_ptype2.tbl_df.vfold_cv <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.tbl_df.vfold_cv")
}
#' @export
vec_ptype2.vfold_cv.data.frame <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.vfold_cv.data.frame")
}
#' @export
vec_ptype2.data.frame.vfold_cv <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.data.frame.vfold_cv")
}


#' @export
vec_cast.vfold_cv.vfold_cv <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.vfold_cv.tbl_df <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.tbl_df.vfold_cv <- function(x, to, ..., x_arg = "", to_arg = "") {
  tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.vfold_cv.data.frame <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.data.frame.vfold_cv <- function(x, to, ..., x_arg = "", to_arg = "") {
  df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}

# ------------------------------------------------------------------------------
# group_vfold_cv

#' @export
vec_restore.group_vfold_cv <- function(x, to, ...) {
  out <- NextMethod()
  rset_strip(x)
}


#' @export
vec_ptype2.group_vfold_cv.group_vfold_cv <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.group_vfold_cv.group_vfold_cv")
}
#' @export
vec_ptype2.group_vfold_cv.tbl_df <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.group_vfold_cv.tbl_df")
}
#' @export
vec_ptype2.tbl_df.group_vfold_cv <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.tbl_df.group_vfold_cv")
}
#' @export
vec_ptype2.group_vfold_cv.data.frame <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.group_vfold_cv.data.frame")
}
#' @export
vec_ptype2.data.frame.group_vfold_cv <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.data.frame.group_vfold_cv")
}


#' @export
vec_cast.group_vfold_cv.group_vfold_cv <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.group_vfold_cv.tbl_df <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.tbl_df.group_vfold_cv <- function(x, to, ..., x_arg = "", to_arg = "") {
  tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.group_vfold_cv.data.frame <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.data.frame.group_vfold_cv <- function(x, to, ..., x_arg = "", to_arg = "") {
  df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}

# ------------------------------------------------------------------------------
# loo_cv

#' @export
vec_restore.loo_cv <- function(x, to, ...) {
  out <- NextMethod()
  rset_strip(x)
}


#' @export
vec_ptype2.loo_cv.loo_cv <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.loo_cv.loo_cv")
}
#' @export
vec_ptype2.loo_cv.tbl_df <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.loo_cv.tbl_df")
}
#' @export
vec_ptype2.tbl_df.loo_cv <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.tbl_df.loo_cv")
}
#' @export
vec_ptype2.loo_cv.data.frame <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.loo_cv.data.frame")
}
#' @export
vec_ptype2.data.frame.loo_cv <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.data.frame.loo_cv")
}


#' @export
vec_cast.loo_cv.loo_cv <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.loo_cv.tbl_df <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.tbl_df.loo_cv <- function(x, to, ..., x_arg = "", to_arg = "") {
  tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.loo_cv.data.frame <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.data.frame.loo_cv <- function(x, to, ..., x_arg = "", to_arg = "") {
  df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}

# ------------------------------------------------------------------------------
# mc_cv

#' @export
vec_restore.mc_cv <- function(x, to, ...) {
  out <- NextMethod()
  rset_strip(x)
}


#' @export
vec_ptype2.mc_cv.mc_cv <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.mc_cv.mc_cv")
}
#' @export
vec_ptype2.mc_cv.tbl_df <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.mc_cv.tbl_df")
}
#' @export
vec_ptype2.tbl_df.mc_cv <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.tbl_df.mc_cv")
}
#' @export
vec_ptype2.mc_cv.data.frame <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.mc_cv.data.frame")
}
#' @export
vec_ptype2.data.frame.mc_cv <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.data.frame.mc_cv")
}


#' @export
vec_cast.mc_cv.mc_cv <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.mc_cv.tbl_df <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.tbl_df.mc_cv <- function(x, to, ..., x_arg = "", to_arg = "") {
  tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.mc_cv.data.frame <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.data.frame.mc_cv <- function(x, to, ..., x_arg = "", to_arg = "") {
  df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}

# ------------------------------------------------------------------------------
# nested_cv

#' @export
vec_restore.nested_cv <- function(x, to, ...) {
  out <- NextMethod()
  rset_strip(x)
}


#' @export
vec_ptype2.nested_cv.nested_cv <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.nested_cv.nested_cv")
}
#' @export
vec_ptype2.nested_cv.tbl_df <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.nested_cv.tbl_df")
}
#' @export
vec_ptype2.tbl_df.nested_cv <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.tbl_df.nested_cv")
}
#' @export
vec_ptype2.nested_cv.data.frame <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.nested_cv.data.frame")
}
#' @export
vec_ptype2.data.frame.nested_cv <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.data.frame.nested_cv")
}


#' @export
vec_cast.nested_cv.nested_cv <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.nested_cv.tbl_df <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.tbl_df.nested_cv <- function(x, to, ..., x_arg = "", to_arg = "") {
  tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.nested_cv.data.frame <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.data.frame.nested_cv <- function(x, to, ..., x_arg = "", to_arg = "") {
  df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}

# ------------------------------------------------------------------------------
# validation_split

#' @export
vec_restore.validation_split <- function(x, to, ...) {
  out <- NextMethod()
  rset_strip(x)
}


#' @export
vec_ptype2.validation_split.validation_split <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.validation_split.validation_split")
}
#' @export
vec_ptype2.validation_split.tbl_df <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.validation_split.tbl_df")
}
#' @export
vec_ptype2.tbl_df.validation_split <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.tbl_df.validation_split")
}
#' @export
vec_ptype2.validation_split.data.frame <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.validation_split.data.frame")
}
#' @export
vec_ptype2.data.frame.validation_split <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.data.frame.validation_split")
}


#' @export
vec_cast.validation_split.validation_split <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.validation_split.tbl_df <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.tbl_df.validation_split <- function(x, to, ..., x_arg = "", to_arg = "") {
  tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.validation_split.data.frame <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.data.frame.validation_split <- function(x, to, ..., x_arg = "", to_arg = "") {
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
