# This file was generated, do not edit by hand
# Please edit inst/generate_vctrs.R instead
# ------------------------------------------------------------------------------
# bootstraps

#' @export
vec_restore.bootstraps <- function(x, to, ...) {
  rset_reconstruct(x, to)
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
# group_bootstraps

#' @export
vec_restore.group_bootstraps <- function(x, to, ...) {
  rset_reconstruct(x, to)
}

#' @export
vec_ptype2.group_bootstraps.group_bootstraps <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.group_bootstraps.group_bootstraps")
}
#' @export
vec_ptype2.group_bootstraps.tbl_df <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.group_bootstraps.tbl_df")
}
#' @export
vec_ptype2.tbl_df.group_bootstraps <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.tbl_df.group_bootstraps")
}
#' @export
vec_ptype2.group_bootstraps.data.frame <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.group_bootstraps.data.frame")
}
#' @export
vec_ptype2.data.frame.group_bootstraps <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.data.frame.group_bootstraps")
}

#' @export
vec_cast.group_bootstraps.group_bootstraps <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.group_bootstraps.tbl_df <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.tbl_df.group_bootstraps <- function(x, to, ..., x_arg = "", to_arg = "") {
  tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.group_bootstraps.data.frame <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.data.frame.group_bootstraps <- function(x, to, ..., x_arg = "", to_arg = "") {
  df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
# ------------------------------------------------------------------------------
# vfold_cv

#' @export
vec_restore.vfold_cv <- function(x, to, ...) {
  rset_reconstruct(x, to)
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
  rset_reconstruct(x, to)
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
  rset_reconstruct(x, to)
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
  rset_reconstruct(x, to)
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
# group_mc_cv

#' @export
vec_restore.group_mc_cv <- function(x, to, ...) {
  rset_reconstruct(x, to)
}

#' @export
vec_ptype2.group_mc_cv.group_mc_cv <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.group_mc_cv.group_mc_cv")
}
#' @export
vec_ptype2.group_mc_cv.tbl_df <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.group_mc_cv.tbl_df")
}
#' @export
vec_ptype2.tbl_df.group_mc_cv <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.tbl_df.group_mc_cv")
}
#' @export
vec_ptype2.group_mc_cv.data.frame <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.group_mc_cv.data.frame")
}
#' @export
vec_ptype2.data.frame.group_mc_cv <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.data.frame.group_mc_cv")
}

#' @export
vec_cast.group_mc_cv.group_mc_cv <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.group_mc_cv.tbl_df <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.tbl_df.group_mc_cv <- function(x, to, ..., x_arg = "", to_arg = "") {
  tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.group_mc_cv.data.frame <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.data.frame.group_mc_cv <- function(x, to, ..., x_arg = "", to_arg = "") {
  df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
# ------------------------------------------------------------------------------
# nested_cv

#' @export
vec_restore.nested_cv <- function(x, to, ...) {
  rset_reconstruct(x, to)
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
  rset_reconstruct(x, to)
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
# rolling_origin

#' @export
vec_restore.rolling_origin <- function(x, to, ...) {
  rset_reconstruct(x, to)
}

#' @export
vec_ptype2.rolling_origin.rolling_origin <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.rolling_origin.rolling_origin")
}
#' @export
vec_ptype2.rolling_origin.tbl_df <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.rolling_origin.tbl_df")
}
#' @export
vec_ptype2.tbl_df.rolling_origin <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.tbl_df.rolling_origin")
}
#' @export
vec_ptype2.rolling_origin.data.frame <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.rolling_origin.data.frame")
}
#' @export
vec_ptype2.data.frame.rolling_origin <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.data.frame.rolling_origin")
}

#' @export
vec_cast.rolling_origin.rolling_origin <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.rolling_origin.tbl_df <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.tbl_df.rolling_origin <- function(x, to, ..., x_arg = "", to_arg = "") {
  tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.rolling_origin.data.frame <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.data.frame.rolling_origin <- function(x, to, ..., x_arg = "", to_arg = "") {
  df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
# ------------------------------------------------------------------------------
# sliding_window

#' @export
vec_restore.sliding_window <- function(x, to, ...) {
  rset_reconstruct(x, to)
}

#' @export
vec_ptype2.sliding_window.sliding_window <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.sliding_window.sliding_window")
}
#' @export
vec_ptype2.sliding_window.tbl_df <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.sliding_window.tbl_df")
}
#' @export
vec_ptype2.tbl_df.sliding_window <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.tbl_df.sliding_window")
}
#' @export
vec_ptype2.sliding_window.data.frame <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.sliding_window.data.frame")
}
#' @export
vec_ptype2.data.frame.sliding_window <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.data.frame.sliding_window")
}

#' @export
vec_cast.sliding_window.sliding_window <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.sliding_window.tbl_df <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.tbl_df.sliding_window <- function(x, to, ..., x_arg = "", to_arg = "") {
  tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.sliding_window.data.frame <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.data.frame.sliding_window <- function(x, to, ..., x_arg = "", to_arg = "") {
  df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
# ------------------------------------------------------------------------------
# sliding_index

#' @export
vec_restore.sliding_index <- function(x, to, ...) {
  rset_reconstruct(x, to)
}

#' @export
vec_ptype2.sliding_index.sliding_index <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.sliding_index.sliding_index")
}
#' @export
vec_ptype2.sliding_index.tbl_df <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.sliding_index.tbl_df")
}
#' @export
vec_ptype2.tbl_df.sliding_index <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.tbl_df.sliding_index")
}
#' @export
vec_ptype2.sliding_index.data.frame <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.sliding_index.data.frame")
}
#' @export
vec_ptype2.data.frame.sliding_index <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.data.frame.sliding_index")
}

#' @export
vec_cast.sliding_index.sliding_index <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.sliding_index.tbl_df <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.tbl_df.sliding_index <- function(x, to, ..., x_arg = "", to_arg = "") {
  tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.sliding_index.data.frame <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.data.frame.sliding_index <- function(x, to, ..., x_arg = "", to_arg = "") {
  df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
# ------------------------------------------------------------------------------
# sliding_period

#' @export
vec_restore.sliding_period <- function(x, to, ...) {
  rset_reconstruct(x, to)
}

#' @export
vec_ptype2.sliding_period.sliding_period <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.sliding_period.sliding_period")
}
#' @export
vec_ptype2.sliding_period.tbl_df <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.sliding_period.tbl_df")
}
#' @export
vec_ptype2.tbl_df.sliding_period <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.tbl_df.sliding_period")
}
#' @export
vec_ptype2.sliding_period.data.frame <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.sliding_period.data.frame")
}
#' @export
vec_ptype2.data.frame.sliding_period <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.data.frame.sliding_period")
}

#' @export
vec_cast.sliding_period.sliding_period <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.sliding_period.tbl_df <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.tbl_df.sliding_period <- function(x, to, ..., x_arg = "", to_arg = "") {
  tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.sliding_period.data.frame <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.data.frame.sliding_period <- function(x, to, ..., x_arg = "", to_arg = "") {
  df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
# ------------------------------------------------------------------------------
# manual_rset

#' @export
vec_restore.manual_rset <- function(x, to, ...) {
  rset_reconstruct(x, to)
}

#' @export
vec_ptype2.manual_rset.manual_rset <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.manual_rset.manual_rset")
}
#' @export
vec_ptype2.manual_rset.tbl_df <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.manual_rset.tbl_df")
}
#' @export
vec_ptype2.tbl_df.manual_rset <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.tbl_df.manual_rset")
}
#' @export
vec_ptype2.manual_rset.data.frame <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.manual_rset.data.frame")
}
#' @export
vec_ptype2.data.frame.manual_rset <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.data.frame.manual_rset")
}

#' @export
vec_cast.manual_rset.manual_rset <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.manual_rset.tbl_df <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.tbl_df.manual_rset <- function(x, to, ..., x_arg = "", to_arg = "") {
  tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.manual_rset.data.frame <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.data.frame.manual_rset <- function(x, to, ..., x_arg = "", to_arg = "") {
  df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
# ------------------------------------------------------------------------------
# apparent

#' @export
vec_restore.apparent <- function(x, to, ...) {
  rset_reconstruct(x, to)
}

#' @export
vec_ptype2.apparent.apparent <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.apparent.apparent")
}
#' @export
vec_ptype2.apparent.tbl_df <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.apparent.tbl_df")
}
#' @export
vec_ptype2.tbl_df.apparent <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.tbl_df.apparent")
}
#' @export
vec_ptype2.apparent.data.frame <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.apparent.data.frame")
}
#' @export
vec_ptype2.data.frame.apparent <- function(x, y, ..., x_arg = "", y_arg = "") {
  stop_never_called("vec_ptype2.data.frame.apparent")
}

#' @export
vec_cast.apparent.apparent <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.apparent.tbl_df <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.tbl_df.apparent <- function(x, to, ..., x_arg = "", to_arg = "") {
  tib_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.apparent.data.frame <- function(x, to, ..., x_arg = "", to_arg = "") {
  stop_incompatible_cast_rset(x, to, x_arg = x_arg, to_arg = to_arg)
}
#' @export
vec_cast.data.frame.apparent <- function(x, to, ..., x_arg = "", to_arg = "") {
  df_cast(x, to, ..., x_arg = x_arg, to_arg = to_arg)
}
