#' @import vctrs
NULL

# The cast, ptype2, and restore methods are all the same here, but we
# can't use inheritance because vctrs doesn't allow it in the ptype2 and
# cast methods.

# `vec_restore()`
#
# Called at the end of `vec_slice()` and `vec_ptype()` after all slicing has
# been done on the proxy object.
#
# If no changes have been made to the row/column structure of rset specific
# columns, then we can return an rset subclass. Otherwise, the resulting
# object is no longer guaranteed to return a valid rset, and we have to
# fallback to a bare tibble.
#
# It is very important that the result of `vec_ptype()` is a bare tibble.
# This ensures that the `vec_ptype2.<rset-subclass>.<rset-subclass>()` methods
# never get called. `vec_ptype()` is able to return a bare tibble because it
# essentially takes a 0-row slice of the rset, and then calls `vec_restore()`.
# Because the row structure has been modified, we return a bare tibble from
# `vec_restore.<rset-subclass>()`.

# ------------------------------------------------------------------------------
# bootstraps

#' @export
vec_restore.bootstraps <- function(x, to, ...) {
  out <- NextMethod()

  if (rset_identical(out, to)) {
    rset_reconstruct(out, to)
  } else {
    rset_strip(out)
  }
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

  if (rset_identical(out, to)) {
    rset_reconstruct(out, to)
  } else {
    rset_strip(out)
  }
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

  if (rset_identical(out, to)) {
    rset_reconstruct(out, to)
  } else {
    rset_strip(out)
  }
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

  if (rset_identical(out, to)) {
    rset_reconstruct(out, to)
  } else {
    rset_strip(out)
  }
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

  if (rset_identical(out, to)) {
    rset_reconstruct(out, to)
  } else {
    rset_strip(out)
  }
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

  # If underlying rset subclass restore method already dropped the
  # subclass, then we return immediately.
  if (!inherits(out, "nested_cv")) {
    return(out)
  }

  if (rset_identical(out, to)) {
    rset_reconstruct(out, to)
  } else {
    rset_strip(out)
  }
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

  if (rset_identical(out, to)) {
    rset_reconstruct(out, to)
  } else {
    rset_strip(out)
  }
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
  out <- NextMethod()

  if (rset_identical(out, to)) {
    rset_reconstruct(out, to)
  } else {
    rset_strip(out)
  }
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
# apparent

#' @export
vec_restore.apparent <- function(x, to, ...) {
  out <- NextMethod()

  if (rset_identical(out, to)) {
    rset_reconstruct(out, to)
  } else {
    rset_strip(out)
  }
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

# ------------------------------------------------------------------------------

stop_incompatible_cast_rset <- function(x, to, ..., x_arg, to_arg) {
  details <- "Can't cast to an rset because attributes are likely incompatible."
  stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg, details = details)
}

stop_never_called <- function(fn) {
  rlang::abort(paste0("Internal error: `", fn, "()` should never be called."))
}
