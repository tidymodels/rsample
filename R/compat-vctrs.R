#' @import vctrs
NULL

# ------------------------------------------------------------------------------

stop_incompatible_cast_rset <- function(x, to, ..., x_arg, to_arg) {
  details <- "Can't cast to an rset because attributes are likely incompatible."
  stop_incompatible_cast(x, to, x_arg = x_arg, to_arg = to_arg, details = details)
}

stop_never_called <- function(fn) {
  rlang::abort(paste0("Internal error: `", fn, "()` should never be called."))
}

#' @export
vec_ptype_abbr.initial_split <- function(x, ...) {
  "i_splt"
}

#' @export
vec_ptype_abbr.initial_time_split <- function(x, ...) {
  "i_t_splt"
}

#' @export
vec_ptype_abbr.group_initial_split <- function(x, ...) {
  "g_i_splt"
}

#' @export
vec_ptype_abbr.initial_validation_split <- function(x, ...) {
  "i_v_splt"
}

#' @export
vec_ptype_abbr.group_initial_validation_split <- function(x, ...) {
  "giv_splt"
}

#' @export
vec_ptype_abbr.validation_set <- function(x, ...) {
  "v_set"
}

#' @export
vec_ptype_abbr.validation_split <- function(x, ...) {
  "v_splt"
}

#' @export
vec_ptype_abbr.validation_time_split <- function(x, ...) {
  "v_t_splt"
}

#' @export
vec_ptype_abbr.group_validation_split <- function(x, ...) {
  "g_v_splt"
}

#' @export
vec_ptype_abbr.bootstraps <- function(x, ...) {
  "boot"
}

#' @export
vec_ptype_abbr.group_bootstraps <- function(x, ...) {
  "g_boot"
}

#' @export
vec_ptype_abbr.vfold_cv <- function(x, ...) {
  "vfold"
}

#' @export
vec_ptype_abbr.group_vfold_cv <- function(x, ...) {
  "g_vfold"
}

#' @export
vec_ptype_abbr.group_mc_cv <- function(x, ...) {
  "g_mc_cv"
}

#' @export
vec_ptype_abbr.clustering_cv <- function(x, ...) {
  "clt_fold"
}

#' @export
vec_ptype_abbr.rolling_origin <- function(x, ...) {
  "roll_ori"
}

#' @export
vec_ptype_abbr.sliding_window <- function(x, ...) {
  "slide_w"
}

#' @export
vec_ptype_abbr.sliding_index <- function(x, ...) {
  "slide_i"
}

#' @export
vec_ptype_abbr.sliding_period <- function(x, ...) {
  "slide_p"
}

#' @export
vec_ptype_abbr.nested_cv <- function(x, ...) {
  "nest_cv"
}

#' @export
vec_ptype_abbr.apparent <- function(x, ...) {
  "appr"
}

#' @export
vec_ptype_abbr.manual_rset <- function(x, ...) {
  "m_rset"
}

#' @export
vec_ptype_abbr.permutations <- function(x, ...) {
  "perm"
}
