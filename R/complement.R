#' Determine the Assessment Samples
#'
#' Given an `rsplit` object, `complement` will determine which
#'   of the data rows are contained in the assessment set. To save space,
#'   many of the `rset` objects will not contain indices for the
#'   assessment split.
#'
#' @param x An `rsplit` object
#' @param ... Not currently used
#' @return A integer vector.
#' @seealso [populate()]
#' @examples
#' set.seed(28432)
#' fold_rs <- vfold_cv(mtcars)
#' head(fold_rs$splits[[1]]$in_id)
#' fold_rs$splits[[1]]$out_id
#' complement(fold_rs$splits[[1]])
#' @export
complement <- function(x, ...)
  UseMethod("complement")

#' @export
complement.vfold_split <- function(x, ...) {
  if (!all(is.na(x$out_id))) {
    return(x$out_id)
  } else {
    setdiff(1:nrow(x$data), x$in_id)
  }
}
#' @export
complement.mc_split  <- complement.vfold_split
#' @export
complement.val_split <- complement.vfold_split
#' @export
complement.loo_split <- complement.vfold_split
#' @export
complement.spatial_split <- complement.vfold_split
#' @export
complement.group_vfold_split <- complement.vfold_split
#' @export
complement.boot_split <- function(x, ...) {
  if (!all(is.na(x$out_id))) {
    return(x$out_id)
  } else {
    (1:nrow(x$data))[-unique(x$in_id)]
  }
}
#' @export
complement.rof_split <- function(x, ...) {
  get_stored_out_id(x)
}
#' @export
complement.sliding_window_split <- function(x, ...) {
  get_stored_out_id(x)
}
#' @export
complement.sliding_index_split <- function(x, ...) {
  get_stored_out_id(x)
}
#' @export
complement.sliding_period_split <- function(x, ...) {
  get_stored_out_id(x)
}

get_stored_out_id <- function(x) {
  out_id <- x$out_id

  if (length(out_id) == 0L) {
    return(out_id)
  }

  if (all(is.na(out_id))) {
    rlang::abort("Cannot derive the assessment set for this type of resampling.")
  }

  out_id
}

#' @export
complement.apparent_split <- function(x, ...) {
  if (!all(is.na(x$out_id))) {
    return(x$out_id)
  } else {
    1:nrow(x$data)
  }
}


#' Add Assessment Indices
#'
#' Many `rsplit` and `rset` objects do not contain indicators for
#'   the assessment samples. `populate()` can be used to fill the slot
#'   for the appropriate indices.
#' @param x A `rsplit` and `rset` object.
#' @param ... Not currently used
#' @return An object of the same kind with the integer indices.
#' @examples
#' set.seed(28432)
#' fold_rs <- vfold_cv(mtcars)
#'
#' fold_rs$splits[[1]]$out_id
#' complement(fold_rs$splits[[1]])
#'
#' populate(fold_rs$splits[[1]])$out_id
#'
#' fold_rs_all <- populate(fold_rs)
#' fold_rs_all$splits[[1]]$out_id
#' @export
populate <- function (x, ...) UseMethod("populate")

#' @export
populate.rsplit <- function(x, ...) {
  x$out_id <- complement(x, ...)
  x
}

#' @export
populate.rset <- function(x, ...) {
  x$splits <- map(x$splits, populate)
  x
}

## This will remove the assessment indices from an rsplit object
rm_out <- function(x) {
  x$out_id <- NA
  x
}

