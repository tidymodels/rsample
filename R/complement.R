#' Determine the Assessment Samples
#'
#' This method and function help find which data belong in the analysis and
#' assessment sets.
#'
#' Given an `rsplit` object, `complement()` will determine which
#'   of the data rows are contained in the assessment set. To save space,
#'   many of the `rsplit` objects will not contain indices for the
#'   assessment split.
#'
#' @param x An `rsplit` object.
#' @param ... Not currently used.
#' @return A integer vector.
#' @seealso [populate()]
#' @examples
#' set.seed(28432)
#' fold_rs <- vfold_cv(mtcars)
#' head(fold_rs$splits[[1]]$in_id)
#' fold_rs$splits[[1]]$out_id
#' complement(fold_rs$splits[[1]])
#' @export
complement <- function(x, ...) {
  UseMethod("complement")
}

#' @export
#' @rdname complement
complement.rsplit <- function(x, ...) {
  check_dots_empty()
  if (!is_missing_out_id(x)) {
    return(x$out_id)
  } else {
    seq_len(nrow(x$data))[-unique(x$in_id)]
  }
}
#' @export
#' @rdname complement
complement.rof_split <- function(x, ...) {
  check_dots_empty()
  get_stored_out_id(x)
}
#' @export
#' @rdname complement
complement.sliding_window_split <- function(x, ...) {
  check_dots_empty()
  get_stored_out_id(x)
}
#' @export
#' @rdname complement
complement.sliding_index_split <- function(x, ...) {
  check_dots_empty()
  get_stored_out_id(x)
}
#' @export
#' @rdname complement
complement.sliding_period_split <- function(x, ...) {
  check_dots_empty()
  get_stored_out_id(x)
}

get_stored_out_id <- function(x) {
  out_id <- x$out_id

  if (length(out_id) == 0L) {
    return(out_id)
  }

  if (all(is.na(out_id))) {
    x_cls <- class(x)
    cli_abort("Cannot derive the assessment set for this type of resampling with class{?es}: {.cls {x_cls}}.")
  }

  out_id
}

#' @export
#' @rdname complement
complement.apparent_split <- function(x, ...) {
  check_dots_empty()
  if (!is_missing_out_id(x)) {
    return(x$out_id)
  } else {
    seq_len(nrow(x$data))
  }
}

#' @export
complement.default <- function(x, ...) {
  x_cls <- class(x)
  cli_abort(
     "No {.fn complement} method for objects of class{?es}: {.cls {x_cls}}"
  )
}

# Get the indices of the analysis set from the assessment set
default_complement <- function(ind, n) {
  list(
    analysis = setdiff(1:n, ind),
    assessment = unique(ind)
  )
}


#' Add Assessment Indices
#'
#' Many `rsplit` and `rset` objects do not contain indicators for
#'   the assessment samples. `populate()` can be used to fill the slot
#'   for the appropriate indices.
#' @param x A `rsplit` and `rset` object.
#' @param ... Not currently used.
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
populate <- function(x, ...) UseMethod("populate")

#' @export
populate.rsplit <- function(x, ...) {
  check_dots_empty()
  x$out_id <- complement(x)
  x
}

#' @export
populate.rset <- function(x, ...) {
  check_dots_empty()
  x$splits <- map(x$splits, populate)
  x
}

## This will remove the assessment indices from an rsplit object
rm_out <- function(x) {
  x$out_id <- NA
  x
}

is_missing_out_id <- function(x) {
  identical(x$out_id, NA)
}
