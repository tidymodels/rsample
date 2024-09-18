#' "Reshuffle" an rset to re-generate a new rset with the same parameters
#'
#' This function re-generates an rset object, using the same arguments used
#' to generate the original.
#'
#' @param rset The `rset` object to be reshuffled
#'
#' @return An rset of the same class as `rset`.
#'
#' @examples
#' set.seed(123)
#' (starting_splits <- group_vfold_cv(mtcars, cyl, v = 3))
#' reshuffle_rset(starting_splits)
#'
#' @export
reshuffle_rset <- function(rset) {
  if (!inherits(rset, "rset")) {
    cli_abort("{.arg rset} must be an {.cls rset} object.")
  }

  if (inherits(rset, "manual_rset")) {
    cli_abort("{.arg manual_rset} objects cannot be reshuffled.")
  }

  # non-random classes is defined below
  if (any(non_random_classes %in% class(rset))) {
    cls <- class(rset)[[1]]
    cli::cli_warn(
      "{.fun reshuffle_rset} will return an identical {.cls rset} when called on {.cls {cls}} objects."
    )
    if ("validation_set" %in% class(rset)) {
      return(rset)
    }
  }

  rset_type <- class(rset)[[1]]
  split_arguments <- .get_split_args(rset)
  if (identical(split_arguments$strata, TRUE)) {
    cli_abort(c(
      "Cannot reshuffle this rset ({.code attr(rset, 'strata')} is {.val TRUE}, not a column identifier)",
      i = "If the original object was created with an older version of rsample, try recreating it with the newest version of the package."
    ))
  }

  do.call(
    rset_type,
    c(list(data = rset$splits[[1]]$data), split_arguments)
  )
}

non_random_classes <- c(
  "sliding_index",
  "sliding_period",
  "sliding_window",
  "rolling_origin",
  "validation_time_split",
  "validation_set"
)
