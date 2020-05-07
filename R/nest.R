#' Nested or Double Resampling
#'
#' `nested_cv` can be used to take the results of one resampling procedure
#'   and conduct further resamples within each split. Any type of resampling
#'   used in `rsample` can be used.
#'
#' @details
#' It is a bad idea to use bootstrapping as the outer resampling procedure (see
#'   the example below)
#' @param data  A data frame.
#' @param outside The initial resampling specification. This can be an already
#'   created object or an expression of a new object (see the examples below).
#'   If the latter is used, the `data` argument does not need to be
#'   specified and, if it is given, will be ignored.
#' @param inside An expression for the type of resampling to be conducted
#'   within the initial procedure.
#' @return  An tibble with classe `nested_cv` and any other classes that
#'   outer resampling process normally contains. The results include a
#'  column for the outer data split objects, one or more `id` columns,
#'  and a column of nested tibbles called `inner_resamples` with the
#'  additional resamples.
#' @examples
#' ## Using expressions for the resampling procedures:
#' nested_cv(mtcars, outside = vfold_cv(v = 3), inside = bootstraps(times = 5))
#'
#' ## Using an existing object:
#' folds <- vfold_cv(mtcars)
#' nested_cv(mtcars, folds, inside = bootstraps(times = 5))
#'
#' ## The dangers of outer bootstraps:
#' set.seed(2222)
#' bad_idea <- nested_cv(mtcars,
#'                       outside = bootstraps(times = 5),
#'                       inside = vfold_cv(v = 3))
#'
#' first_outer_split <- bad_idea$splits[[1]]
#' outer_analysis <- as.data.frame(first_outer_split)
#' sum(grepl("Volvo 142E", rownames(outer_analysis)))
#'
#' ## For the 3-fold CV used inside of each bootstrap, how are the replicated
#' ## `Volvo 142E` data partitioned?
#' first_inner_split <- bad_idea$inner_resamples[[1]]$splits[[1]]
#' inner_analysis <- as.data.frame(first_inner_split)
#' inner_assess   <- as.data.frame(first_inner_split, data = "assessment")
#'
#' sum(grepl("Volvo 142E", rownames(inner_analysis)))
#' sum(grepl("Volvo 142E", rownames(inner_assess)))
#' @export
nested_cv <- function(data, outside, inside)  {
  nest_args <- formalArgs(nested_cv)
  cl <- match.call()

  boot_msg <-
    paste0(
      "Using bootstrapping as the outer resample is dangerous ",
      "since the inner resample might have the same data ",
      "point in both the analysis and assessment set."
    )

  outer_cl <- cl[["outside"]]
  if (is_call(outer_cl)) {
    if (grepl("^bootstraps", deparse(outer_cl)))
      warning(boot_msg, call. = FALSE)
    outer_cl$data <- quote(data)
    outside <- eval(outer_cl)
  } else {
    if (inherits(outside, "bootstraps"))
      warning(boot_msg, call. = FALSE)
  }

  inner_cl <- cl[["inside"]]
  if (!is_call(inner_cl))
    stop(
      "`inside` should be a expression such as `vfold()` or ",
      "bootstraps(times = 10)` instead of a existing object.",
      call. = FALSE
    )
  inside <- map(outside$splits, inside_resample, cl = inner_cl)

  out <- dplyr::mutate(outside, inner_resamples = inside)

  out <- add_class(out, cls = "nested_cv", at_end = FALSE)

  attr(out, "outside") <- cl$outside
  attr(out, "inside") <- cl$inside

  out
}

inside_resample <- function(src, cl) {
  cl$data <- quote(as.data.frame(src))
  eval(cl)
}

#' @export
print.nested_cv <- function(x, ...) {
  char_x <- paste("#", pretty(x))
  cat(char_x, sep = "\n")
  class(x) <- class(tibble())
  print(x, ...)
}

# ------------------------------------------------------------------------------

#' @export
`[.nested_cv` <- function(x, i, j, drop = FALSE, ...) {
  # Call `[.rset`
  out <- NextMethod()

  # If we already dropped the rset subclass, return
  if (!inherits(out, "rset")) {
    return(out)
  }

  if (col_subset_requires_fallback_nested_cv(out)) {
    out <- rset_strip(out)
  }

  out
}

# Must have the `inner_resamples` column exactly once
col_subset_requires_fallback_nested_cv <- function(new) {
  names <- names(new)
  inner_resamples_indicator <- col_matches_inner_resamples(names)

  times <- sum(inner_resamples_indicator)

  !identical(times, 1L)
}

col_matches_inner_resamples <- function(x) {
  vec_in(x, "inner_resamples")
}

# ------------------------------------------------------------------------------

#' @export
`names<-.nested_cv` <- function(x, value) {
  # Call `names<-.rset`
  out <- NextMethod()

  # If we already dropped the rset subclass, return
  if (!inherits(out, "rset")) {
    return(out)
  }

  old_names <- names(x)
  new_names <- names(out)

  old_rset_inner_resamples_indicator <- col_matches_inner_resamples(old_names)
  new_rset_inner_resamples_indicator <- col_matches_inner_resamples(new_names)

  # Ensure that the single `inner_resamples` column is in the same place
  if (!identical(old_rset_inner_resamples_indicator, new_rset_inner_resamples_indicator)) {
    out <- rset_strip(out)
    return(out)
  }

  out
}
