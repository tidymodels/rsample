#' Nested or Double Resampling
#'
#' `nested_cv` can be used to take the results of one resampling procedure
#'   and conduct further resamples within each split. Any type of resampling
#'   used in rsample can be used.
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
#' @return  An tibble with `nested_cv` class and any other classes that
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
#'   outside = bootstraps(times = 5),
#'   inside = vfold_cv(v = 3)
#' )
#'
#' first_outer_split <- get_rsplit(bad_idea, 1)
#' outer_analysis <- analysis(first_outer_split)
#' sum(grepl("Camaro Z28", rownames(outer_analysis)))
#'
#' ## For the 3-fold CV used inside of each bootstrap, how are the replicated
#' ## `Camaro Z28` data partitioned?
#' first_inner_split <- get_rsplit(bad_idea$inner_resamples[[1]], 1)
#' inner_analysis <- analysis(first_inner_split)
#' inner_assess <- assessment(first_inner_split)
#'
#' sum(grepl("Camaro Z28", rownames(inner_analysis)))
#' sum(grepl("Camaro Z28", rownames(inner_assess)))
#' @export
nested_cv <- function(data, outside, inside) {
  cl <- match.call()
  env <- rlang::caller_env()

  boot_msg <-
    paste0(
      "Using bootstrapping as the outer resample is dangerous ",
      "since the inner resample might have the same data ",
      "point in both the analysis and assessment set."
    )

  outer_cl <- cl[["outside"]]
  if (is_call(outer_cl)) {
    using_bootstraps <- grepl(
      "^bootstraps",
      paste(
        deparse(outer_cl, width.cutoff = 500L),
        collapse = " "
      )
    )
    if (using_bootstraps) {
      warn(boot_msg)
    }
    outer_cl <- rlang::call_modify(outer_cl, data = data)
    outside <- eval(outer_cl, env)
  } else {
    if (inherits(outside, "bootstraps")) {
      warn(boot_msg)
    }
  }

  inner_cl <- cl[["inside"]]
  if (!is_call(inner_cl)) {
    abort(
      "`inside` should be a expression such as `vfold()` or ",
      "bootstraps(times = 10)` instead of an existing object.",
    )
  }
  inside <- map(outside$splits, inside_resample, cl = inner_cl, env = env)

  out <- dplyr::mutate(outside, inner_resamples = inside)
  out <- add_class(out, cls = "nested_cv")
  attr(out, "outside") <- cl$outside
  attr(out, "inside") <- cl$inside

  out
}

inside_resample <- function(src, cl, env) {
  cl <- rlang::call_modify(cl, data = as.data.frame(src))
  eval(cl, envir = env)
}
