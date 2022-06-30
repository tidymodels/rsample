#' Monte Carlo Cross-Validation
#'
#' One resample of Monte Carlo cross-validation takes a random sample (without
#'  replacement) of the original data set to be used for analysis. All other
#'  data points are added to the assessment set.
#' @template strata_details
#' @inheritParams vfold_cv
#' @inheritParams make_strata
#' @param prop The proportion of data to be retained for modeling/analysis.
#' @param times The number of times to repeat the sampling.
#' @export
#' @return An tibble with classes `mc_cv`, `rset`, `tbl_df`, `tbl`, and
#'  `data.frame`. The results include a column for the data split objects and a
#'  column called `id` that has a character string with the resample identifier.
#' @examples
#' mc_cv(mtcars, times = 2)
#' mc_cv(mtcars, prop = .5, times = 2)
#'
#' library(purrr)
#' data(wa_churn, package = "modeldata")
#'
#' set.seed(13)
#' resample1 <- mc_cv(wa_churn, times = 3, prop = .5)
#' map_dbl(
#'   resample1$splits,
#'   function(x) {
#'     dat <- as.data.frame(x)$churn
#'     mean(dat == "Yes")
#'   }
#' )
#'
#' set.seed(13)
#' resample2 <- mc_cv(wa_churn, strata = churn, times = 3, prop = .5)
#' map_dbl(
#'   resample2$splits,
#'   function(x) {
#'     dat <- as.data.frame(x)$churn
#'     mean(dat == "Yes")
#'   }
#' )
#'
#' set.seed(13)
#' resample3 <- mc_cv(wa_churn, strata = tenure, breaks = 6, times = 3, prop = .5)
#' map_dbl(
#'   resample3$splits,
#'   function(x) {
#'     dat <- as.data.frame(x)$churn
#'     mean(dat == "Yes")
#'   }
#' )
#' @export
mc_cv <- function(data, prop = 3 / 4, times = 25,
                  strata = NULL, breaks = 4, pool = 0.1, ...) {
  if (!missing(strata)) {
    strata <- tidyselect::vars_select(names(data), !!enquo(strata))
    if (length(strata) == 0) strata <- NULL
  }

  strata_check(strata, data)

  split_objs <-
    mc_splits(
      data = data,
      prop = prop,
      times = times,
      strata = strata,
      breaks = breaks,
      pool = pool
    )

  ## We remove the holdout indices since it will save space and we can
  ## derive them later when they are needed.

  split_objs$splits <- map(split_objs$splits, rm_out)

  mc_att <- list(
    prop = prop,
    times = times,
    strata = !is.null(strata)
  )

  new_rset(
    splits = split_objs$splits,
    ids = split_objs$id,
    attrib = mc_att,
    subclass = c("mc_cv", "rset")
  )
}

# Get the indices of the assessment set from the analysis set
mc_complement <- function(ind, n) {
  list(
    analysis = ind,
    assessment = setdiff(1:n, ind)
  )
}


mc_splits <- function(data, prop = 3 / 4, times = 25,
                      strata = NULL, breaks = 4, pool = 0.1) {
  if (!is.numeric(prop) | prop >= 1 | prop <= 0) {
    rlang::abort("`prop` must be a number on (0, 1).")
  }

  n <- nrow(data)
  if (is.null(strata)) {
    indices <- purrr::map(rep(n, times), sample, size = floor(n * prop))
  } else {
    stratas <- tibble::tibble(
      idx = 1:n,
      strata = make_strata(getElement(data, strata),
        breaks = breaks,
        pool = pool
      )
    )
    stratas <- split_unnamed(stratas, stratas$strata)
    stratas <-
      purrr::map_df(stratas, strat_sample, prop = prop, times = times)
    indices <- split_unnamed(stratas$idx, stratas$rs_id)
  }
  indices <- lapply(indices, mc_complement, n = n)
  split_objs <-
    purrr::map(indices, make_splits, data = data, class = "mc_split")
  list(
    splits = split_objs,
    id = names0(length(split_objs), "Resample")
  )
}

strat_sample <- function(x, prop, times, ...) {
  n <- nrow(x)
  idx <- purrr::map(rep(n, times), sample, size = floor(n * prop), ...)
  out <- purrr::map_df(idx, function(ind, x) x[sort(ind), "idx"], x = x)
  out$rs_id <- rep(1:times, each = floor(n * prop))
  out
}

#' Group Monte Carlo Cross-Validation
#'
#' Group Monte Carlo cross-validation creates splits of the data based
#'  on some grouping variable (which may have more than a single row
#'  associated with it). One resample of Monte Carlo cross-validation takes a
#'  random sample (without replacement) of groups in the original data set to be
#'  used for analysis. All other data points are added to the assessment set.
#'  A common use of this kind of resampling is when you have
#'  repeated measures of the same subject.
#'
#' @inheritParams mc_cv
#' @inheritParams make_groups
#' @export
#' @return A tibble with classes `group_mc_cv`,
#'  `rset`, `tbl_df`, `tbl`, and `data.frame`.
#'  The results include a column for the data split objects and an
#'  identification variable.
#' @examplesIf rlang::is_installed("modeldata")
#' data(ames, package = "modeldata")
#'
#' set.seed(123)
#' group_mc_cv(ames, group = Neighborhood, times = 5)
#'
#' @export
group_mc_cv <- function(data, group, prop = 3 / 4, times = 25, ...) {

  rlang::check_dots_empty()

  group <- validate_group({{ group }}, data)

  split_objs <-
    group_mc_splits(
      data = data,
      group = group,
      prop = prop,
      times = times
    )

  ## We remove the holdout indices since it will save space and we can
  ## derive them later when they are needed.
  split_objs$splits <- map(split_objs$splits, rm_out)

  mc_att <- list(
    group = group,
    prop = prop,
    times = times,
    balance = "prop"
  )

  new_rset(
    splits = split_objs$splits,
    ids = split_objs$id,
    attrib = mc_att,
    subclass = c("group_mc_cv", "rset")
  )
}

group_mc_splits <- function(data, group, prop = 3 / 4, times = 25) {

  group <- getElement(data, group)
  n <- nrow(data)
  indices <- make_groups(data, group, times, balance = "prop", prop = prop, replace = FALSE)
  indices <- lapply(indices, mc_complement, n = n)
  split_objs <-
    purrr::map(indices, make_splits, data = data, class = "grouped_mc_split")
  all_assessable <- purrr::map(split_objs, function(x) nrow(assessment(x)))

  if (any(all_assessable == 0)) {
    rlang::abort(
      c(
        "Some assessment sets contained zero rows",
        i = "Consider using a non-grouped resampling method"
      ),
      call = rlang::caller_env()
    )
  }
  list(
    splits = split_objs,
    id = names0(length(split_objs), "Resample")
  )
}
