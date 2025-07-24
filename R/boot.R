#' Bootstrap Sampling
#'
#' A bootstrap sample is a sample that is the same size as the original data
#'  set that is made using replacement. This results in analysis samples that
#'  have multiple replicates of some of the original rows of the data. The
#'  assessment set is defined as the rows of the original data that were not
#'  included in the bootstrap sample. This is often referred to as the
#'  "out-of-bag" (OOB) sample.
#' @details The argument `apparent` enables the option of an additional
#'  "resample" where the analysis and assessment data sets are the same as the
#'  original data set. This can be required for some types of analysis of the
#'  bootstrap results.
#'
#' @template strata_details
#' @inheritParams vfold_cv
#' @inheritParams make_strata
#' @param times The number of bootstrap samples.
#' @param apparent A logical. Should an extra resample be added where the
#'  analysis and holdout subset are the entire data set. This is required for
#'  some estimators used by the [summary()] function that require the apparent
#'  error rate.
#' @export
#' @return A tibble with classes `bootstraps`, `rset`, `tbl_df`, `tbl`, and
#'  `data.frame`. The results include a column for the data split objects and a
#'  column called `id` that has a character string with the resample identifier.
#' @examplesIf rlang::is_installed("modeldata")
#' bootstraps(mtcars, times = 2)
#' bootstraps(mtcars, times = 2, apparent = TRUE)
#'
#' library(purrr)
#' library(modeldata)
#' data(wa_churn)
#'
#' set.seed(13)
#' resample1 <- bootstraps(wa_churn, times = 3)
#' map_dbl(
#'   resample1$splits,
#'   function(x) {
#'     dat <- as.data.frame(x)$churn
#'     mean(dat == "Yes")
#'   }
#' )
#'
#' set.seed(13)
#' resample2 <- bootstraps(wa_churn, strata = churn, times = 3)
#' map_dbl(
#'   resample2$splits,
#'   function(x) {
#'     dat <- as.data.frame(x)$churn
#'     mean(dat == "Yes")
#'   }
#' )
#'
#' set.seed(13)
#' resample3 <- bootstraps(wa_churn, strata = tenure, breaks = 6, times = 3)
#' map_dbl(
#'   resample3$splits,
#'   function(x) {
#'     dat <- as.data.frame(x)$churn
#'     mean(dat == "Yes")
#'   }
#' )
#' @export
bootstraps <- function(
  data,
  times = 25,
  strata = NULL,
  breaks = 4,
  pool = 0.1,
  apparent = FALSE,
  ...
) {
  check_dots_empty()

  if (!missing(strata)) {
    strata <- tidyselect::vars_select(names(data), !!enquo(strata))
    if (length(strata) == 0) strata <- NULL
  }

  check_strata(strata, data)

  split_objs <-
    boot_splits(
      data = data,
      times = times,
      strata = strata,
      breaks = breaks,
      pool = pool
    )

  if (apparent) {
    split_objs <- bind_rows(split_objs, apparent(data))
  }

  if (!is.null(strata)) {
    names(strata) <- NULL
  }
  boot_att <- list(
    times = times,
    apparent = apparent,
    strata = strata,
    breaks = breaks,
    pool = pool
  )

  new_rset(
    splits = split_objs$splits,
    ids = split_objs$id,
    attrib = boot_att,
    subclass = c("bootstraps", "rset")
  )
}

# Get the indices of the analysis set from the analysis set (= bootstrap sample)
boot_complement <- function(ind, n) {
  list(analysis = ind, assessment = NA)
}

boot_splits <- function(
  data,
  times = 25,
  strata = NULL,
  breaks = 4,
  pool = 0.1
) {
  n <- nrow(data)

  if (is.null(strata)) {
    indices <- purrr::map(rep(n, times), sample, replace = TRUE)
  } else {
    stratas <- tibble::tibble(
      idx = 1:n,
      strata = make_strata(
        getElement(data, strata),
        breaks = breaks,
        pool = pool
      )
    )
    stratas <- split_unnamed(stratas, stratas$strata)
    stratas <-
      purrr::map(
        stratas,
        strat_sample,
        prop = 1,
        times = times,
        replace = TRUE
      ) |>
      list_rbind()
    indices <- split_unnamed(stratas$idx, stratas$rs_id)
  }

  indices <- lapply(indices, boot_complement, n = n)

  split_objs <-
    purrr::map(indices, make_splits, data = data, class = "boot_split")

  all_assessable <- purrr::map(split_objs, function(x) nrow(assessment(x)))
  if (any(all_assessable == 0)) {
    cli_warn(
      "Some assessment sets contained zero rows.",
      class = "rsample_bootstrap_empty_assessment",
      call = rlang::caller_env()
    )
  }

  list(
    splits = split_objs,
    id = names0(length(split_objs), "Bootstrap")
  )
}

#' Group Bootstraps
#'
#' Group bootstrapping creates splits of the data based
#'  on some grouping variable (which may have more than a single row
#'  associated with it). A common use of this kind of resampling is when you
#'  have repeated measures of the same subject.
#'  A bootstrap sample is a sample that is the same size as the original data
#'  set that is made using replacement. This results in analysis samples that
#'  have multiple replicates of some of the original rows of the data. The
#'  assessment set is defined as the rows of the original data that were not
#'  included in the bootstrap sample. This is often referred to as the
#'  "out-of-bag" (OOB) sample.
#' @details The argument `apparent` enables the option of an additional
#'  "resample" where the analysis and assessment data sets are the same as the
#'  original data set. This can be required for some types of analysis of the
#'  bootstrap results.
#'
#' @inheritParams bootstraps
#' @inheritParams make_groups
#' @export
#' @return An tibble with classes `group_bootstraps` `bootstraps`, `rset`,
#'  `tbl_df`, `tbl`, and `data.frame`. The results include a column for the data
#'  split objects and a column called `id` that has a character string with the
#'  resample identifier.
#' @examplesIf rlang::is_installed("modeldata")
#' data(ames, package = "modeldata")
#'
#' set.seed(13)
#' group_bootstraps(ames, Neighborhood, times = 3)
#' group_bootstraps(ames, Neighborhood, times = 3, apparent = TRUE)
#'
#' @export
group_bootstraps <- function(
  data,
  group,
  times = 25,
  apparent = FALSE,
  ...,
  strata = NULL,
  pool = 0.1
) {
  check_dots_empty()

  group <- validate_group({{ group }}, data)

  if (!missing(strata)) {
    strata <- check_grouped_strata({{ group }}, {{ strata }}, pool, data)
  }

  split_objs <-
    group_boot_splits(
      data = data,
      group = group,
      times = times,
      strata = strata,
      pool = pool
    )

  ## We remove the holdout indices since it will save space and we can
  ## derive them later when they are needed.
  split_objs$splits <- map(split_objs$splits, rm_out)

  if (apparent) {
    split_objs <- bind_rows(split_objs, apparent(data))
  }

  # This is needed for printing checks; strata can't be missing
  if (is.null(strata)) {
    strata <- FALSE
  }
  boot_att <- list(
    times = times,
    apparent = apparent,
    strata = strata,
    pool = pool,
    group = group
  )

  new_rset(
    splits = split_objs$splits,
    ids = split_objs$id,
    attrib = boot_att,
    subclass = c("group_bootstraps", "bootstraps", "group_rset", "rset")
  )
}

group_boot_splits <- function(
  data,
  group,
  times = 25,
  strata = NULL,
  pool = 0.1
) {
  group <- getElement(data, group)
  if (!is.null(strata)) {
    strata <- getElement(data, strata)
    strata <- as.character(strata)
    strata <- make_strata(strata, pool = pool)
  }

  n <- nrow(data)

  indices <- make_groups(
    data,
    group,
    times,
    balance = "prop",
    prop = 1,
    replace = TRUE,
    strata = strata
  )

  indices <- lapply(indices, boot_complement, n = n)
  split_objs <- purrr::map(
    indices,
    make_splits,
    data = data,
    class = c("group_boot_split", "boot_split")
  )

  all_assessable <- purrr::map(split_objs, function(x) nrow(assessment(x)))
  if (any(all_assessable == 0)) {
    cli_warn(
      c(
        "Some assessment sets contained zero rows.",
        i = "Consider using a non-grouped resampling method."
      ),
      call = rlang::caller_env()
    )
  }

  list(
    splits = split_objs,
    id = names0(length(split_objs), "Bootstrap")
  )
}
