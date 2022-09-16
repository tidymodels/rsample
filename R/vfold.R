#' V-Fold Cross-Validation
#'
#' V-fold cross-validation (also known as k-fold cross-validation) randomly
#'  splits the data into V groups of roughly equal size (called "folds"). A
#'  resample of the analysis data consists of V-1 of the folds while the
#'  assessment set contains the final fold. In basic V-fold cross-validation
#'  (i.e. no repeats), the number of resamples is equal to V.
#' @details With more than one repeat, the basic V-fold cross-validation is
#'  conducted each time. For example, if three repeats are used with `v = 10`,
#'  there are a total of 30 splits: three groups of 10 that are generated
#'  separately.
#' @template strata_details
#' @inheritParams make_strata
#' @param data A data frame.
#' @param v The number of partitions of the data set.
#' @param repeats The number of times to repeat the V-fold partitioning.
#' @param strata A variable in `data` (single character or name) used to conduct
#'  stratified sampling. When not `NULL`, each resample is created within the
#'  stratification variable. Numeric `strata` are binned into quartiles.
#' @param ... Not currently used.
#' @export
#' @return A tibble with classes `vfold_cv`, `rset`, `tbl_df`, `tbl`, and
#'  `data.frame`. The results include a column for the data split objects and
#'  one or more identification variables. For a single repeat, there will be
#'  one column called `id` that has a character string with the fold identifier.
#'  For repeats, `id` is the repeat number and an additional column called `id2`
#'  that contains the fold information (within repeat).
#'
#' @examplesIf rlang::is_installed("modeldata")
#' vfold_cv(mtcars, v = 10)
#' vfold_cv(mtcars, v = 10, repeats = 2)
#'
#' library(purrr)
#' data(wa_churn, package = "modeldata")
#'
#' set.seed(13)
#' folds1 <- vfold_cv(wa_churn, v = 5)
#' map_dbl(
#'   folds1$splits,
#'   function(x) {
#'     dat <- as.data.frame(x)$churn
#'     mean(dat == "Yes")
#'   }
#' )
#'
#' set.seed(13)
#' folds2 <- vfold_cv(wa_churn, strata = churn, v = 5)
#' map_dbl(
#'   folds2$splits,
#'   function(x) {
#'     dat <- as.data.frame(x)$churn
#'     mean(dat == "Yes")
#'   }
#' )
#'
#' set.seed(13)
#' folds3 <- vfold_cv(wa_churn, strata = tenure, breaks = 6, v = 5)
#' map_dbl(
#'   folds3$splits,
#'   function(x) {
#'     dat <- as.data.frame(x)$churn
#'     mean(dat == "Yes")
#'   }
#' )
#' @export
vfold_cv <- function(data, v = 10, repeats = 1,
                     strata = NULL, breaks = 4, pool = 0.1, ...) {
  if (!missing(strata)) {
    strata <- tidyselect::vars_select(names(data), !!enquo(strata))
    if (length(strata) == 0) strata <- NULL
  }

  strata_check(strata, data)

  if (repeats == 1) {
    split_objs <- vfold_splits(
      data = data, v = v,
      strata = strata, breaks = breaks, pool = pool
    )
  } else {
    if (v == nrow(data)) {
      rlang::abort(
        glue::glue("Repeated resampling when `v` is {v} would create identical resamples")
      )
    }
    for (i in 1:repeats) {
      tmp <- vfold_splits(data = data, v = v, strata = strata, pool = pool)
      tmp$id2 <- tmp$id
      tmp$id <- names0(repeats, "Repeat")[i]
      split_objs <- if (i == 1) {
        tmp
      } else {
        rbind(split_objs, tmp)
      }
    }
  }

  ## We remove the holdout indices since it will save space and we can
  ## derive them later when they are needed.

  split_objs$splits <- map(split_objs$splits, rm_out)

  ## Save some overall information

  if (!is.null(strata)) names(strata) <- NULL
  cv_att <- list(
    v = v,
    repeats = repeats,
    strata = strata,
    breaks = breaks,
    pool = pool
  )

  new_rset(
    splits = split_objs$splits,
    ids = split_objs[, grepl("^id", names(split_objs))],
    attrib = cv_att,
    subclass = c("vfold_cv", "rset")
  )
}


vfold_splits <- function(data, v = 10, strata = NULL, breaks = 4, pool = 0.1) {

  n <- nrow(data)
  check_v(v, n, call = rlang::caller_env())

  if (is.null(strata)) {
    folds <- sample(rep(1:v, length.out = n))
    idx <- seq_len(n)
    indices <- split_unnamed(idx, folds)
  } else {
    stratas <- tibble::tibble(
      idx = 1:n,
      strata = make_strata(getElement(data, strata),
        breaks = breaks,
        pool = pool
      )
    )
    stratas <- split_unnamed(stratas, stratas$strata)
    stratas <- purrr::map(stratas, add_vfolds, v = v)
    stratas <- dplyr::bind_rows(stratas)
    indices <- split_unnamed(stratas$idx, stratas$folds)
  }

  indices <- lapply(indices, default_complement, n = n)

  split_objs <- purrr::map(indices, make_splits, data = data, class = "vfold_split")
  tibble::tibble(
    splits = split_objs,
    id = names0(length(split_objs), "Fold")
  )
}

#' Group V-Fold Cross-Validation
#'
#' Group V-fold cross-validation creates splits of the data based
#'  on some grouping variable (which may have more than a single row
#'  associated with it). The function can create as many splits as
#'  there are unique values of the grouping variable or it can
#'  create a smaller set of splits where more than one group is left
#'  out at a time. A common use of this kind of resampling is when you have
#'  repeated measures of the same subject.
#'
#' @inheritParams vfold_cv
#' @param v The number of partitions of the data set. If left as `NULL` (the
#'   default), `v` will be set to the number of unique values in the grouping
#'   variable, creating "leave-one-group-out" splits.
#' @param balance If `v` is less than the number of unique groups, how should
#'  groups be combined into folds? Should be one of
#'  `"groups"`, which will assign roughly the same number of groups to each
#'  fold, or `"observations"`, which will assign roughly the same number of
#'  observations to each fold.
#' @inheritParams make_groups
#'
#' @export
#' @return A tibble with classes `group_vfold_cv`,
#'  `rset`, `tbl_df`, `tbl`, and `data.frame`.
#'  The results include a column for the data split objects and an
#'  identification variable.
#' @examplesIf rlang::is_installed("modeldata")
#' data(ames, package = "modeldata")
#'
#' set.seed(123)
#' group_vfold_cv(ames, group = Neighborhood, v = 5)
#' group_vfold_cv(
#'   ames,
#'   group = Neighborhood,
#'   v = 5,
#'   balance = "observations"
#' )
#' group_vfold_cv(ames, group = Neighborhood, v = 5, repeats = 2)
#'
#' # Leave-one-group-out CV
#' group_vfold_cv(ames, group = Neighborhood)
#'
#' library(dplyr)
#' data(Sacramento, package = "modeldata")
#'
#' city_strata <- Sacramento %>%
#'   group_by(city) %>%
#'   summarize(strata = mean(price)) %>%
#'   summarize(city = city,
#'             strata = cut(strata, quantile(strata), include.lowest = TRUE))
#'
#' sacramento_data <- Sacramento %>%
#'   full_join(city_strata, by = "city")
#'
#' group_vfold_cv(sacramento_data, city, strata = strata)
#'
#' @export
group_vfold_cv <- function(data, group = NULL, v = NULL, repeats = 1, balance = c("groups", "observations"), ..., strata = NULL, pool = 0.1) {

  group <- validate_group({{ group }}, data)
  balance <- rlang::arg_match(balance)

  if (!missing(strata)) {
    strata <- check_grouped_strata({{ group }}, {{ strata }}, pool, data)
  }

  if (repeats == 1) {
    split_objs <- group_vfold_splits(data = data, group = group, v = v, balance = balance, strata = strata, pool = pool)
  } else {
    if (is.null(v)) {
      rlang::abort(
        "Repeated resampling when `v` is `NULL` would create identical resamples"
      )
    }
    if (v == length(unique(getElement(data, group)))) {
      rlang::abort(
        glue::glue("Repeated resampling when `v` is {v} would create identical resamples")
      )
    }
    for (i in 1:repeats) {
      tmp <- group_vfold_splits(data = data, group = group, v = v, balance = balance, strata = strata, pool = pool)
      tmp$id2 <- tmp$id
      tmp$id <- names0(repeats, "Repeat")[i]
      split_objs <- if (i == 1) {
        tmp
      } else {
        rbind(split_objs, tmp)
      }
    }
  }

  ## We remove the holdout indices since it will save space and we can
  ## derive them later when they are needed.

  split_objs$splits <- map(split_objs$splits, rm_out)

  # Update `v` if not supplied directly
  if (is.null(v)) {
    v <- length(split_objs$splits)
  }

  ## Save some overall information

  cv_att <- list(v = v, group = group, balance = balance, repeats = 1, strata = strata, pool = pool)

  new_rset(
    splits = split_objs$splits,
    ids = split_objs[, grepl("^id", names(split_objs))],
    attrib = cv_att,
    subclass = c("group_vfold_cv", "vfold_cv", "group_rset", "rset")
  )
}

group_vfold_splits <- function(data, group, v = NULL, balance, strata = NULL, pool = 0.1) {

  group <- getElement(data, group)
  max_v <- length(unique(group))
  if (!is.null(strata)) {
    strata <- getElement(data, strata)
    strata <- as.character(strata)
    strata <- make_strata(strata, pool = pool)

    if (is.null(v)) {
      # Set max_v to be the lowest number of groups in a single strata
      # to ensure that all folds get each strata
      max_v <- min(
        vec_count(
          vec_unique(
            data.frame(group, strata)
          )$strata
        )$count
      )
      message <- c(
        "Leaving `v = NULL` while using stratification will set `v` to the number of groups present in the least common stratum."
      )

      if (max_v < 5) {
        rlang::abort(c(
          message,
          x = glue::glue("The least common stratum only had {max_v} groups, which may not be enough for cross-validation."),
          i = "Set `v` explicitly to override this error."
        ),
        call = rlang::caller_env())
      }

      rlang::warn(c(
        message,
        i = "Set `v` explicitly to override this warning."
      ),
      call = rlang::caller_env())
    }
  }

  if (is.null(v)) {
    v <- max_v
  }
  check_v(v = v, max_v = max_v, rows = "groups", call = rlang::caller_env())

  indices <- make_groups(data, group, v, balance, strata)
  indices <- lapply(indices, default_complement, n = nrow(data))
  split_objs <-
    purrr::map(indices,
               make_splits,
               data = data,
               class = c("group_vfold_split", "vfold_split")
    )
  tibble::tibble(
    splits = split_objs,
    id = names0(length(split_objs), "Resample")
  )
}

add_vfolds <- function(x, v) {
  x$folds <- sample(rep(1:v, length.out = nrow(x)))
  x
}

check_v <- function(v, max_v, rows = "rows", call = rlang::caller_env()) {
  if (!is.numeric(v) || length(v) != 1 || v < 0) {
    rlang::abort("`v` must be a single positive integer", call = call)
  } else if (v > max_v) {
    rlang::abort(
      glue::glue("The number of {rows} is less than `v = {v}`"), call = call
    )
  }
}

check_grouped_strata <- function(group, strata, pool, data) {

  strata <- tidyselect::vars_select(names(data), !!enquo(strata))
  grouped_table <- tibble(
    group = getElement(data, group),
    strata = getElement(data, strata)
  )

  if (nrow(vctrs::vec_unique(grouped_table)) !=
      nrow(vctrs::vec_unique(grouped_table["group"]))) {
    rlang::abort("`strata` must be constant across all members of each `group`.")
  }

  strata
}
