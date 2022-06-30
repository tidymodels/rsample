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
#' @examples
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

  cv_att <- list(v = v, repeats = repeats, strata = !is.null(strata))

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
#'  create a smaller set of splits where more than one value is left
#'  out at a time. A common use of this kind of resampling is when you have
#'  repeated measures of the same subject.
#'
#' @inheritParams vfold_cv
#' @param v The number of partitions of the data set. If left as `NULL`, `v`
#'  will be set to the number of unique values in the group.
#' @param balance If `v` is less than the number of unique groups, how should
#'  groups be combined into folds? Should be one of
#'  `"groups"` or `"observations"`.
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
#'
#' @export
group_vfold_cv <- function(data, group = NULL, v = NULL, balance = c("groups", "observations"), ...) {

  group <- validate_group({{ group }}, data)
  balance <- rlang::arg_match(balance)

  split_objs <- group_vfold_splits(data = data, group = group, v = v, balance = balance)

  ## We remove the holdout indices since it will save space and we can
  ## derive them later when they are needed.

  split_objs$splits <- map(split_objs$splits, rm_out)

  # Update `v` if not supplied directly
  if (is.null(v)) {
    v <- length(split_objs$splits)
  }

  ## Save some overall information

  cv_att <- list(v = v, group = group, balance = balance, repeats = 1, strata = FALSE)

  new_rset(
    splits = split_objs$splits,
    ids = split_objs[, grepl("^id", names(split_objs))],
    attrib = cv_att,
    subclass = c("group_vfold_cv", "vfold_cv", "group_rset", "rset")
  )
}

group_vfold_splits <- function(data, group, v = NULL, balance) {

  group <- getElement(data, group)
  max_v <- length(unique(group))

  if (is.null(v)) {
    v <- max_v
  } else {
    check_v(v = v, max_v = max_v, rows = "rows", call = rlang::caller_env())
  }

  indices <- make_groups(data, group, v, balance)
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
