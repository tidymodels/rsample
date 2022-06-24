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
#' @inheritParams make_groups
#' @export
#' @return A tibble with classes `group_vfold_cv`,
#'  `rset`, `tbl_df`, `tbl`, and `data.frame`.
#'  The results include a column for the data split objects and an
#'  identification variable.
#' @examplesIf rlang::is_installed("modeldata")
#' data(Sacramento, package = "modeldata")
#'
#' set.seed(123)
#' group_vfold_cv(Sacramento, group = city, v = 5)
#'
#' @export
group_vfold_cv <- function(data, group = NULL, v = NULL, balance = balance_groups(), ...) {
  if (!missing(group)) {
    group <- tidyselect::vars_select(names(data), !!enquo(group))
    if (length(group) == 0) {
      group <- NULL
    }
  }

  if (is.null(group) || !is.character(group) || length(group) != 1) {
    rlang::abort(
      "`group` should be a single character value for the column that will be used for splitting."
    )
  }
  if (!any(names(data) == group)) {
    rlang::abort("`group` should be a column in `data`.")
  }

  split_objs <- group_vfold_splits(data = data, group = group, v = v, balance = balance)

  ## We remove the holdout indices since it will save space and we can
  ## derive them later when they are needed.

  split_objs$splits <- map(split_objs$splits, rm_out)

  # Update `v` if not supplied directly
  if (is.null(v)) {
    v <- length(split_objs$splits)
  }

  ## Save some overall information

  cv_att <- list(v = v, group = group)

  new_rset(
    splits = split_objs$splits,
    ids = split_objs[, grepl("^id", names(split_objs))],
    attrib = cv_att,
    subclass = c("group_vfold_cv", "rset")
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
      class = "group_vfold_split"
    )
  tibble::tibble(
    splits = split_objs,
    id = names0(length(split_objs), "Resample")
  )
}
