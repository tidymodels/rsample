#' Tidy Resampling Object
#'
#' The `tidy` function from the \pkg{broom} package can be used on `rset` and
#'  `rsplit` objects to generate tibbles with which rows are in the analysis and
#'  assessment sets.
#' @param x A  `rset` or  `rsplit` object
#' @param unique_ind Should unique row identifiers be returned? For example,
#'  if `FALSE` then bootstrapping results will include multiple rows in the
#'  sample for the same row in the original data.
#' @inheritParams rlang::args_dots_empty
#' @return A tibble with columns `Row` and `Data`. The latter has possible
#'  values "Analysis" or "Assessment". For `rset` inputs, identification columns
#'  are also returned but their names and values depend on the type of
#'  resampling. `vfold_cv` contains a column "Fold" and, if repeats are used,
#'  another called "Repeats". `bootstraps` and `mc_cv` use the column
#'  "Resample".
#' @details Note that for nested resampling, the rows of the inner resample,
#'  named `inner_Row`, are *relative* row indices and do not correspond to the
#'  rows in the original data set.
#' @examplesIf rlang::is_installed("ggplot2")
#' library(ggplot2)
#' theme_set(theme_bw())
#'
#' set.seed(4121)
#' cv <- tidy(vfold_cv(mtcars, v = 5))
#' ggplot(cv, aes(x = Fold, y = Row, fill = Data)) +
#'   geom_tile() +
#'   scale_fill_brewer()
#'
#' set.seed(4121)
#' rcv <- tidy(vfold_cv(mtcars, v = 5, repeats = 2))
#' ggplot(rcv, aes(x = Fold, y = Row, fill = Data)) +
#'   geom_tile() +
#'   facet_wrap(~Repeat) +
#'   scale_fill_brewer()
#'
#' set.seed(4121)
#' mccv <- tidy(mc_cv(mtcars, times = 5))
#' ggplot(mccv, aes(x = Resample, y = Row, fill = Data)) +
#'   geom_tile() +
#'   scale_fill_brewer()
#'
#' set.seed(4121)
#' bt <- tidy(bootstraps(mtcars, time = 5))
#' ggplot(bt, aes(x = Resample, y = Row, fill = Data)) +
#'   geom_tile() +
#'   scale_fill_brewer()
#'
#' dat <- data.frame(day = 1:30)
#' # Resample by week instead of day
#' ts_cv <- rolling_origin(dat,
#'   initial = 7, assess = 7,
#'   skip = 6, cumulative = FALSE
#' )
#' ts_cv <- tidy(ts_cv)
#' ggplot(ts_cv, aes(x = Resample, y = factor(Row), fill = Data)) +
#'   geom_tile() +
#'   scale_fill_brewer()
#' @export
tidy.rsplit <- function(x, unique_ind = TRUE, ...) {
  check_dots_empty()
  if (unique_ind) x$in_id <- unique(x$in_id)
  out <- tibble(
    Row = c(x$in_id, complement(x)),
    Data = rep(
      c("Analysis", "Assessment"),
      c(length(x$in_id), length(complement(x)))
    )
  )
  out <- dplyr::arrange(.data = out, Data, Row)
  out
}

#' @rdname tidy.rsplit
#' @export
tidy.rset <- function(x, unique_ind = TRUE, ...) {
  check_dots_empty()
  stacked <- purrr::map(x$splits, tidy, unique_ind = unique_ind)
  for (i in seq_along(stacked)) {
    stacked[[i]]$Resample <- x$id[i]
  }
  stacked <- dplyr::bind_rows(stacked)
  stacked <- dplyr::arrange(.data = stacked, Data, Row)
  stacked
}

#' @rdname tidy.rsplit
#' @export
tidy.vfold_cv <- function(x, ...) {
  check_dots_empty()
  stacked <- purrr::map(x$splits, tidy)
  for (i in seq_along(stacked)) {
    if (attr(x, "repeats") > 1) {
      stacked[[i]]$Repeat <- x$id[i]
      stacked[[i]]$Fold <- x$id2[i]
    } else {
      stacked[[i]]$Fold <- x$id[i]
    }
  }
  stacked <- dplyr::bind_rows(stacked)
  stacked <- dplyr::arrange(.data = stacked, Data, Row)
  stacked
}

#' @rdname tidy.rsplit
#' @export
tidy.nested_cv <- function(x, unique_ind = TRUE, ...) {
  check_dots_empty()
  x$inner_tidy <- purrr::map(
    x$inner_resamples,
    tidy_wrap,
    unique_ind = unique_ind
  )
  inner_tidy <- tidyr::unnest(x, inner_tidy)
  class(x) <- class(x)[class(x) != "nested_cv"]
  outer_tidy <- tidy(x, unique_ind = unique_ind)
  id_cols <- names(outer_tidy)
  id_cols <- id_cols[!(id_cols %in% c("Row", "Data"))]

  inner_id <- grep("^id", names(inner_tidy))
  if (length(inner_id) != length(id_cols)) {
    rlang::abort("Cannot merge tidy data sets")
  }
  names(inner_tidy)[inner_id] <- id_cols
  full_join(outer_tidy, inner_tidy, by = id_cols)
}

tidy_wrap <- function(x, unique_ind = TRUE) {
  x <- tidy(x, unique_ind = unique_ind)
  names(x) <- paste0("inner_", names(x))
  x
}
