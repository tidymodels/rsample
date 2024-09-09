#' Time-based Resampling
#'
#' @description
#' These resampling functions are focused on various forms of _time series_
#' resampling.
#'
#' - `sliding_window()` uses the row number when computing the resampling
#'   indices. It is independent of any time index, but is useful with
#'   completely regular series.
#'
#' - `sliding_index()` computes resampling indices relative to the `index`
#'   column. This is often a Date or POSIXct column, but doesn't have to be.
#'   This is useful when resampling irregular series, or for using irregular
#'   lookback periods such as `lookback = lubridate::years(1)` with daily
#'   data (where the number of days in a year may vary).
#'
#' - `sliding_period()` first breaks up the `index` into less granular groups
#'   based on `period`, and then uses that to construct the resampling indices.
#'   This is extremely useful for constructing rolling monthly or yearly
#'   windows from daily data.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @param data A data frame.
#'
#' @param index The index to compute resampling indices relative to, specified
#'   as a bare column name. This must be an existing column in `data`.
#'
#'   - For `sliding_index()`, this is commonly a date vector, but is not
#'     required.
#'
#'   - For `sliding_period()`, it is required that this is a Date or POSIXct
#'     vector.
#'
#'   The `index` must be an _increasing_ vector, but duplicate values are
#'   allowed. Additionally, the index cannot contain any missing values.
#'
#' @param period The period to group the `index` by. This is specified as a
#'   single string, such as `"year"` or `"month"`. See the `.period` argument
#'   of [slider::slide_period()] for the full list of options and further
#'   explanation.
#'
#' @param lookback The number of elements to look back from the current element
#'   when computing the resampling indices of the analysis set. The current
#'   row is always included in the analysis set.
#'
#'   - For `sliding_window()`, a single integer defining the number of rows to
#'     look back from the current row.
#'
#'   - For `sliding_index()`, a single object that will be subtracted from the
#'     `index` as `index - lookback` to define the boundary of where to start
#'     searching for rows to include in the current resample. This is often
#'     an integer value corresponding to the number of days to look back,
#'     or a lubridate Period object.
#'
#'   - For `sliding_period()`, a single integer defining the number of groups
#'     to look back from the current group, where the groups were defined from
#'     breaking up the `index` according to the `period`.
#'
#'   In all cases, `Inf` is also allowed to force an expanding window.
#'
#' @param assess_start,assess_stop This combination of arguments determines
#'   how far into the future to look when constructing the assessment set.
#'   Together they construct a range of
#'   `[index + assess_start, index + assess_stop]` to search for rows to include
#'   in the assessment set.
#'
#'   Generally, `assess_start` will always be `1` to indicate that the first
#'   value to potentially include in the assessment set should start one element
#'   after the current row, but it can be increased to a larger value to
#'   create "gaps" between the analysis and assessment set if you are worried
#'   about high levels of correlation in short term forecasting.
#'
#'   - For `sliding_window()`, these are both single integers defining the
#'     number of rows to look forward from the current row.
#'
#'   - For `sliding_index()`, these are single objects that will be added
#'     to the `index` to compute the range to search for rows to include
#'     in the assessment set. This is often an integer value corresponding to
#'     the number of days to look forward, or a lubridate Period object.
#'
#'   - For `sliding_period()`, these are both single integers defining the
#'     number of groups to look forward from the current group, where the groups
#'     were defined from breaking up the `index` according to the `period`.
#'
#' @param complete A single logical. When using `lookback` to compute the
#'   analysis sets, should only complete windows be considered? If set to
#'   `FALSE`, partial windows will be used until it is possible to create
#'   a complete window (based on `lookback`). This is a way to use an
#'   expanding window up to a certain point, and then switch to a sliding
#'   window.
#'
#' @param step A single positive integer. After computing the resampling
#'   indices, `step` is used to thin out the results by selecting every
#'   `step`-th result by subsetting the indices with
#'   `seq(1L, n_indices, by = step)`. `step` is applied after `skip`.
#'   Note that `step` is independent of any time `index` used.
#'
#' @param skip A single positive integer, or zero. After computing the
#'   resampling indices, the first `skip` results will be dropped by subsetting
#'   the indices with `seq(skip + 1L, n_indices)`. This can be especially
#'   useful when combined with `lookback = Inf`, which creates an expanding
#'   window starting from the first row. By skipping forward, you can drop
#'   the first few windows that have very few data points. `skip` is
#'   applied before `step`. Note that `skip` is independent of any time
#'   `index` used.
#'
#' @param every A single positive integer. The number of periods to group
#'   together.
#'
#'   For example, if the `period` was set to `"year"` with an `every`
#'   value of 2, then the years 1970 and 1971 would be placed in the same
#'   group.
#'
#' @param origin The reference date time value. The default when left
#'   as `NULL` is the epoch time of `1970-01-01 00:00:00`,
#'   _in the time zone of the index_.
#'
#'   This is generally used to define the anchor time to count from,
#'   which is relevant when the `every` value is `> 1`.
#'
#' @seealso
#' [rolling_origin()]
#'
#' [slider::slide()], [slider::slide_index()], and [slider::slide_period()],
#' which power these resamplers.
#'
#' @name slide-resampling
#'
#' @examplesIf rlang::is_installed("modeldata")
#' library(vctrs)
#' library(tibble)
#' library(modeldata)
#' data("Chicago")
#'
#' index <- new_date(c(1, 3, 4, 7, 8, 9, 13, 15, 16, 17))
#' df <- tibble(x = 1:10, index = index)
#' df
#'
#' # Look back two rows beyond the current row, for a total of three rows
#' # in each analysis set. Each assessment set is composed of the two rows after
#' # the current row.
#' sliding_window(df, lookback = 2, assess_stop = 2)
#'
#' # Same as before, but step forward by 3 rows between each resampling slice,
#' # rather than just by 1.
#' rset <- sliding_window(df, lookback = 2, assess_stop = 2, step = 3)
#' rset
#'
#' analysis(rset$splits[[1]])
#' analysis(rset$splits[[2]])
#'
#' # Now slide relative to the `index` column in `df`. This time we look back
#' # 2 days from the current row's `index` value, and 2 days forward from
#' # it to construct the assessment set. Note that this series is irregular,
#' # so it produces different results than `sliding_window()`. Additionally,
#' # note that it is entirely possible for the assessment set to contain no
#' # data if you have a highly irregular series and "look forward" into a
#' # date range where no data points actually exist!
#' sliding_index(df, index, lookback = 2, assess_stop = 2)
#'
#' # With `sliding_period()`, we can break up our date index into less granular
#' # chunks, and slide over them instead of the index directly. Here we'll use
#' # the Chicago data, which contains daily data spanning 16 years, and we'll
#' # break it up into rolling yearly chunks. Three years worth of data will
#' # be used for the analysis set, and one years worth of data will be held out
#' # for performance assessment.
#' sliding_period(
#'   Chicago,
#'   date,
#'   "year",
#'   lookback = 2,
#'   assess_stop = 1
#' )
#'
#' # Because `lookback = 2`, three years are required to form a "complete"
#' # window of data. To allow partial windows, set `complete = FALSE`.
#' # Here that first constructs two expanding windows until a complete three
#' # year window can be formed, at which point we switch to a sliding window.
#' sliding_period(
#'   Chicago,
#'   date,
#'   "year",
#'   lookback = 2,
#'   assess_stop = 1,
#'   complete = FALSE
#' )
#'
#' # Alternatively, you could break the resamples up by month. Here we'll
#' # use an expanding monthly window by setting `lookback = Inf`, and each
#' # assessment set will contain two months of data. To ensure that we have
#' # enough data to fit our models, we'll `skip` the first 4 expanding windows.
#' # Finally, to thin out the results, we'll `step` forward by 2 between
#' # each resample.
#' sliding_period(
#'   Chicago,
#'   date,
#'   "month",
#'   lookback = Inf,
#'   assess_stop = 2,
#'   skip = 4,
#'   step = 2
#' )
NULL

#' @export
#' @rdname slide-resampling
sliding_window <- function(data,
                           ...,
                           lookback = 0L,
                           assess_start = 1L,
                           assess_stop = 1L,
                           complete = TRUE,
                           step = 1L,
                           skip = 0L) {
  rlang::check_dots_empty()

  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.")
  }

  lookback <- check_lookback(lookback)
  assess_start <- check_assess(assess_start, "assess_start")
  assess_stop <- check_assess(assess_stop, "assess_stop")
  step <- check_step(step)
  skip <- check_skip(skip)

  if (assess_start > assess_stop) {
    cli_abort("{.arg assess_start} must be less than or equal to {.arg assess_stop}.")
  }

  seq <- vctrs::vec_seq_along(data)

  id_in <- slider::slide(
    .x = seq,
    .f = identity,
    .before = lookback,
    .after = 0L,
    .step = 1L,
    .complete = complete
  )

  id_out <- slider::slide(
    .x = seq,
    .f = identity,
    .before = -assess_start,
    .after = assess_stop,
    .step = 1L,
    .complete = TRUE
  )

  indices <- compute_complete_indices(id_in, id_out)

  if (!identical(skip, 0L)) {
    indices <- slice_skip(indices, skip)
  }

  if (!identical(step, 1L)) {
    indices <- slice_step(indices, step)
  }

  splits <- purrr::map(
    indices,
    ~ make_splits(.x, data = data, class = "sliding_window_split")
  )

  ids <- names0(length(indices), prefix = "Slice")

  attrib <- list(
    lookback = lookback,
    assess_start = assess_start,
    assess_stop = assess_stop,
    complete = complete,
    step = step,
    skip = skip
  )

  new_rset(
    splits = splits,
    ids = ids,
    attrib = attrib,
    subclass = c("sliding_window", "rset")
  )
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname slide-resampling
sliding_index <- function(data,
                          index,
                          ...,
                          lookback = 0L,
                          assess_start = 1L,
                          assess_stop = 1L,
                          complete = TRUE,
                          step = 1L,
                          skip = 0L) {
  rlang::check_dots_empty()

  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.")
  }

  step <- check_step(step)
  skip <- check_skip(skip)

  index <- rlang::enexpr(index)
  loc <- tidyselect::eval_select(index, data)

  if (length(loc) != 1L) {
    cli_abort("{.arg index} must specify exactly one column in {.arg data}.")
  }

  index_attrib <- index
  index <- data[[loc]]

  seq <- vctrs::vec_seq_along(data)

  id_in <- slider::slide_index(
    .x = seq,
    .i = index,
    .f = identity,
    .before = lookback,
    .after = 0L,
    .complete = complete
  )

  id_out <- slider::slide_index(
    .x = seq,
    .i = index,
    .f = identity,
    .before = -assess_start,
    .after = assess_stop,
    .complete = TRUE
  )

  indices <- compute_complete_indices(id_in, id_out)

  if (!identical(skip, 0L)) {
    indices <- slice_skip(indices, skip)
  }

  if (!identical(step, 1L)) {
    indices <- slice_step(indices, step)
  }

  splits <- purrr::map(
    indices,
    ~ make_splits(.x, data = data, class = "sliding_index_split")
  )

  ids <- names0(length(indices), prefix = "Slice")

  attrib <- list(
    index = index_attrib,
    lookback = lookback,
    assess_start = assess_start,
    assess_stop = assess_stop,
    complete = complete,
    step = step,
    skip = skip
  )

  new_rset(
    splits = splits,
    ids = ids,
    attrib = attrib,
    subclass = c("sliding_index", "rset")
  )
}

# ------------------------------------------------------------------------------

#' @export
#' @rdname slide-resampling
sliding_period <- function(data,
                           index,
                           period,
                           ...,
                           lookback = 0L,
                           assess_start = 1L,
                           assess_stop = 1L,
                           complete = TRUE,
                           step = 1L,
                           skip = 0L,
                           every = 1L,
                           origin = NULL) {
  rlang::check_dots_empty()

  if (!is.data.frame(data)) {
    cli_abort("{.arg data} must be a data frame.")
  }

  lookback <- check_lookback(lookback)
  assess_start <- check_assess(assess_start, "assess_start")
  assess_stop <- check_assess(assess_stop, "assess_stop")
  step <- check_step(step)

  if (assess_start > assess_stop) {
    cli_abort("{.arg assess_start} must be less than or equal to {.arg assess_stop}.")
  }

  index <- rlang::enexpr(index)
  loc <- tidyselect::eval_select(index, data)

  if (length(loc) != 1L) {
    cli_abort("{.arg index} must specify exactly one column in {.arg data}.")
  }

  index_attrib <- index
  index <- data[[loc]]

  seq <- vctrs::vec_seq_along(data)

  id_in <- slider::slide_period(
    .x = seq,
    .i = index,
    .period = period,
    .f = identity,
    .every = every,
    .origin = origin,
    .before = lookback,
    .after = 0L,
    .complete = complete
  )

  id_out <- slider::slide_period(
    .x = seq,
    .i = index,
    .period = period,
    .f = identity,
    .every = every,
    .origin = origin,
    .before = -assess_start,
    .after = assess_stop,
    .complete = TRUE
  )

  indices <- compute_complete_indices(id_in, id_out)

  if (!identical(skip, 0L)) {
    indices <- slice_skip(indices, skip)
  }

  if (!identical(step, 1L)) {
    indices <- slice_step(indices, step)
  }

  splits <- purrr::map(
    indices,
    ~ make_splits(.x, data = data, class = "sliding_period_split")
  )

  ids <- names0(length(indices), prefix = "Slice")

  attrib <- list(
    index = index_attrib,
    period = period,
    lookback = lookback,
    assess_start = assess_start,
    assess_stop = assess_stop,
    complete = complete,
    step = step,
    skip = skip,
    every = every,
    origin = origin
  )

  new_rset(
    splits = splits,
    ids = ids,
    attrib = attrib,
    subclass = c("sliding_period", "rset")
  )
}

# ------------------------------------------------------------------------------

check_lookback <- function(x) {
  if (vctrs::vec_size(x) != 1L) {
    cli_abort("{.arg lookback} must have size 1.")
  }

  if (identical(x, Inf)) {
    return(x)
  }

  if (!rlang::is_integerish(x, finite = TRUE)) {
    cli_abort("{.arg lookback} must be an integer of size 1, or {.arg Inf}.")
  }

  if (x < 0L) {
    cli_abort("{.arg lookback} must be positive, or zero.")
  }

  vctrs::vec_cast(x, integer(), x_arg = "lookback")
}

check_assess <- function(x, arg) {
  if (vctrs::vec_size(x) != 1L) {
    cli_abort("{.arg {arg}} must have size 1.")
  }

  if (identical(x, Inf)) {
    return(x)
  }

  if (!rlang::is_integerish(x, finite = TRUE)) {
    cli_abort("{.arg {arg}} must be an integer of size 1, or {.code Inf}.")
  }

  if (x <= 0L) {
    cli_abort("{.arg {arg}} must be positive.")
  }

  vctrs::vec_cast(x, integer(), x_arg = arg)
}

check_step <- function(x) {
  if (vctrs::vec_size(x) != 1L) {
    cli_abort("{.arg step} must have size 1.")
  }

  if (!rlang::is_integerish(x, finite = TRUE)) {
    cli_abort("{.arg step} must be an integer of size 1.")
  }

  if (x <= 0L) {
    cli_abort("{.arg step} must be positive.")
  }

  vctrs::vec_cast(x, integer(), x_arg = "step")
}

check_skip <- function(x) {
  if (vctrs::vec_size(x) != 1L) {
    cli_abort("{.arg skip} must have size 1.")
  }

  if (!rlang::is_integerish(x, finite = TRUE)) {
    cli_abort("{.arg skip}` must be an integer of size 1.")
  }

  if (x < 0L) {
    cli_abort("{.arg skip} must be positive, or zero.")
  }

  vctrs::vec_cast(x, integer(), x_arg = "skip")
}

compute_complete_indices <- function(id_in, id_out) {
  # Remove where either list has a `NULL` element.
  # These are incomplete windows.
  id_in_na <- vctrs::vec_detect_missing(id_in)
  id_out_na <- vctrs::vec_detect_missing(id_out)

  id_either_na <- id_in_na | id_out_na

  id_in <- vctrs::vec_slice(id_in, !id_either_na)
  id_out <- vctrs::vec_slice(id_out, !id_either_na)

  purrr::map2(id_in, id_out, merge_lists)
}

slice_skip <- function(indices, skip) {
  n_indices <- length(indices)
  slicer <- rlang::seq2(skip + 1L, n_indices)
  vctrs::vec_slice(indices, slicer)
}

slice_step <- function(indices, step) {
  n_indices <- length(indices)
  slicer <- seq2_by(1L, n_indices, by = step)
  indices <- vctrs::vec_slice(indices, slicer)
}

seq2_by <- function(from, to, by) {
  if (length(from) != 1) {
    cli_abort("{.arg from} must be length one")
  }
  if (length(to) != 1) {
    cli_abort("{.arg to} must be length one")
  }

  by <- as.integer(by)

  if (length(by) != 1) {
    cli_abort("{.arg by} must be length one")
  }
  if (by <= 0L) {
    cli_abort("{.arg by} must be positive")
  }

  if (from > to) {
    integer()
  } else {
    seq.int(from, to, by = by)
  }
}
