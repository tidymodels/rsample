#' Rolling Origin Forecast Resampling
#'
#' This resampling method is useful when the data set has a strong time
#'  component. The resamples are not random and contain data points that are
#'  consecutive values. The function assumes that the original data set are
#'  sorted in time order.
#' @details The main options, `initial` and `assess`, control the number of
#'  data points from the original data that are in the analysis and assessment
#'  set, respectively. When `cumulative = TRUE`, the analysis set will grow as
#'  resampling continues while the assessment set size will always remain
#'  static.
#' `skip` enables the function to not use every data point in the resamples.
#'  When `skip = 0`, the resampling data sets will increment by one position.
#'  Suppose that the rows of a data set are consecutive days. Using `skip = 6`
#'  will make the analysis data set to operate on *weeks* instead of days. The
#'  assessment set size is not affected by this option.
#' @seealso
#' [sliding_window()], [sliding_index()], and [sliding_period()] for additional
#' time based resampling functions.
#' @inheritParams vfold_cv
#' @param initial The number of samples used for analysis/modeling in the
#'  initial resample.
#' @param assess The number of samples used for each assessment resample.
#' @param cumulative A logical. Should the analysis resample grow beyond the
#'  size specified by `initial` at each resample?.
#' @param skip A integer indicating how many (if any) _additional_ resamples
#'  to skip to thin the total amount of data points in the analysis resample.
#' See the example below.
#' @param lag A value to include a lag between the assessment
#'  and analysis set. This is useful if lagged predictors will be used
#'  during training and testing.
#' @export
#' @return An tibble with classes `rolling_origin`, `rset`, `tbl_df`, `tbl`,
#'  and `data.frame`. The results include a column for the data split objects
#'  and a column called `id` that has a character string with the resample
#'  identifier.
#' @examplesIf rlang::is_installed("modeldata")
#' set.seed(1131)
#' ex_data <- data.frame(row = 1:20, some_var = rnorm(20))
#' dim(rolling_origin(ex_data))
#' dim(rolling_origin(ex_data, skip = 2))
#' dim(rolling_origin(ex_data, skip = 2, cumulative = FALSE))
#'
#' # You can also roll over calendar periods by first nesting by that period,
#' # which is especially useful for irregular series where a fixed window
#' # is not useful. This example slides over 5 years at a time.
#' library(dplyr)
#' library(tidyr)
#' data(drinks, package = "modeldata")
#'
#' drinks_annual <- drinks %>%
#'   mutate(year = as.POSIXlt(date)$year + 1900) %>%
#'   nest(data = c(-year))
#'
#' multi_year_roll <- rolling_origin(drinks_annual, cumulative = FALSE)
#'
#' analysis(multi_year_roll$splits[[1]])
#' assessment(multi_year_roll$splits[[1]])
#' @export
rolling_origin <- function(data, initial = 5, assess = 1,
                           cumulative = TRUE, skip = 0, lag = 0, ...) {
  n <- nrow(data)

  if (n < initial + assess) {
    rlang::abort(
      "There should be at least ", initial + assess, " rows in `data`."
    )
  }

  if (!is.numeric(lag) | !(lag %% 1 == 0)) {
    rlang::abort("`lag` must be a whole number.")
  }

  if (lag > initial) {
    rlang::abort(
      "`lag` must be less than or equal to the number of training observations."
    )
  }

  stops <- seq(initial, (n - assess), by = skip + 1)
  starts <- if (!cumulative) {
    stops - initial + 1
  } else {
    starts <- rep(1, length(stops))
  }

  in_ind <- mapply(seq, starts, stops, SIMPLIFY = FALSE)
  out_ind <-
    mapply(seq, stops + 1 - lag, stops + assess, SIMPLIFY = FALSE)
  indices <- mapply(merge_lists, in_ind, out_ind, SIMPLIFY = FALSE)
  split_objs <-
    purrr::map(indices, make_splits, data = data, class = "rof_split")
  split_objs <- list(
    splits = split_objs,
    id = names0(length(split_objs), "Slice")
  )

  roll_att <- list(
    initial = initial,
    assess = assess,
    cumulative = cumulative,
    skip = skip,
    lag = lag
  )

  new_rset(
    splits = split_objs$splits,
    ids = split_objs$id,
    attrib = roll_att,
    subclass = c("rolling_origin", "rset")
  )
}
