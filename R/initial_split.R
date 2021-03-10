#' Simple Training/Test Set Splitting
#'
#' `initial_split` creates a single binary split of the data into a training
#'  set and testing set. `initial_time_split` does the same, but takes the
#'  _first_ `prop` samples for training, instead of a random selection.
#'  `training` and `testing` are used to extract the resulting data.
#' @details The `strata` argument causes the random sampling to be conducted
#'  *within the stratification variable*. This can help ensure that the number
#'  of data points in the training data is equivalent to the proportions in the
#'  original data set. (Strata below 10% of the total are pooled together.)
#' @inheritParams vfold_cv
#' @inheritParams make_strata
#' @param prop The proportion of data to be retained for modeling/analysis.
#' @param strata A variable that is used to conduct stratified sampling to
#'  create the resamples. This could be a single character value or a variable
#'  name that corresponds to a variable that exists in the data frame.
#' @export
#' @return An `rsplit` object that can be used with the `training` and `testing`
#'  functions to extract the data in each split.
#' @examples
#' set.seed(1353)
#' car_split <- initial_split(mtcars)
#' train_data <- training(car_split)
#' test_data <- testing(car_split)
#'
#' data(drinks, package = "modeldata")
#' drinks_split <- initial_time_split(drinks)
#' train_data <- training(drinks_split)
#' test_data <- testing(drinks_split)
#' c(max(train_data$date), min(test_data$date))  # no lag
#'
#' # With 12 period lag
#' drinks_lag_split <- initial_time_split(drinks, lag = 12)
#' train_data <- training(drinks_lag_split)
#' test_data <- testing(drinks_lag_split)
#' c(max(train_data$date), min(test_data$date))  # 12 period lag
#'
#' @export
#'
initial_split <- function(data, prop = 3/4,
                          strata = NULL, breaks = 4, pool = 0.1, ...) {

  if (!missing(strata)) {
    strata <- tidyselect::vars_select(names(data), !!enquo(strata))
    if (length(strata) == 0) {
      strata <- NULL
    }
  }

  res <-
    mc_cv(
      data = data,
      prop = prop,
      strata = strata,
      breaks = breaks,
      pool = pool,
      times = 1,
      ...
    )
  res$splits[[1]]
}

#' @rdname initial_split
#' @param lag A value to include a lag between the assessment
#'  and analysis set. This is useful if lagged predictors will be used
#'  during training and testing.
#' @export
initial_time_split <- function(data, prop = 3/4, lag = 0, ...) {

  if (!is.numeric(prop) | prop >= 1 | prop <= 0) {
    rlang::abort("`prop` must be a number on (0, 1).")
  }

  if (!is.numeric(lag) | !(lag%%1==0)) {
    stop("`lag` must be a whole number.", call. = FALSE)
  }

  n_train <- floor(nrow(data) * prop)

  if (lag > n_train) {
    stop("`lag` must be less than or equal to the number of training observations.", call. = FALSE)
  }

  split  <- rsplit(data, 1:n_train, (n_train + 1 - lag):nrow(data))
  splits <- list(split)
  ids    <- "Resample1"
  rset   <- new_rset(splits, ids)

  rset$splits[[1]]

}

#' @rdname initial_split
#' @export
#' @param x An `rsplit` object produced by `initial_split`
training <- function(x) analysis(x)
#' @rdname initial_split
#' @export
testing <- function(x) assessment(x)
