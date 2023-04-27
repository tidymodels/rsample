#' Simple Training/Test Set Splitting
#'
#' `initial_split` creates a single binary split of the data into a training
#'  set and testing set. `initial_time_split` does the same, but takes the
#'  _first_ `prop` samples for training, instead of a random selection.
#'  `group_initial_split` creates splits of the data based
#'  on some grouping variable, so that all data in a "group" is assigned to
#'  the same split.
#'  `training` and `testing` are used to extract the resulting data.
#' @template strata_details
#' @inheritParams vfold_cv
#' @inheritParams make_strata
#' @param prop The proportion of data to be retained for modeling/analysis.
#' @export
#' @return An `rsplit` object that can be used with the `training` and `testing`
#'  functions to extract the data in each split.
#' @examplesIf rlang::is_installed("modeldata")
#' set.seed(1353)
#' car_split <- initial_split(mtcars)
#' train_data <- training(car_split)
#' test_data <- testing(car_split)
#'
#' data(drinks, package = "modeldata")
#' drinks_split <- initial_time_split(drinks)
#' train_data <- training(drinks_split)
#' test_data <- testing(drinks_split)
#' c(max(train_data$date), min(test_data$date)) # no lag
#'
#' # With 12 period lag
#' drinks_lag_split <- initial_time_split(drinks, lag = 12)
#' train_data <- training(drinks_lag_split)
#' test_data <- testing(drinks_lag_split)
#' c(max(train_data$date), min(test_data$date)) # 12 period lag
#'
#' set.seed(1353)
#' car_split <- group_initial_split(mtcars, cyl)
#' train_data <- training(car_split)
#' test_data <- testing(car_split)
#'
#' @export
#'
initial_split <- function(data, prop = 3 / 4,
                          strata = NULL, breaks = 4, pool = 0.1, ...) {
  check_dots_empty()
  res <-
    mc_cv(
      data = data,
      prop = prop,
      strata = {{ strata }},
      breaks = breaks,
      pool = pool,
      times = 1
    )
  res <- res$splits[[1]]
  class(res) <- c("initial_split", class(res))
  res
}

#' @rdname initial_split
#' @param lag A value to include a lag between the assessment
#'  and analysis set. This is useful if lagged predictors will be used
#'  during training and testing.
#' @export
initial_time_split <- function(data, prop = 3 / 4, lag = 0, ...) {
  check_dots_empty()
  if (!is.numeric(prop) | prop >= 1 | prop <= 0) {
    rlang::abort("`prop` must be a number on (0, 1).")
  }

  if (!is.numeric(lag) | !(lag %% 1 == 0)) {
    rlang::abort("`lag` must be a whole number.")
  }

  n_train <- floor(nrow(data) * prop)

  if (lag > n_train) {
    rlang::abort("`lag` must be less than or equal to the number of training observations.")
  }

  split <- rsplit(data, 1:n_train, (n_train + 1 - lag):nrow(data))
  splits <- list(split)
  ids <- "Resample1"
  rset <- new_rset(splits, ids)

  res <- rset$splits[[1]]
  class(res) <- c("initial_time_split", "initial_split", class(res))
  res
}

#' @rdname initial_split
#' @export
#' @param x An `rsplit` object produced by `initial_split()` or
#'  `initial_time_split()`.
training <- function(x, ...) {
  UseMethod("training")
}

#' @export
#' @rdname initial_split
training.default <- function(x, ...) {
  cls <- class(x)
  cli::cli_abort(
    "No method for objects of class{?es}: {cls}"
  )
}

#' @rdname initial_split
#' @export
training.rsplit <- function(x, ...) {
  analysis(x, ...)
}

#' @rdname initial_split
#' @export
testing <- function(x, ...) {
  UseMethod("testing")
}

#' @export
#' @rdname initial_split
testing.default <- function(x, ...) {
  cls <- class(x)
  cli::cli_abort(
    "No method for objects of class{?es}: {cls}"
  )
}

#' @rdname initial_split
#' @export
testing.rsplit <- function(x, ...) {
  assessment(x, ...)
}

#' @inheritParams make_groups
#' @rdname initial_split
#' @export
group_initial_split <- function(data, group, prop = 3 / 4, ..., strata = NULL, pool = 0.1) {
  check_dots_empty()

  if (missing(strata)) {
    res <- group_mc_cv(
        data = data,
        group = {{ group }},
        prop = prop,
        times = 1
      )
  } else {
    res <- group_mc_cv(
        data = data,
        group = {{ group }},
        prop = prop,
        times = 1,
        strata = {{ strata }},
        pool = pool
      )
  }
  res <- res$splits[[1]]
  class(res) <- c("group_initial_split", "initial_split", class(res))
  res
}
