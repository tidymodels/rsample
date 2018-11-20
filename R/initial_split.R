#' Simple Training/Test Set Splitting
#'
#' `initial_split` creates a single binary split of the data into a training set
#' and testing set. `initial_time_split` does the same, but takes the _first_ `prop`
#' samples for training, instead of a random selection. `training` and
#' `testing` are used to extract the resulting data.
#'
#' @details
#' The `strata` argument causes the random sampling to be conducted *within the stratification variable*. The can help ensure that the number of data points in the training data is equivalent to the proportions in the original data set.
#'
#' @inheritParams vfold_cv
#' @param prop The proportion of data to be retained for modeling/analysis.
#' @param strata A variable that is used to conduct stratified sampling to create the resamples.
#' @export
#' @return  An `rset` object that can be used with the `training` and `testing` functions to extract the data in each split.
#' @examples
#' set.seed(1353)
#' car_split <- initial_split(mtcars)
#' train_data <- training(car_split)
#' test_data <- testing(car_split)
#'
#' drinks_split <- initial_time_split(drinks)
#' train_data <- training(drinks_split)
#' test_data <- testing(car_split)
#' c(max(train_data$date), min(test_data$date))  # no overlap
#' @export
#'
initial_split <- function(data, prop = 3/4, strata = NULL, ...) {
  res <-
    mc_cv(
      data = data,
      prop = prop,
      strata = strata,
      times = 1,
      ...
    )
  res$splits[[1]]
}

#' @rdname initial_split
#' @export
initial_time_split <- function(data, prop = 3/4, ...) {

  if (!is.numeric(prop) | prop >= 1 | prop <= 0) {
    stop("`prop` must be a number on (0, 1).", call. = FALSE)
  }

  n_train <- floor(nrow(data) * prop)

  rsplit(data, 1:n_train, (n_train + 1):nrow(data))
}

#' @rdname initial_split
#' @export
#' @param x An `rsplit` object produced by `initial_split`
training <- function(x) analysis(x)
#' @rdname initial_split
#' @export
testing <- function(x) assessment(x)
