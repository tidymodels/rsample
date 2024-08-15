#' Create an Initial Train/Validation/Test Split
#'
#' `initial_validation_split()` creates a random three-way split of the data
#' into a training set, a validation set, and a testing set.
#' `initial_validation_time_split()` does the same, but instead of a random
#' selection the training, validation, and testing set are in order of the full
#' data set, with the first observations being put into the training set.
#' `group_initial_validation_split()` creates similar random splits of the data
#' based on some grouping variable, so that all data in a "group" are assigned
#' to the same partition.
#'
#' @details `training()`, `validation()`, and `testing()` can be used to extract the
#' resulting data sets.
#' Use [`validation_set()`] to create an `rset` object for use with functions from
#' the tune package such as `tune::tune_grid()`.
#'
#' @template strata_details
#'
#' @inheritParams vfold_cv
#' @inheritParams make_strata
#' @param prop A length-2 vector of proportions of data to be retained for training and
#' validation data, respectively.
#' @inheritParams rlang::args_dots_empty
#' @param x An object of class `initial_validation_split`.
#'
#' @return An `initial_validation_split` object that can be used with the
#' [training()], [validation()], and [testing()] functions to extract the data
#' in each split.
#'
#' @seealso [validation_set()]
#'
#' @export
#' @examplesIf rlang::is_installed("modeldata")
#' set.seed(1353)
#' car_split <- initial_validation_split(mtcars)
#' train_data <- training(car_split)
#' validation_data <- validation(car_split)
#' test_data <- testing(car_split)
#'
#' data(drinks, package = "modeldata")
#' drinks_split <- initial_validation_time_split(drinks)
#' train_data <- training(drinks_split)
#' validation_data <- validation(drinks_split)
#' c(max(train_data$date), min(validation_data$date))
#'
#' data(ames, package = "modeldata")
#' set.seed(1353)
#' ames_split <- group_initial_validation_split(ames, group = Neighborhood)
#' train_data <- training(ames_split)
#' validation_data <- validation(ames_split)
#' test_data <- testing(ames_split)
initial_validation_split <- function(data,
                                     prop = c(0.6, 0.2),
                                     strata = NULL,
                                     breaks = 4,
                                     pool = 0.1,
                                     ...) {
  rlang::check_dots_empty()

  check_prop_3(prop)
  prop_train <- prop[1]
  prop_val <- prop[2] / (1 - prop_train)


  if (!missing(strata)) {
    strata <- tidyselect::vars_select(names(data), !!enquo(strata))
    if (length(strata) == 0) {
      strata <- NULL
    }
  }
  strata_check(strata, data)

  split_train <- mc_cv(
      data = data,
      prop = prop_train,
      strata = {{ strata }},
      breaks = breaks,
      pool = pool,
      times = 1,
      ...
    )
  split_train <- split_train$splits[[1]]
  train_id <- split_train$in_id

  val_and_test <- assessment(split_train)
  val_and_test_id <- complement(split_train)

  split_val <- mc_cv(
    data = val_and_test,
    prop = prop_val,
    strata = {{ strata }},
    breaks = breaks,
    pool = pool / (1 - prop_train),
    times = 1,
    ...
  )
  split_val <- split_val$splits[[1]]
  val_id <- val_and_test_id[split_val$in_id]

  res <- list(
    data = data,
    train_id = train_id,
    val_id = val_id,
    test_id = NA,
    id = "split"
  )

  # include those so that they can be attached to the `rset` later in `validation_set()`
  if (!is.null(strata)) names(strata) <- NULL
  val_att <- list(
    prop = prop,
    strata = strata,
    breaks = breaks,
    pool = pool
  )
  attr(res, "val_att") <- val_att

  class(res) <- c("initial_validation_split", "three_way_split")
  res
}

check_prop_3 <- function(prop, call = rlang::caller_env()) {
  if (!is.numeric(prop)) {
    rlang::abort("`prop` needs to be numeric.", call = call)
  }
  if (any(is.na(prop))) {
    rlang::abort("`prop` cannot contain `NA`.", call = call)
  }
  if (any(is.null(prop))) {
    rlang::abort("`prop` cannot contain `NULL`.", call = call)
  }
  if (length(prop) != 2L) {
    rlang::abort(
      "`prop` needs to contain the proportions for training and validation.",
      call = call
    )
  }
  if (any(!(prop > 0)) | any(!(prop < 1))) {
    rlang::abort("Elements of `prop` need to be in (0, 1).", call = call)
  }
  if (!(sum(prop) > 0 ) | !(sum(prop) < 1) ) {
    rlang::abort(
      "The sum of the proportions in `prop` needs to be in (0, 1).",
      call = call
    )
  }
  invisible(prop)
}

#' @rdname initial_validation_split
#' @export
initial_validation_time_split <- function(data,
                                          prop = c(0.6, 0.2),
                                          ...) {
  rlang::check_dots_empty()

  check_prop_3(prop)
  prop_train <- prop[1]
  prop_val <- prop[2] / (1 - prop_train)

  n_train <- floor(nrow(data) * prop_train)
  n_val <- floor((nrow(data) - n_train) * prop_val)

  train_id <- seq(1, n_train, by = 1)
  val_id <- seq(n_train + 1, n_train + n_val, by = 1)

  res <- list(
    data = data,
    train_id = train_id,
    val_id = val_id,
    test_id = NA,
    id = "split"
  )

  # include those so that they can be attached to the `rset` later in `validation_set()`
  val_att <- list(
    prop = prop
  )
  attr(res, "val_att") <- val_att

  class(res) <- c(
    "initial_validation_time_split",
    "initial_validation_split",
    "three_way_split"
  )
  res
}

#' @inheritParams make_groups
#' @rdname initial_validation_split
#' @export
group_initial_validation_split <- function(data,
                                           group,
                                           prop = c(0.6, 0.2),
                                           ...,
                                           strata = NULL,
                                           pool = 0.1) {
  rlang::check_dots_empty()

  group <- validate_group({{ group }}, data)

  check_prop_3(prop)
  prop_train <- prop[1]
  prop_val <- prop[2] / (1 - prop_train)

  if (!missing(strata)) {
    strata <- tidyselect::vars_select(names(data), !!enquo(strata))
    if (length(strata) == 0) {
      strata <- NULL
    }
  }
  strata_check(strata, data)

  if (missing(strata)) {
    split_train <- group_mc_cv(
      data = data,
      group = {{ group }},
      prop = prop_train,
      times = 1
    )
  } else {
    split_train <- group_mc_cv(
      data = data,
      group = {{ group }},
      prop = prop_train,
      times = 1,
      strata = {{ strata }},
      pool = pool
    )
  }
  split_train <- split_train$splits[[1]]
  train_id <- split_train$in_id

  val_and_test <- assessment(split_train)
  val_and_test_id <- complement(split_train)

  if (missing(strata)) {
    split_val <- group_mc_cv(
      data = val_and_test,
      group = {{ group }},
      prop = prop_val,
      times = 1
    )
  } else {
    split_val <- group_mc_cv(
      data = val_and_test,
      group = {{ group }},
      prop = prop_val,
      times = 1,
      strata = {{ strata }},
      pool = pool
    )
  }
  split_val <- split_val$splits[[1]]
  val_id <- val_and_test_id[split_val$in_id]

  res <- list(
    data = data,
    train_id = train_id,
    val_id = val_id,
    test_id = NA,
    id = "split"
  )

  # include those so that they can be attached to the `rset` later in `validation_set()`
  if (!is.null(strata)) names(strata) <- NULL
  val_att <- list(
    group = group,
    prop = prop,
    strata = strata,
    pool = pool
  )
  attr(res, "val_att") <- val_att

  class(res) <- c("group_initial_validation_split",
                  "initial_validation_split",
                  "three_way_split")
  res
}

#' @export
#' @rdname initial_validation_split
training.initial_validation_split <- function(x, ...) {
  check_dots_empty()
  ind <- sort(x$train_id)
  vctrs::vec_slice(x$data, ind)
}

#' @export
#' @rdname initial_validation_split
testing.initial_validation_split <- function(x, ...) {
  check_dots_empty()
  ind <- -sort(c(x$train_id, x$val_id))
  vctrs::vec_slice(x$data, ind)
}

#' @export
#' @rdname initial_validation_split
validation <- function(x, ...) {
  UseMethod("validation")
}

#' @export
#' @rdname initial_validation_split
validation.default <- function(x, ...) {
  cls <- class(x)
  cli::cli_abort(
    "No method for objects of class{?es}: {cls}"
  )
}

#' @export
#' @rdname initial_validation_split
validation.initial_validation_split <- function(x, ...) {
  check_dots_empty()

  ind <- sort(x$val_id)
  vctrs::vec_slice(x$data, ind)
}


#' @export
#' @keywords internal
analysis.initial_validation_split <- function(x, ...) {
  rlang::abort(
    "The initial validation split does not contain an analysis set.",
    i = "You can access the training data with `training()`."
  )
}

#' @export
#' @keywords internal
assessment.initial_validation_split <- function(x, ...) {
  rlang::abort(
    "The initial validation split does not contain an assessment set.",
    i = "You can access the testing data with `testing()`."
  )
}

#' @export
print.initial_validation_split <- function(x, ...) {

  n_test <- nrow(x$data) - length(x$train_id) - length(x$val_id)
  n_test_char <- paste(n_test)

  cat("<Training/Validation/Testing/Total>\n")
  cat("<",
      length(x$train_id), "/",
      length(x$val_id), "/",
      n_test_char, "/",
      nrow(x$data), ">\n",
      sep = ""
  )
}

#' @export
dim.initial_validation_split <- function(x, ...) {
  n_test <- nrow(x$data) - length(x$train_id) - length(x$val_id)
  c(
    training = length(x$train_id),
    validation = length(x$val_id),
    testing = length(n_test),
    n = nrow(x$data),
    p = ncol(x$data)
  )
}
