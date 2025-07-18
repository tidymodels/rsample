# mc ---------------------------------------------------------------------

test_that("mc_split", {
  set.seed(11)
  r_set <- mc_cv(warpbreaks)
  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_identical(
    isplit$data,
    analysis(r_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, ],
    ignore_attr = "row.names"
  )
})

test_that("group_mc_split", {
  skip_if_not_installed("modeldata")

  data(ames, package = "modeldata", envir = rlang::current_env())

  set.seed(11)
  r_set <- group_mc_cv(ames, "MS_SubClass")
  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_identical(
    isplit$data,
    analysis(r_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, ],
    ignore_attr = "row.names"
  )
})


# vfold ------------------------------------------------------------------

test_that("vfold_split", {
  set.seed(11)
  r_set <- vfold_cv(warpbreaks, v = 5)
  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_identical(
    isplit$data,
    analysis(r_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, ],
    ignore_attr = "row.names"
  )
})

test_that("group_vfold_split", {
  skip_if_not_installed("modeldata")

  data(ames, package = "modeldata", envir = rlang::current_env())

  set.seed(11)
  r_set <- group_vfold_cv(ames, "MS_SubClass")
  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_identical(
    isplit$data,
    analysis(r_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, ],
    ignore_attr = "row.names"
  )
})

# bootstrap --------------------------------------------------------------

test_that("boot_split", {
  set.seed(11)
  r_set <- bootstraps(warpbreaks, times = 2)
  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_lte(
    nrow(isplit$data),
    analysis(r_split) |> nrow()
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[complement(isplit), ],
    ignore_attr = "row.names"
  )
})

test_that("group_boot_split", {
  skip_if_not_installed("modeldata")

  data(ames, package = "modeldata", envir = rlang::current_env())

  set.seed(11)
  r_set <- group_bootstraps(ames, group = "MS_SubClass", times = 2)
  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_lte(
    nrow(isplit$data),
    analysis(r_split) |> nrow()
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[complement(isplit), ],
    ignore_attr = "row.names"
  )
})


# validation set ---------------------------------------------------------

test_that("initial_validation_split", {
  set.seed(11)
  initial_vsplit <- initial_validation_split(
    warpbreaks,
    prop = c(0.6, 0.2)
  )
  r_set <- validation_set(initial_vsplit)
  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_identical(
    isplit$data,
    analysis(r_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, ],
    ignore_attr = "row.names"
  )
})

test_that("group_initial_validation_split", {
  skip_if_not_installed("modeldata")

  data(ames, package = "modeldata", envir = rlang::current_env())

  set.seed(11)
  initial_vsplit <- group_initial_validation_split(
    ames,
    group = "MS_SubClass",
    prop = c(0.7, 0.2)
  )
  r_set <- validation_set(initial_vsplit)
  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_identical(
    isplit$data,
    analysis(r_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, ],
    ignore_attr = "row.names"
  )
})

test_that("initial_validation_time_split", {
  set.seed(11)
  initial_vsplit <- initial_validation_time_split(
    warpbreaks,
    prop = c(0.6, 0.2)
  )
  r_set <- validation_set(initial_vsplit)
  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_identical(
    isplit$data,
    analysis(r_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, ],
    ignore_attr = "row.names"
  )
})


# clustering -------------------------------------------------------------

test_that("clustering_split", {
  set.seed(11)
  r_set <- clustering_cv(warpbreaks, vars = breaks, v = 5)
  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_identical(
    isplit$data,
    analysis(r_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[-isplit$in_id, ],
    ignore_attr = "row.names"
  )
})

# apparent ---------------------------------------------------------------

test_that("apparent_split", {
  set.seed(11)
  r_set <- apparent(warpbreaks)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split)

  expect_identical(
    isplit$data,
    analysis(r_split)
  )

  expect_identical(
    analysis(isplit),
    analysis(r_split)
  )
  expect_identical(
    assessment(isplit),
    analysis(r_split)
  )
})


# slide ------------------------------------------------------------------

test_that("sliding_window_split", {
  df <- data.frame(x = 1:10)
  r_set <- sliding_window(df, lookback = 4, assess_start = 3, assess_stop = 5)

  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_identical(
    isplit$in_id,
    1:3
  )
  expect_identical(
    isplit$out_id,
    4:5
  )

  expect_identical(
    isplit$data,
    analysis(r_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, , drop = FALSE],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, , drop = FALSE],
    ignore_attr = "row.names"
  )
})

test_that("sliding_window_split needs at least 2 observations", {
  df <- data.frame(x = 1:10)
  r_set <- sliding_window(df, lookback = 0)

  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  expect_snapshot(error = TRUE, {
    inner_split(r_split, split_args)
  })
})

test_that("sliding_window_split with incomplete sets", {
  df <- data.frame(x = 1:10)
  r_set <- sliding_window(
    df,
    lookback = 4,
    assess_start = 3,
    assess_stop = 5,
    complete = FALSE
  )

  split_args <- .get_split_args(r_set)

  # not enough observations for a full calibration set and (inner) analysis set
  r_split <- get_rsplit(r_set, 2)
  expect_snapshot(error = TRUE, {
    inner_split(r_split, split_args)
  })

  r_split <- get_rsplit(r_set, 3)
  isplit <- inner_split(r_split, split_args)

  expect_identical(
    isplit$in_id,
    1L
  )
  expect_identical(
    isplit$out_id,
    2:3
  )

  expect_identical(
    isplit$data,
    analysis(r_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, , drop = FALSE],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, , drop = FALSE],
    ignore_attr = "row.names"
  )
})

test_that("sliding_index_split", {
  df <- data.frame(x = 1:10)
  r_set <- sliding_index(df, x, lookback = 4, assess_start = 3, assess_stop = 5)

  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_identical(
    isplit$in_id,
    1:3
  )
  expect_identical(
    isplit$out_id,
    4:5
  )

  expect_identical(
    isplit$data,
    analysis(r_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, , drop = FALSE],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, , drop = FALSE],
    ignore_attr = "row.names"
  )
})

test_that("sliding_index_split can lookback over irregular index", {
  df <- data.frame(x = c(1, 3, 4, 5, 6:10))
  r_set <- sliding_index(df, x, lookback = 4, assess_start = 3, assess_stop = 5)

  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_identical(
    isplit$in_id,
    1:2
  )
  expect_identical(
    isplit$out_id,
    3:4
  )
})

test_that("sliding_index_split can make calibration set relative to irregular index", {
  df <- data.frame(x = c(1:7, 9:20))
  r_set <- sliding_index(
    df,
    x,
    lookback = 9,
    assess_start = 3,
    assess_stop = 10
  )

  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_identical(
    isplit$in_id,
    1:5
  )
  expect_identical(
    isplit$out_id,
    7:9
  )
})

test_that("sliding_index_split needs at least 2 observations", {
  df <- data.frame(x = 1:10)
  r_set <- sliding_index(df, x, lookback = 0)

  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  expect_snapshot(error = TRUE, {
    inner_split(r_split, split_args)
  })
})

test_that("sliding_index_split with incomplete sets", {
  df <- data.frame(x = 1:10)
  r_set <- sliding_index(
    df,
    x,
    lookback = 4,
    assess_start = 3,
    assess_stop = 5,
    complete = FALSE
  )

  split_args <- .get_split_args(r_set)

  # not enough observations for a full calibration set and (inner) analysis set
  r_split <- get_rsplit(r_set, 2)
  expect_snapshot(error = TRUE, {
    inner_split(r_split, split_args)
  })

  r_split <- get_rsplit(r_set, 3)
  isplit <- inner_split(r_split, split_args)

  expect_identical(
    isplit$in_id,
    1L
  )
  expect_identical(
    isplit$out_id,
    2:3
  )

  expect_identical(
    isplit$data,
    analysis(r_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, , drop = FALSE],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, , drop = FALSE],
    ignore_attr = "row.names"
  )
})

test_that("sliding_period_split", {
  index <- vctrs::new_date(c(-18, -14, -2, 0, 1, 3:10, 31))
  df <- data.frame(index = index)

  r_set <- sliding_period(df, index, period = "month", lookback = 1)
  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  i_split <- inner_split(r_split, split_args)

  expect_identical(
    i_split$in_id,
    1:3
  )
  expect_identical(
    i_split$out_id,
    4:13
  )

  expect_identical(
    i_split$data,
    analysis(r_split)
  )

  expect_identical(
    analysis(i_split),
    i_split$data[i_split$in_id, , drop = FALSE],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(i_split),
    i_split$data[i_split$out_id, , drop = FALSE],
    ignore_attr = "row.names"
  )
})

test_that("sliding_period_split when looking back over multiple periods, only complete ones are used", {
  index <- vctrs::new_date(c(-32, -1, 0, 1, 31))
  df <- data.frame(index = index)

  r_set <- sliding_period(df, index, period = "month", lookback = 1)

  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  i_split <- inner_split(r_split, split_args)

  expect_identical(i_split$in_id, 1L)
  expect_identical(i_split$out_id, 2L)
})

# initial split ----------------------------------------------------------

test_that("initial_split", {
  set.seed(11)
  car_split <- initial_split(mtcars)
  isplit <- inner_split(car_split, .get_split_args(car_split))

  expect_identical(
    isplit$data,
    training(car_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, ],
    ignore_attr = "row.names"
  )
})

test_that("group_initial_split", {
  set.seed(11)
  car_split <- group_initial_split(mtcars, cyl)
  isplit <- inner_split(car_split, .get_split_args(car_split))

  expect_identical(
    isplit$data,
    training(car_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, ],
    ignore_attr = "row.names"
  )
})

test_that("initial_time_split", {
  set.seed(11)
  car_split <- initial_time_split(mtcars)
  isplit <- inner_split(car_split, .get_split_args(car_split))

  expect_identical(
    isplit$data,
    training(car_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, ],
    ignore_attr = "row.names"
  )
})


# initial validation split -----------------------------------------------

test_that("initial_validation_split", {
  set.seed(11)
  car_split <- initial_validation_split(mtcars)
  isplit <- inner_split(car_split, .get_split_args(car_split))

  expect_identical(
    isplit$data,
    training(car_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, ],
    ignore_attr = "row.names"
  )
})

test_that("group_initial_validation_split", {
  skip_if_not_installed("modeldata")
  data(ames, package = "modeldata", envir = rlang::current_env())

  set.seed(12)
  ames_split <- group_initial_validation_split(ames, group = Neighborhood)
  isplit <- inner_split(ames_split, .get_split_args(ames_split))

  expect_identical(
    isplit$data,
    training(ames_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, ],
    ignore_attr = "row.names"
  )
})

test_that("initial_validation_time_split", {
  skip_if_not_installed("modeldata")
  data(drinks, package = "modeldata", envir = rlang::current_env())

  set.seed(12)
  drinks_split <- initial_validation_time_split(drinks)
  isplit <- inner_split(drinks_split, .get_split_args(drinks_split))

  expect_identical(
    isplit$data,
    training(drinks_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, ],
    ignore_attr = "row.names"
  )
})
