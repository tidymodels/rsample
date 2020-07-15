# ------------------------------------------------------------------------------
# `sliding_window()`

test_that("defaults work", {
  df <- data.frame(x = 1:3)
  x <- sliding_window(df)

  split1 <- x[["splits"]][[1]]
  split2 <- x[["splits"]][[2]]

  expect_identical(split1[["in_id"]], 1L)
  expect_identical(split1[["out_id"]], 2L)

  expect_identical(split2[["in_id"]], 2L)
  expect_identical(split2[["out_id"]], 3L)
})

test_that("lookback always uses complete windows", {
  df <- data.frame(x = 1:4)
  x <- sliding_window(df, lookback = 1)

  split1 <- x[["splits"]][[1]]
  split2 <- x[["splits"]][[2]]

  expect_identical(split1[["in_id"]], 1:2)
  expect_identical(split1[["out_id"]], 3L)

  expect_identical(split2[["in_id"]], 2:3)
  expect_identical(split2[["out_id"]], 4L)
})

test_that("can step forward between slices", {
  df <- data.frame(x = 1:6)
  x <- sliding_window(df, lookback = 1, step = 2)

  split1 <- x[["splits"]][[1]]
  split2 <- x[["splits"]][[2]]

  expect_identical(split1[["in_id"]], 1:2)
  expect_identical(split1[["out_id"]], 3L)

  expect_identical(split2[["in_id"]], 3:4)
  expect_identical(split2[["out_id"]], 5L)
})

test_that("can generate assessment slices", {
  df <- data.frame(x = 1:4)
  x <- sliding_window(df, assess_stop = 2)

  split1 <- x[["splits"]][[1]]
  split2 <- x[["splits"]][[2]]

  expect_identical(split1[["in_id"]], 1L)
  expect_identical(split1[["out_id"]], 2:3)

  expect_identical(split2[["in_id"]], 2L)
  expect_identical(split2[["out_id"]], 3:4)

  expect_identical(nrow(x), 2L)
})

test_that("can add analysis / assessment gaps", {
  df <- data.frame(x = 1:7)
  x <- sliding_window(df, lookback = 1, assess_start = 3, assess_stop = 4)

  split1 <- x[["splits"]][[1]]
  split2 <- x[["splits"]][[2]]

  expect_identical(split1[["in_id"]], 1:2)
  expect_identical(split1[["out_id"]], 5:6)

  expect_identical(split2[["in_id"]], 2:3)
  expect_identical(split2[["out_id"]], 6:7)

  expect_identical(nrow(x), 2L)
})

test_that("`data` is validated", {
  expect_error(sliding_window(1), "`data` must be a data frame")
})

test_that("`lookback` is validated", {
  expect_error(sliding_window(data.frame(), lookback = -1), "`lookback` must be positive, or zero")
  expect_error(sliding_window(data.frame(), lookback = "a"), "`lookback` must be an integer")
  expect_error(sliding_window(data.frame(), lookback = c(1, 2)), "`lookback` must have size 1")
  expect_error(sliding_window(data.frame(), lookback = NA), "`lookback` must be an integer")
})

test_that("`assess_start` is validated", {
  expect_error(sliding_window(data.frame(), assess_start = -1), "`assess_start` must be positive")
  expect_error(sliding_window(data.frame(), assess_start = "a"), "`assess_start` must be an integer")
  expect_error(sliding_window(data.frame(), assess_start = c(1, 2)), "`assess_start` must have size 1")
  expect_error(sliding_window(data.frame(), assess_start = NA), "`assess_start` must be an integer")
})

test_that("`assess_stop` is validated", {
  expect_error(sliding_window(data.frame(), assess_stop = -1), "`assess_stop` must be positive")
  expect_error(sliding_window(data.frame(), assess_stop = "a"), "`assess_stop` must be an integer")
  expect_error(sliding_window(data.frame(), assess_stop = c(1, 2)), "`assess_stop` must have size 1")
  expect_error(sliding_window(data.frame(), assess_stop = NA), "`assess_stop` must be an integer")
})

test_that("`assess_start` must be before or equal to `assess_stop`", {
  expect_error(sliding_window(data.frame(), assess_start = 2, assess_stop = 1), "less than or equal to")
})

# ------------------------------------------------------------------------------
# `sliding_index()`

test_that("`data` is validated", {
  expect_error(sliding_index(1), "`data` must be a data frame")
})

test_that("`index` is validated", {
  df <- data.frame(x = 1:2)
  expect_error(sliding_index(df, y))
})
