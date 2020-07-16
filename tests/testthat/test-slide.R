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

test_that("can create an expanding window", {
  df <- data.frame(x = 1:4)
  x <- sliding_window(df, lookback = Inf)

  split1 <- x[["splits"]][[1]]
  split2 <- x[["splits"]][[2]]
  split3 <- x[["splits"]][[3]]

  expect_identical(split1[["in_id"]], 1L)
  expect_identical(split1[["out_id"]], 2L)

  expect_identical(split2[["in_id"]], 1:2)
  expect_identical(split2[["out_id"]], 3L)

  expect_identical(split3[["in_id"]], 1:3)
  expect_identical(split3[["out_id"]], 4L)

  expect_identical(nrow(x), 3L)
})

test_that("can skip first few resampling slices", {
  df <- data.frame(x = 1:8)
  x <- sliding_window(df, lookback = 1, skip = 3)

  split1 <- x[["splits"]][[1]]

  expect_identical(split1[["in_id"]], 4:5)
  expect_identical(split1[["out_id"]], 6L)

  expect_identical(nrow(x), 3L)
})

test_that("`skip` is applied before `step`", {
  df <- data.frame(x = 1:8)
  x <- sliding_window(df, lookback = 1, skip = 3, step = 2)

  split1 <- x[["splits"]][[1]]
  split2 <- x[["splits"]][[2]]

  expect_identical(split1[["in_id"]], 4:5)
  expect_identical(split1[["out_id"]], 6L)

  expect_identical(split2[["in_id"]], 6:7)
  expect_identical(split2[["out_id"]], 8L)

  expect_identical(nrow(x), 2L)
})

test_that("can use incomplete windows at the beginning", {
  df <- data.frame(x = 1:5)
  x <- sliding_window(df, lookback = 2, complete = FALSE)

  split1 <- x[["splits"]][[1]]
  split2 <- x[["splits"]][[2]]
  split3 <- x[["splits"]][[3]]
  split4 <- x[["splits"]][[4]]

  expect_identical(split1[["in_id"]], 1L)
  expect_identical(split1[["out_id"]], 2L)

  expect_identical(split2[["in_id"]], 1:2)
  expect_identical(split2[["out_id"]], 3L)

  expect_identical(split3[["in_id"]], 1:3)
  expect_identical(split3[["out_id"]], 4L)

  expect_identical(split4[["in_id"]], 2:4)
  expect_identical(split4[["out_id"]], 5L)

  expect_identical(nrow(x), 4L)
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

test_that("defaults works", {
  df <- data.frame(x = 1:3)
  x <- sliding_index(df, x)

  split1 <- x$splits[[1]]
  split2 <- x$splits[[2]]

  expect_identical(split1$in_id, 1L)
  expect_identical(split1$out_id, 2L)

  expect_identical(split2$in_id, 2L)
  expect_identical(split2$out_id, 3L)

  expect_identical(nrow(x), 2L)
})

test_that("can lookback over irregular index", {
  df <- data.frame(x = c(1, 3, 4, 5))
  x <- sliding_index(df, x, lookback = 1)

  split1 <- x$splits[[1]]
  split2 <- x$splits[[2]]

  expect_identical(split1$in_id, 2L)
  expect_identical(split1$out_id, 3L)

  expect_identical(split2$in_id, 2:3)
  expect_identical(split2$out_id, 4L)

  expect_identical(nrow(x), 2L)
})

test_that("can compute assessment indices relative to irregular index", {
  df <- data.frame(x = c(1, 3, 4, 5, 7, 8))
  x <- sliding_index(df, x, lookback = 1, assess_stop = 2)

  split1 <- x$splits[[1]]
  split2 <- x$splits[[2]]
  split3 <- x$splits[[3]]

  expect_identical(split1$in_id, 2L)
  expect_identical(split1$out_id, 3:4)

  expect_identical(split2$in_id, 2:3)
  expect_identical(split2$out_id, 4L)

  expect_identical(split3$in_id, 3:4)
  expect_identical(split3$out_id, 5L)

  expect_identical(nrow(x), 3L)
})

test_that("it is possible to create empty assessment sets", {
  # Look forward 1->2 values from `5`, so creates a window with range of [6, 7].
  # But no `x` values fall in this range. However, in theory it is "possible"
  # to make a complete window starting at `5`, which is why `complete = TRUE`
  # didn't remove it.
  df <- data.frame(x = c(1, 3, 4, 5, 8, 9))
  x <- sliding_index(df, x, lookback = 1, assess_stop = 2)

  split3 <- x$splits[[3]]

  expect_identical(split3$in_id, 3:4)
  expect_identical(split3$out_id, integer())

  expect_identical(nrow(x), 3L)
})

test_that("can add a gap between the analysis and assessment set", {
  df <- data.frame(x = c(1, 3, 4, 5, 7, 8))
  x <- sliding_index(df, x, lookback = 2, assess_start = 2, assess_stop = 3)

  split1 <- x$splits[[1]]
  split2 <- x$splits[[2]]
  split3 <- x$splits[[3]]

  expect_identical(split1$in_id, 1:2)
  expect_identical(split1$out_id, 4L)

  expect_identical(split2$in_id, 2:3)
  expect_identical(split2$out_id, 5L)

  expect_identical(split3$in_id, 2:4)
  expect_identical(split3$out_id, 5:6)

  expect_identical(nrow(x), 3L)
})

test_that("can use `step` to thin results after calling `slide_index()`", {
  df <- data.frame(x = c(1, 3, 4, 6, 7, 10))
  x <- sliding_index(df, x, lookback = 2, assess_stop = 2, step = 2)

  split1 <- x$splits[[1]]
  split2 <- x$splits[[2]]

  expect_identical(split1$in_id, 1:2)
  expect_identical(split1$out_id, 3L)

  expect_identical(split2$in_id, 3:4)
  expect_identical(split2$out_id, 5L)

  expect_identical(nrow(x), 2L)
})

test_that("can skip first few resampling slices", {
  df <- data.frame(x = c(1, 3, 4, 6, 7, 10))
  x <- sliding_index(df, x, lookback = 1, skip = 2)

  split1 <- x[["splits"]][[1]]

  expect_identical(split1[["in_id"]], 4L)
  expect_identical(split1[["out_id"]], 5L)

  expect_identical(nrow(x), 2L)
})

test_that("`skip` is applied before `step`", {
  df <- data.frame(x = c(1, 3, 4, 6, 7, 9, 11, 13, 14))
  x <- sliding_index(df, x, lookback = 1, skip = 3, step = 2, assess_stop = 2)

  split1 <- x[["splits"]][[1]]
  split2 <- x[["splits"]][[2]]

  expect_identical(split1[["in_id"]], 4:5)
  expect_identical(split1[["out_id"]], 6L)

  expect_identical(split2[["in_id"]], 7L)
  expect_identical(split2[["out_id"]], 8L)

  expect_identical(nrow(x), 2L)
})

test_that("can use incomplete windows at the beginning", {
  df <- data.frame(x = c(1, 3, 4, 5, 7))
  x <- sliding_index(df, x, lookback = 2, complete = FALSE, assess_stop = 2)

  split1 <- x[["splits"]][[1]]
  split2 <- x[["splits"]][[2]]
  split3 <- x[["splits"]][[3]]
  split4 <- x[["splits"]][[4]]

  expect_identical(split1[["in_id"]], 1L)
  expect_identical(split1[["out_id"]], 2L)

  expect_identical(split2[["in_id"]], 1:2)
  expect_identical(split2[["out_id"]], 3:4)

  expect_identical(split3[["in_id"]], 2:3)
  expect_identical(split3[["out_id"]], 4L)

  expect_identical(split4[["in_id"]], 2:4)
  expect_identical(split4[["out_id"]], 5L)

  expect_identical(nrow(x), 4L)
})

test_that("`data` is validated", {
  expect_error(sliding_index(1), "`data` must be a data frame")
})

test_that("`index` is validated", {
  df <- data.frame(x = 1:2)
  expect_error(sliding_index(df, y))
})

# ------------------------------------------------------------------------------
# `sliding_period()`

test_that("can group by month", {
  index <- vctrs::new_date(c(-1, 0, 1, 31))
  df <- data.frame(index = index)

  x <- sliding_period(df, index, period = "month")

  split1 <- x$splits[[1]]
  split2 <- x$splits[[2]]

  expect_identical(split1$in_id, 1L)
  expect_identical(split1$out_id, 2:3)

  expect_identical(split2$in_id, 2:3)
  expect_identical(split2$out_id, 4L)

  expect_identical(nrow(x), 2L)
})

test_that("can group by year", {
  index <- vctrs::new_date(c(-1, 0, 1, 31))
  df <- data.frame(index = index)

  x <- sliding_period(df, index, period = "year")

  split1 <- x$splits[[1]]

  expect_identical(split1$in_id, 1L)
  expect_identical(split1$out_id, 2:4)

  expect_identical(nrow(x), 1L)
})

test_that("when looking back over multiple periods, only complete ones are used", {
  index <- vctrs::new_date(c(-32, -1, 0, 1, 31))
  df <- data.frame(index = index)

  x <- sliding_period(df, index, period = "month", lookback = 1)

  split1 <- x$splits[[1]]

  expect_identical(split1$in_id, 1:2)
  expect_identical(split1$out_id, 3:4)

  expect_identical(nrow(x), 2L)
})

test_that("can look forward to assess over multiple periods", {
  index <- vctrs::new_date(c(-32, -1, 0, 1, 31))
  df <- data.frame(index = index)

  x <- sliding_period(df, index, period = "month", assess_stop = 2)

  split1 <- x$splits[[1]]
  split2 <- x$splits[[2]]

  expect_identical(split1$in_id, 1L)
  expect_identical(split1$out_id, 2:4)

  expect_identical(split2$in_id, 2L)
  expect_identical(split2$out_id, 3:5)

  expect_identical(nrow(x), 2L)
})

test_that("can use `step` to thin results after calling `slide_period()`", {
  df <- data.frame(x = vctrs::new_date(c(1, 3, 4, 6, 7, 10)))
  x <- sliding_period(df, x, "day", lookback = 2, assess_stop = 2, step = 2)

  split1 <- x$splits[[1]]
  split2 <- x$splits[[2]]

  expect_identical(split1$in_id, 1:2)
  expect_identical(split1$out_id, 3L)

  expect_identical(split2$in_id, 3:4)
  expect_identical(split2$out_id, 5L)

  expect_identical(nrow(x), 2L)
})

test_that("can skip first few resampling slices", {
  index <- vctrs::new_date(c(-32, -1, 0, 1, 31, 59))
  df <- data.frame(index = index)

  x <- sliding_period(df, index, "month", lookback = 1, skip = 2)

  split1 <- x[["splits"]][[1]]

  expect_identical(split1[["in_id"]], 3:5)
  expect_identical(split1[["out_id"]], 6L)

  expect_identical(nrow(x), 1L)
})

test_that("can skip with expanding window", {
  index <- vctrs::new_date(c(-32, -1, 0, 1, 31, 59))
  df <- data.frame(index = index)

  x <- sliding_period(df, index, "month", lookback = Inf, skip = 2)

  split1 <- x[["splits"]][[1]]
  split2 <- x[["splits"]][[2]]

  expect_identical(split1[["in_id"]], 1:4)
  expect_identical(split1[["out_id"]], 5L)

  expect_identical(split2[["in_id"]], 1:5)
  expect_identical(split2[["out_id"]], 6L)

  expect_identical(nrow(x), 2L)
})

test_that("`skip` is applied before `step`", {
  index <- vctrs::new_date(c(-32, -1, 0, 1, 31, 59, 90))
  df <- data.frame(index = index)

  x <- sliding_period(df, index, "month", lookback = Inf, skip = 2, step = 2)

  split1 <- x[["splits"]][[1]]
  split2 <- x[["splits"]][[2]]

  expect_identical(split1[["in_id"]], 1:4)
  expect_identical(split1[["out_id"]], 5L)

  expect_identical(split2[["in_id"]], 1:6)
  expect_identical(split2[["out_id"]], 7L)

  expect_identical(nrow(x), 2L)
})

test_that("can use incomplete windows at the beginning", {
  index <- vctrs::new_date(c(-32, -1, 0, 1, 59, 90))
  df <- data.frame(index = index)

  x <- sliding_period(df, index, "month", lookback = 2, complete = FALSE)

  split1 <- x[["splits"]][[1]]
  split2 <- x[["splits"]][[2]]
  split3 <- x[["splits"]][[3]]
  split4 <- x[["splits"]][[4]]

  expect_identical(split1[["in_id"]], 1L)
  expect_identical(split1[["out_id"]], 2L)

  expect_identical(split2[["in_id"]], 1:2)
  expect_identical(split2[["out_id"]], 3:4)

  expect_identical(split3[["in_id"]], 1:4)
  expect_identical(split3[["out_id"]], integer())

  expect_identical(split4[["in_id"]], 3:5)
  expect_identical(split4[["out_id"]], 6L)

  expect_identical(nrow(x), 4L)
})

test_that("`data` is validated", {
  expect_error(sliding_period(1), "`data` must be a data frame")
})

test_that("`index` is validated", {
  df <- data.frame(x = 1:2)
  expect_error(sliding_period(df, y))
})
