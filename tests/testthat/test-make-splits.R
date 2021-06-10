test_that("can create a split with an empty assessment set (#188)", {
  df <- data.frame(x = c(1, 2, 3, 4))
  indices <- list(analysis = 1:4, assessment = integer())

  split <- make_splits(indices, df)

  expect_identical(split$out_id, integer())
  expect_identical(assessment(split), df[0, , drop = FALSE])
})

test_that("cannot create a split with an empty analysis set", {
  df <- data.frame(x = c(1, 2, 3, 4))
  indices <- list(analysis = integer(), assessment = 1:4)

  expect_error(make_splits(indices, df), "At least one row")
})

test_that("create a split from training and testing dataframes", {
  training <- data.frame(x = c(1, 2, 3, 4))
  testing <- data.frame(x = c(5, 6))

  split <- split_from_dataframes(training, testing)
  expect_identical(analysis(split), training)
  expect_identical(assessment(split), testing)
})

test_that("can create a split from empty testing dataframe", {
  training <- data.frame(x = c(1, 2, 3, 4))
  testing <- data.frame()

  split <- split_from_dataframes(training, testing)
  expect_identical(split$out_id, integer())
  expect_identical(assessment(split), training)
})

test_that("cannot create a split from empty training dataframe", {
  training <- data.frame(x = c(1, 2, 3, 4))
  testing <- data.frame()

  expect_error(
    split_from_dataframes(training, testing),
    "`training` must contain at least one row."
  )
})

test_that("cannot create a split from dataframes with different columns", {
  training <- data.frame(x = c(1, 2, 3, 4))
  testing <- data.frame(y = c(5, 6))

  except_error(split_from_dataframes(training, testing), "names do not match previous names")
})
