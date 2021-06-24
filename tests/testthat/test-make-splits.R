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
  training <- tibble(x = c(1, 2, 3, 4))
  testing <- tibble(x = c(5, 6))

  split <- make_splits(training, testing)
  expect_identical(analysis(split), training)
  expect_identical(assessment(split), testing)
})

test_that("can create a split from empty testing dataframe", {
  training <- tibble(x = c(1, 2, 3, 4))
  testing <- tibble()

  split <- make_splits(training, testing)
  expect_identical(split$out_id, integer())
  expect_identical(analysis(split), training)
})

test_that("cannot create a split from empty training dataframe", {
  training <- tibble()
  testing <- tibble(x = c(5, 6))

  expect_error(
    make_splits(training, testing),
    "The analysis set must contain at least one row."
  )
})

test_that("cannot create a split from dataframes with different columns", {
  training <- tibble(x = c(1, 2, 3, 4))
  testing <- tibble(y = c(5, 6))

  expect_error(
    make_splits(training, testing),
    "The analysis and assessment sets must have"
    )
})
