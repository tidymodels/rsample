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

  expect_snapshot(error = TRUE, {
    make_splits(indices, df)
  })
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

  expect_snapshot(error = TRUE, {
    make_splits(training, testing)
  })
})

test_that("cannot create a split from dataframes with different columns", {
  training <- tibble(x = c(1, 2, 3, 4))
  testing <- tibble(y = c(5, 6))

  expect_snapshot(error = TRUE, {
    make_splits(training, testing)
  })
})

test_that("improper argument", {
  expect_snapshot(error = TRUE, {
    make_splits("potato")
  })
})

test_that("basic naming sequences", {
  expect_equal(names0(2), c("x1", "x2"))
  expect_equal(names0(2, "y"), c("y1", "y2"))
  expect_equal(
    names0(10),
    c(paste0("x0", 1:9), "x10")
  )
})

test_that("get_rsplit()", {
  skip_if_not_installed("withr")
  # for `validation_split()` and variants
  withr::local_options(lifecycle_verbosity = "quiet")

  val <- withr::with_seed(
    11,
    validation_split(warpbreaks)
  )

  expect_identical(val$splits[[1]], get_rsplit(val, 1))

  expect_snapshot(error = TRUE,{
    get_rsplit(val, 3)
  })

  expect_snapshot(error = TRUE,{
    get_rsplit(val, c(1, 2))
  })

  expect_snapshot(error = TRUE,{
    get_rsplit(val, 1.5)
  })

  expect_snapshot(error = TRUE,{
    get_rsplit(warpbreaks, 1)
  })

})
