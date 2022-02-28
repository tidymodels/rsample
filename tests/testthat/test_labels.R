library(testthat)
library(rsample)

context("Labels")

test_that("basic cv", {
  cv_obj <- vfold_cv(mtcars)
  expect_equal(cv_obj$id, labels(cv_obj))
  expect_is(labels(cv_obj), "character")
  expect_s3_class(labels(cv_obj, TRUE), "factor")
})

test_that("repeated cv", {
  rcv_obj <- vfold_cv(mtcars, repeats = 3)
  expect_equal(
    paste(rcv_obj$id, rcv_obj$id2, sep = "."),
    labels(rcv_obj)
  )
  expect_is(labels(rcv_obj), "character")
  expect_s3_class(labels(rcv_obj, TRUE), "factor")
})

test_that("nested cv", {
  expect_error(
    labels(
      nested_cv(mtcars,
        outside = vfold_cv(v = 3),
        inside = bootstraps(times = 5)
      )
    )
  )
})

test_that("adding labels", {
  set.seed(363)
  car_folds <- vfold_cv(mtcars, repeats = 3)

  res <-
    analysis(car_folds$splits[[1]]) %>%
    add_resample_id(car_folds$splits[[1]])
  expect_equal(colnames(res), c(colnames(mtcars), "id", "id2"))

  car_bt <- bootstraps(mtcars)

  res <- analysis(car_bt$splits[[1]]) %>%
    add_resample_id(car_bt$splits[[1]])

  expect_equal(colnames(res), c(colnames(mtcars), "id"))

  res <- analysis(car_bt$splits[[1]]) %>%
    add_resample_id(car_bt$splits[[1]], TRUE)

  expect_equal(colnames(res), c(colnames(mtcars), ".id"))

  expect_error(
    analysis(car_folds$splits[[1]]) %>%
      add_resample_id(car_folds$splits[[1]], 7)
  )
  expect_error(
    analysis(car_folds$splits[[1]]) %>%
      add_resample_id(car_folds$splits[[1]], c(TRUE, TRUE))
  )

  expect_error(
    analysis(car_folds$splits[[1]]) %>%
      add_resample_id(car_folds$splits)
  )

  expect_error(
    analysis(car_folds$splits[[1]]) %>%
      as.matrix() %>%
      add_resample_id(car_folds$splits[[1]])
  )
})
