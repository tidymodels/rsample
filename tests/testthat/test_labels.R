context("Labels")

library(testthat)
library(rsample)

test_that('basic cv', {
  cv_obj <- vfold_cv(mtcars)
  expect_equal(cv_obj$id, labels(cv_obj))
  expect_is(labels(cv_obj), "character")
  expect_s3_class(labels(cv_obj, TRUE), "factor")
})

test_that('repeated cv', {
  rcv_obj <- vfold_cv(mtcars, repeats = 3)
  expect_equal(paste(rcv_obj$id, rcv_obj$id2, sep = "."),
               labels(rcv_obj))
  expect_is(labels(rcv_obj), "character")
  expect_s3_class(labels(rcv_obj, TRUE), "factor")
})

test_that('nested cv', {
  expect_error(
    labels(
      nested_cv(mtcars,
                outside = vfold_cv(v = 3),
                inside = bootstraps(times = 5)
                )
      )
    )
})
