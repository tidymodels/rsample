context("rset constructor")

library(testthat)
library(rsample)

cars_10fold <- vfold_cv(mtcars)

test_that('bad args', {
  expect_error(
    new_rset(cars_10fold$splits[1:2], cars_10fold$id)
  )
  expect_error(
    new_rset(cars_10fold$splits, cars_10fold[ "splits"])
  )
  expect_error(
    new_rset(cars_10fold$splits, cars_10fold$splits)
  )
  expect_error(
    new_rset(list(1), "x"),
    "must be an `rsplit` object"
  )
  args <- list(a = 1, b = 2, 3)
  expect_error(
    new_rset(
      cars_10fold$splits,
      cars_10fold$id,
      attrib = args
      )
  )
})

test_that('rset with attributes', {
  args <- list(value = "potato")
  res3 <- new_rset(
    cars_10fold$splits,
    cars_10fold$id,
    attrib = args
  )
  expect_equal(sort(names(attributes(res3))),
               c("class", "fingerprint", "names", "row.names", "value"))
  expect_equal(attr(res3, "value"), "potato")
})

test_that('rset with additional classes', {
  res4 <- new_rset(
    cars_10fold$splits,
    cars_10fold$id,
    subclass = "potato"
  )
  expect_equal(class(res4),
               c("potato", "tbl_df", "tbl", "data.frame"))
})

test_that('not an rsplit', {
  folds <- vfold_cv(mtcars)
  expect_error(analysis(folds$splits[1]))
  expect_error(assessment(folds$splits[1]))
})
