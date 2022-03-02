test_that("bad args", {
  expect_error(
    new_rset(car_folds$splits[1:2], car_folds$id)
  )
  expect_error(
    new_rset(car_folds$splits, car_folds["splits"])
  )
  expect_error(
    new_rset(car_folds$splits, car_folds$splits)
  )
  expect_error(
    new_rset(list(1), "x"),
    "must be an `rsplit` object"
  )
  args <- list(a = 1, b = 2, 3)
  expect_error(
    new_rset(
      car_folds$splits,
      car_folds$id,
      attrib = args
    )
  )
})

test_that("rset with attributes", {
  args <- list(value = "potato")
  res3 <- new_rset(
    car_folds$splits,
    car_folds$id,
    attrib = args
  )
  expect_equal(
    sort(names(attributes(res3))),
    c("class", "fingerprint", "names", "row.names", "value")
  )
  expect_equal(attr(res3, "value"), "potato")
})

test_that("rset with additional classes", {
  res4 <- new_rset(
    car_folds$splits,
    car_folds$id,
    subclass = "potato"
  )
  expect_equal(
    class(res4),
    c("potato", "tbl_df", "tbl", "data.frame")
  )
})

test_that("not an rsplit", {
  folds <- vfold_cv(mtcars)
  expect_error(analysis(folds$splits[1]))
  expect_error(assessment(folds$splits[1]))
})
