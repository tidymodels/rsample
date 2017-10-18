library(testthat)
library(rsample)
library(tidyr)

cvs <- vfold_cv(mtcars)
cvs$one <- 1
cvs$two <- 2

expt <- cvs %>% gather(model, statistic, -id)


test_that('basics', {
  res_1 <- gather(cvs)
  expect_equal(res_1, expt)
})


test_that('extra args ignored', {
  res_2 <- gather(cvs, contains("o"))
  expect_equal(res_2, expt)
  res_2 <- gather(cvs, key = ignored)
  expect_equal(res_2, expt)
  res_3 <- gather(cvs, ignored)
  expect_equal(res_2, expt)
})

test_that('no extra cols', {
  expect_error(gather(vfold_cv(mtcars)))
})

test_that('no splits', {
  cvs2 <- cvs
  cvs2$splits <- NULL
  res_4 <- gather(cvs2)
  expect_equal(res_4, expt)
})

