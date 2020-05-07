context("Strata constructor")

library(testthat)
library(rsample)
library(purrr)

test_that('simple numerics', {
  set.seed(13333)
  x1 <- rnorm(1000)
  str1a <- make_strata(x1)
  tab1a <- table(str1a)
  expect_equal(as.vector(tab1a), rep(250, 4))

  str1b <- expect_warning(make_strata(x1, depth = 500), "2 breaks instead")
  tab1b <- table(str1b)
  expect_equal(as.vector(tab1b), rep(500, 2))
})

test_that('simple character', {
  x2 <- factor(rep(LETTERS[1:5], each = 50))
  str2a <- make_strata(x2)
  expect_equal(table(str2a, dnn = ""), table(x2, dnn = ""))
})

test_that('bad data', {
  x3 <- factor(rep(LETTERS[1:15], each = 50))
  expect_warning(make_strata(x3))
  expect_warning(make_strata(mtcars$mpg))
})



