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
  x2 <- factor(rep(LETTERS[1:12], each = 20))
  expect_warning(
    str2a <- make_strata(x2, pool = 0.05),
    "Stratifying groups that make up 5%"
  )
  expect_equal(table(str2a, dnn = ""), table(x2, dnn = ""))

})

test_that('bad data', {
  x3 <- factor(rep(LETTERS[1:15], each = 50))
  expect_warning(make_strata(x3), "Too little data")
  expect_warning(make_strata(x3, pool = 0.06),
                 "Stratifying groups that make up 6%")
  expect_warning(make_strata(mtcars$mpg))
})



# strata_check() ----------------------------------------------------------

test_that("don't stratify on Surv objects", {
  df <- data.frame(
    time = c(85, 79, 70, 6, 32, 8, 17, 93, 81, 76),
    event = c(0, 0, 1, 0, 0, 0, 1, 1, 1, 1)
  )
  df$surv <- structure(
    c(85, 79, 70, 6, 32, 8, 17, 93, 81, 76,
      0, 0, 1, 0, 0, 0, 1, 1, 1, 1),
    .Dim = c(10L, 2L),
    .Dimnames = list(NULL, c("time", "status")),
    type = "right",
    class = "Surv")

  expect_error(strata_check("surv", df))
})
