library(testthat)
library(rsample)
library(purrr)

dat1 <- data.frame(a = 1:10, b = letters[1:10])

test_that('Loooooo', {

  loo1 <- loo_cv(dat1)
  expect_equal(nrow(loo1), nrow(dat1))
  
  same_data <-
    map_lgl(loo1$splits, function(x)
      all.equal(x$data, dat1))
  expect_true(all(same_data))
  
  holdouts <-
    map_lgl(loo1$splits, function(x)
      length(x$out_id) == 1)
  expect_true(all(holdouts)) 
  
  retained <-
    map_lgl(loo1$splits, function(x)
      length(x$in_id) == (nrow(dat1) - 1))  
  expect_true(all(retained))  
})
