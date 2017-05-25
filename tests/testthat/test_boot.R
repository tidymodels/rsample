library(testthat)
library(rsample)
library(purrr)

dat1 <- data.frame(a = 1:20, b = letters[1:20])

test_that('default param', {
  set.seed(11)
  rs1 <- bootstraps(dat1)
  sizes1 <- rsample:::dim_rset(rs1)
  
  expect_true(all(sizes1$analysis == nrow(dat1)))
  same_data <-
    map_lgl(rs1$splits, function(x)
      all.equal(x$data, dat1))
  expect_true(all(same_data))
  
  good_holdout <- map_lgl(rs1$splits,
                          function(x) {
                            length(intersect(x$in_ind, x$out_id)) == 0
                          })
  expect_true(all(good_holdout))
})

test_that('apparent', {
  rs2 <- bootstraps(dat1, apparent = TRUE)
  sizes2 <- rsample:::dim_rset(rs2)
  
  expect_true(all(sizes2$analysis == nrow(dat1)))
  expect_true(all(sizes2$assessment[nrow(sizes2)] == nrow(dat1)))
  expect_equal(sizes2$assessment[sizes2$id == "Apparent"], nrow(dat1))
  res2 <-
    as.data.frame(rs2$splits[[nrow(sizes2)]], data = "assessment")
  expect_equal(res2, dat1)
  expect_error(bootstraps(dat1, apparent = TRUE, oob = FALSE))
})

test_that('No OOB', {
  rs3 <- bootstraps(dat1, oob = FALSE)
  sizes3 <- rsample:::dim_rset(rs3)
  expect_true(all(sizes3$assessment == 0))
})
