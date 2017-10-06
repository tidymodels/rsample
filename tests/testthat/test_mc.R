library(testthat)
library(rsample)
library(purrr)

dat1 <- data.frame(a = 1:20, b = letters[1:20])

test_that('default param', {
  set.seed(11)
  rs1 <- mc_cv(dat1)
  sizes1 <- rsample:::dim_rset(rs1)
  
  expect_true(all(sizes1$analysis == 15))
  expect_true(all(sizes1$assessment == 5))  
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

test_that('different percent', {
  set.seed(11)
  rs2 <- mc_cv(dat1, prop = .5)
  sizes2 <- rsample:::dim_rset(rs2)
  
  expect_true(all(sizes2$analysis == 10))
  expect_true(all(sizes2$assessment == 10))  
  same_data <-
    map_lgl(rs2$splits, function(x)
      all.equal(x$data, dat1))
  expect_true(all(same_data))
  
  good_holdout <- map_lgl(rs2$splits,
                          function(x) {
                            length(intersect(x$in_ind, x$out_id)) == 0
                          })
  expect_true(all(good_holdout))
})

test_that('strata', {
  iris2 <- iris[1:130, ]
  set.seed(11)
  rs3 <- mc_cv(iris2, strata = "Species")
  sizes3 <- rsample:::dim_rset(rs3)
  
  expect_true(all(sizes3$analysis == 99))
  expect_true(all(sizes3$assessment == 31))  
  
  rate <- map_dbl(rs3$splits,
                  function(x) {
                    dat <- as.data.frame(x)$Species
                    mean(dat == "virginica")
                  })
  expect_true(length(unique(rate)) == 1)
  
  good_holdout <- map_lgl(rs3$splits,
                          function(x) {
                            length(intersect(x$in_ind, x$out_id)) == 0
                          })
  expect_true(all(good_holdout))
})


test_that('bad args', {
  expect_error(mc_cv(iris, strata = iris$Species))
  expect_error(mc_cv(iris, strata = 2))  
  expect_error(mc_cv(iris, strata = c("Species", "Species")))  
})


test_that('printing', {
  expect_output(print(mc_cv(iris)))
})
