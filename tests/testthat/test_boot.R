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

test_that('strata', {
  iris2 <- iris[1:130, ]
  set.seed(11)
  rs4 <- bootstraps(iris2,  strata = "Species")
  sizes4 <- rsample:::dim_rset(rs4)
  
  expect_true(all(sizes4$analysis == nrow(iris2)))

  rate <- map_dbl(rs4$splits,
                  function(x) {
                    dat <- as.data.frame(x)$Species
                    mean(dat == "virginica")
                  })
  expect_true(length(unique(rate)) == 1)
  
  good_holdout <- map_lgl(rs4$splits,
                          function(x) {
                            length(intersect(x$in_ind, x$out_id)) == 0
                          })
  expect_true(all(good_holdout))
  
  rs5 <- bootstraps(iris2, apparent = TRUE, strata = "Species")
  sizes5 <- rsample:::dim_rset(rs5)
  
  expect_true(all(sizes5$analysis == nrow(iris2)))
  expect_true(all(sizes5$assessment[nrow(sizes5)] == nrow(iris2)))
  expect_equal(sizes5$assessment[sizes5$id == "Apparent"], nrow(iris2))
  res5 <-
    as.data.frame(rs5$splits[[nrow(sizes5)]], data = "assessment")
  expect_equal(res5, iris2)
  
})


test_that('bad args', {
  expect_error(bootstraps(iris, strata = iris$Species))
  expect_error(bootstraps(iris, strata = 2))  
  expect_error(bootstraps(iris, strata = c("Species", "Species")))  
})



