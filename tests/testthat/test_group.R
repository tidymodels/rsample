library(testthat)
library(rsample)
library(purrr)
library(tibble)

iris2 <- as.tibble(iris)

get_id_left_out <- function(x)
     unique(as.character(assessment(x)$Species))

test_that('bad args', {
  expect_error(group_vfold_cv(iris, group = iris$Species))
  expect_error(group_vfold_cv(iris, group = c("Species", "Sepal.Width")))  
  expect_error(group_vfold_cv(iris, group = "Specie"))  
  expect_error(group_vfold_cv(iris))    
  expect_error(group_vfold_cv(iris, group = "Species", v = 10))  
})


test_that('default param', {
  set.seed(11)
  rs1 <- group_vfold_cv(iris, "Species")
  sizes1 <- rsample:::dim_rset(rs1)
  
  expect_true(all(sizes1$analysis == 100))
  expect_true(all(sizes1$assessment == 50))  
  same_data <-
    map_lgl(rs1$splits, function(x)
      all.equal(x$data, iris))
  expect_true(all(same_data))
  
  good_holdout <- map_lgl(rs1$splits,
                          function(x) {
                            length(intersect(x$in_ind, x$out_id)) == 0
                          })
  expect_true(all(good_holdout))
  
  sp_out <- map_chr(rs1$splits, get_id_left_out)
  expect_true(all(table(sp_out) == 1))
})


test_that('v < max v', {
  set.seed(11)
  rs2 <- group_vfold_cv(iris, "Species", v = 2)
  sizes2 <- rsample:::dim_rset(rs2)
  
  expect_true(!all(sizes2$analysis == 100))
  expect_true(!all(sizes2$assessment == 50))  
  same_data <-
    map_lgl(rs2$splits, function(x)
      all.equal(x$data, iris))
  expect_true(all(same_data))
  
  good_holdout <- map_lgl(rs2$splits,
                          function(x) {
                            length(intersect(x$in_ind, x$out_id)) == 0
                          })
  expect_true(all(good_holdout))
  
  sp_out <- map(rs2$splits, get_id_left_out)
  expect_true(all(table(unlist(sp_out)) == 1))
})

test_that('tibble input', {
  set.seed(11)
  rs3 <- group_vfold_cv(iris2, "Species")
  sizes3 <- rsample:::dim_rset(rs3)
  
  expect_true(all(sizes3$analysis == 100))
  expect_true(all(sizes3$assessment == 50))  
  same_data <-
    map_lgl(rs3$splits, function(x)
      all.equal(x$data, iris2))
  expect_true(all(same_data))
  
  good_holdout <- map_lgl(rs3$splits,
                          function(x) {
                            length(intersect(x$in_ind, x$out_id)) == 0
                          })
  expect_true(all(good_holdout))
  
  sp_out <- map_chr(rs3$splits, get_id_left_out)
  expect_true(all(table(sp_out) == 1))
})


test_that('printing', {
  expect_output(print(group_vfold_cv(iris, "Species")))
})


