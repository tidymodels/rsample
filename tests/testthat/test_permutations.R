context("Permutations")

library(testthat)
library(rsample)
library(purrr)

test_that('default param', {
  set.seed(11)
  rs1 <- permutations(mtcars, 1)
  sizes1 <- dim_rset(rs1)

  expect_true(all(sizes1$analysis == nrow(mtcars)))
  same_data <-
    map_lgl(rs1$splits, function(x)
      all.equal(x$data, mtcars))
  expect_true(all(same_data))

  good_holdout <- map_lgl(rs1$splits,
                          function(x) {
                            length(intersect(x$in_ind, x$out_id)) == 0
                          })
  expect_true(all(good_holdout))
})

test_that('apparent', {
  rs2 <- permutations(mtcars, 1, apparent = TRUE)
  sizes2 <- dim_rset(rs2)

  expect_true(all(sizes2$analysis == nrow(mtcars)))
  expect_true(all(sizes2$assessment[nrow(sizes2)] == nrow(mtcars)))
  expect_equal(sizes2$assessment[sizes2$id == "Apparent"], nrow(mtcars))
})

test_that('no assessment set', {
  xx <- permutations(mtcars, 1)
  expect_error(assessment(xx$splits[[1]]))
})

test_that('bad args', {
  expect_error(permutations(mtcars)) # no columns specified
  expect_error(permutations(mtcars, foo)) # column doesn't exist
  expect_error(permutations(mtcars, start_with("z"))) # column doesn't exist
  expect_error(permutations(mtcars, tidyselect::everything())) # all columns
})

test_that('printing', {
  expect_output(print(permutations(mtcars, 1)))
})

test_that('rsplit labels', {
  rs <- permutations(mtcars, 1)
  all_labs <- map_df(rs$splits, labels)
  original_id <- rs[, grepl("^id", names(rs))]
  expect_equal(all_labs, original_id)
})
