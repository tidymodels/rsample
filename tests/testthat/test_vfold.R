context("V-fold CV")

library(testthat)
library(rsample)
library(purrr)

dat1 <- data.frame(a = 1:20, b = letters[1:20])

test_that('default param', {
  set.seed(11)
  rs1 <- vfold_cv(dat1)
  sizes1 <- dim_rset(rs1)

  expect_true(all(sizes1$analysis == 18))
  expect_true(all(sizes1$assessment == 2))
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

test_that('repeated', {
  set.seed(11)
  rs2 <- vfold_cv(dat1, repeats = 4)
  sizes2 <- dim_rset(rs2)

  expect_true(all(sizes2$analysis == 18))
  expect_true(all(sizes2$assessment == 2))
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
  rs3 <- vfold_cv(iris2, repeats = 2, strata = "Species")
  sizes3 <- dim_rset(rs3)

  expect_true(all(sizes3$analysis == 117))
  expect_true(all(sizes3$assessment == 13))

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
  expect_error(vfold_cv(iris, strata = iris$Species))
  expect_error(vfold_cv(iris, strata = c("Species", "Sepal.Width")))
})

test_that('printing', {
  expect_output(print(vfold_cv(mtcars)))
})


test_that('rsplit labels', {
  rs <- vfold_cv(mtcars)
  all_labs <- map_df(rs$splits, labels)
  original_id <- rs[, grepl("^id", names(rs))]
  expect_equal(all_labs, original_id)

  rs2 <- vfold_cv(mtcars, repeats = 4)
  all_labs2 <- map_df(rs2$splits, labels)
  original_id2 <- rs2[, grepl("^id", names(rs2))]
  expect_equal(all_labs2, original_id2)
})

