context("Group resampling")

library(testthat)
library(rsample)
library(purrr)
library(tibble)

warpbreaks2 <- as_tibble(warpbreaks)

get_id_left_out <- function(x)
     unique(as.character(assessment(x)$tension))

test_that('bad args', {
  expect_error(group_vfold_cv(warpbreaks, group = warpbreaks$tension))
  expect_error(group_vfold_cv(warpbreaks, group = c("tension", "wool")))
  expect_error(group_vfold_cv(warpbreaks, group = "tensio"))
  expect_error(group_vfold_cv(warpbreaks))
  expect_error(group_vfold_cv(warpbreaks, group = "tension", v = 10))
})


test_that('default param', {
  set.seed(11)
  rs1 <- group_vfold_cv(warpbreaks, "tension")
  sizes1 <- dim_rset(rs1)

  expect_true(all(sizes1$analysis == 36))
  expect_true(all(sizes1$assessment == 18))
  same_data <-
    map_lgl(rs1$splits, function(x)
      all.equal(x$data, warpbreaks))
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
  rs2 <- group_vfold_cv(warpbreaks, "tension", v = 2)
  sizes2 <- dim_rset(rs2)

  expect_true(!all(sizes2$analysis == 36))
  expect_true(!all(sizes2$assessment == 18))
  same_data <-
    map_lgl(rs2$splits, function(x)
      all.equal(x$data, warpbreaks))
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
  rs3 <- group_vfold_cv(warpbreaks2, "tension")
  sizes3 <- dim_rset(rs3)

  expect_true(all(sizes3$analysis == 36))
  expect_true(all(sizes3$assessment == 18))
  same_data <-
    map_lgl(rs3$splits, function(x)
      all.equal(x$data, warpbreaks2))
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
  expect_output(print(group_vfold_cv(warpbreaks, "tension")))
})

test_that('printing with ...', {
  verify_output(test_path("test-print-groups.txt"), {
    print(group_vfold_cv(warpbreaks, "tension"), n = 2)
  })
})

test_that('rsplit labels', {
  rs <- group_vfold_cv(warpbreaks, "tension")
  all_labs <- map_df(rs$splits, labels)
  original_id <- rs[, grepl("^id", names(rs))]
  expect_equal(all_labs, original_id)
})

