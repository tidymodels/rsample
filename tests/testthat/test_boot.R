context("Bootstrapping")

library(testthat)
library(rsample)
library(purrr)

dat1 <- data.frame(a = 1:20, b = letters[1:20])

test_that("default param", {
  set.seed(11)
  rs1 <- bootstraps(dat1)
  sizes1 <- dim_rset(rs1)

  expect_true(all(sizes1$analysis == nrow(dat1)))
  same_data <-
    map_lgl(rs1$splits, function(x) {
      all.equal(x$data, dat1)
    })
  expect_true(all(same_data))

  good_holdout <- map_lgl(
    rs1$splits,
    function(x) {
      length(intersect(x$in_ind, x$out_id)) == 0
    }
  )
  expect_true(all(good_holdout))
})

test_that("apparent", {
  rs2 <- bootstraps(dat1, apparent = TRUE)
  sizes2 <- dim_rset(rs2)

  expect_true(all(sizes2$analysis == nrow(dat1)))
  expect_true(all(sizes2$assessment[nrow(sizes2)] == nrow(dat1)))
  expect_equal(sizes2$assessment[sizes2$id == "Apparent"], nrow(dat1))
  res2 <-
    as.data.frame(rs2$splits[[nrow(sizes2)]], data = "assessment")
  expect_equal(res2, dat1)
})


test_that("strata", {
  set.seed(11)
  rs4 <- bootstraps(warpbreaks, strata = "tension")
  sizes4 <- dim_rset(rs4)

  expect_true(all(sizes4$analysis == nrow(warpbreaks)))

  rate <- map_dbl(
    rs4$splits,
    function(x) {
      dat <- as.data.frame(x)$tension
      mean(dat == "M")
    }
  )
  expect_true(length(unique(rate)) == 1)

  good_holdout <- map_lgl(
    rs4$splits,
    function(x) {
      length(intersect(x$in_ind, x$out_id)) == 0
    }
  )
  expect_true(all(good_holdout))

  rs5 <- bootstraps(warpbreaks, apparent = TRUE, strata = "tension")
  sizes5 <- dim_rset(rs5)

  expect_true(all(sizes5$analysis == nrow(warpbreaks)))
  expect_true(all(sizes5$assessment[nrow(sizes5)] == nrow(warpbreaks)))
  expect_equal(sizes5$assessment[sizes5$id == "Apparent"], nrow(warpbreaks))
  res5 <-
    as.data.frame(rs5$splits[[nrow(sizes5)]], data = "assessment")
  expect_equal(res5, warpbreaks)
})


test_that("bad args", {
  expect_error(bootstraps(warpbreaks, strata = warpbreaks$tension))
  expect_error(bootstraps(warpbreaks, strata = c("tension", "wool")))
})


test_that("printing", {
  expect_output(print(bootstraps(warpbreaks)))
})


test_that("rsplit labels", {
  rs <- bootstraps(warpbreaks)
  all_labs <- map_df(rs$splits, labels)
  original_id <- rs[, grepl("^id", names(rs))]
  expect_equal(all_labs, original_id)
})
