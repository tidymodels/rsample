test_that("default param", {
  set.seed(11)
  rs1 <- vfold_cv(dat1)
  sizes1 <- dim_rset(rs1)

  expect_true(all(sizes1$analysis == 18))
  expect_true(all(sizes1$assessment == 2))
  same_data <-
    purrr::map_lgl(rs1$splits, function(x) {
      all.equal(x$data, dat1)
    })
  expect_true(all(same_data))

  good_holdout <- purrr::map_lgl(
    rs1$splits,
    function(x) {
      length(intersect(x$in_ind, x$out_id)) == 0
    }
  )
  expect_true(all(good_holdout))
})

test_that("repeated", {
  set.seed(11)
  rs2 <- vfold_cv(dat1, repeats = 4)
  sizes2 <- dim_rset(rs2)

  expect_true(all(sizes2$analysis == 18))
  expect_true(all(sizes2$assessment == 2))
  same_data <-
    purrr::map_lgl(rs2$splits, function(x) {
      all.equal(x$data, dat1)
    })
  expect_true(all(same_data))

  good_holdout <- purrr::map_lgl(
    rs2$splits,
    function(x) {
      length(intersect(x$in_ind, x$out_id)) == 0
    }
  )
  expect_true(all(good_holdout))
})

test_that("strata", {
  set.seed(11)
  data("mlc_churn", package = "modeldata")
  rs3 <- vfold_cv(mlc_churn, repeats = 2, strata = "voice_mail_plan")
  sizes3 <- dim_rset(rs3)

  expect_true(all(sizes3$analysis %in% 4499:4501))
  expect_true(all(sizes3$assessment %in% 499:501))

  rate <- purrr::map_dbl(
    rs3$splits,
    function(x) {
      dat <- as.data.frame(x)$voice_mail_plan
      mean(dat == "yes")
    }
  )
  expect_equal(mean(unique(rate)), 0.2645925848)

  good_holdout <- purrr::map_lgl(
    rs3$splits,
    function(x) {
      length(intersect(x$in_ind, x$out_id)) == 0
    }
  )
  expect_true(all(good_holdout))

  expect_snapshot(
    rs4 <- vfold_cv(mlc_churn, strata = state, pool = 0.01)
  )
})


test_that("bad args", {
  expect_error(vfold_cv(iris, strata = iris$Species))
  expect_error(vfold_cv(iris, strata = c("Species", "Sepal.Width")))
  expect_snapshot_error(vfold_cv(iris, v = -500))
  expect_snapshot_error(vfold_cv(iris, v = 500))
})

test_that("printing", {
  expect_snapshot(vfold_cv(mtcars))
})


test_that("rsplit labels", {
  rs <- vfold_cv(mtcars)
  all_labs <- purrr::map_df(rs$splits, labels)
  original_id <- rs[, grepl("^id", names(rs))]
  expect_equal(all_labs, original_id)

  rs2 <- vfold_cv(mtcars, repeats = 4)
  all_labs2 <- purrr::map_df(rs2$splits, labels)
  original_id2 <- rs2[, grepl("^id", names(rs2))]
  expect_equal(all_labs2, original_id2)
})
