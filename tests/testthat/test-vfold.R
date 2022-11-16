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
  skip_if_not(rlang::is_installed("modeldata"))
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
  expect_snapshot_error(vfold_cv(iris, v = 0))
  expect_snapshot_error(vfold_cv(iris, v = NULL))
  expect_snapshot_error(vfold_cv(iris, v = 500))
  expect_snapshot_error(vfold_cv(iris, v = 150, repeats = 2))
  expect_snapshot_error(vfold_cv(Orange, repeats = 0))
  expect_snapshot_error(vfold_cv(Orange, repeats = NULL))
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

test_that("grouping -- bad args", {
  expect_error(group_vfold_cv(warpbreaks, group = warpbreaks$tension))
  expect_error(group_vfold_cv(warpbreaks, group = c("tension", "wool")))
  expect_error(group_vfold_cv(warpbreaks, group = "tensio"))
  expect_error(group_vfold_cv(warpbreaks))
  expect_error(group_vfold_cv(warpbreaks, group = "tension", v = 10))
  expect_snapshot_error(group_vfold_cv(dat1, c, v = 4, repeats = 4))
  expect_snapshot_error(group_vfold_cv(dat1, c, repeats = 4))
})


test_that("grouping -- default param", {
  set.seed(11)
  rs1 <- group_vfold_cv(warpbreaks, "tension")
  sizes1 <- dim_rset(rs1)

  expect_true(all(sizes1$analysis == 36))
  expect_true(all(sizes1$assessment == 18))
  same_data <-
    purrr::map_lgl(rs1$splits, function(x) {
      all.equal(x$data, warpbreaks)
    })
  expect_true(all(same_data))

  good_holdout <- purrr::map_lgl(
    rs1$splits,
    function(x) {
      length(intersect(x$in_ind, x$out_id)) == 0
    }
  )
  expect_true(all(good_holdout))

  sp_out <- purrr::map_chr(rs1$splits, get_id_left_out)
  expect_true(all(table(sp_out) == 1))
})


test_that("grouping -- v < max v", {
  set.seed(11)
  rs2 <- group_vfold_cv(warpbreaks, "tension", v = 2)
  sizes2 <- dim_rset(rs2)

  expect_true(!all(sizes2$analysis == 36))
  expect_true(!all(sizes2$assessment == 18))
  same_data <-
    purrr::map_lgl(rs2$splits, function(x) {
      all.equal(x$data, warpbreaks)
    })
  expect_true(all(same_data))

  good_holdout <- purrr::map_lgl(
    rs2$splits,
    function(x) {
      length(intersect(x$in_ind, x$out_id)) == 0
    }
  )
  expect_true(all(good_holdout))

  sp_out <- purrr::map(rs2$splits, get_id_left_out)
  expect_true(all(table(unlist(sp_out)) == 1))
})

test_that("grouping -- tibble input", {
  warpbreaks2 <- tibble::as_tibble(warpbreaks)
  set.seed(11)
  rs3 <- group_vfold_cv(warpbreaks2, "tension")
  sizes3 <- dim_rset(rs3)

  expect_true(all(sizes3$analysis == 36))
  expect_true(all(sizes3$assessment == 18))
  same_data <-
    purrr::map_lgl(rs3$splits, function(x) {
      all.equal(x$data, warpbreaks2)
    })
  expect_true(all(same_data))

  good_holdout <- purrr::map_lgl(
    rs3$splits,
    function(x) {
      length(intersect(x$in_ind, x$out_id)) == 0
    }
  )
  expect_true(all(good_holdout))

  sp_out <- purrr::map_chr(rs3$splits, get_id_left_out)
  expect_true(all(table(sp_out) == 1))
})

test_that("grouping -- other balance methods", {
  skip_if_not(rlang::is_installed("modeldata"))
  data(ames, package = "modeldata")
  set.seed(11)
  rs1 <- group_vfold_cv(
    ames,
    "Neighborhood",
    balance = "observations",
    v = 5
  )
  expect_snapshot(rs1)

  sizes1 <- dim_rset(rs1)
  same_data <-
    purrr::map_lgl(rs1$splits, function(x) {
      all.equal(x$data, ames)
    })
  expect_true(all(same_data))

  good_holdout <- purrr::map_lgl(
    rs1$splits,
    function(x) {
      length(intersect(x$in_ind, x$out_id)) == 0
    }
  )
  expect_true(all(good_holdout))

  expect_true(
    !any(
      unique(as.character(assessment(rs1$splits[[1]])$Neighborhood)) %in%
        unique(as.character(analysis(rs1$splits[[1]])$Neighborhood))
    )
  )

})

test_that("grouping -- strata", {
  set.seed(11)

  n_common_class <- 70
  n_rare_class <- 30

  group_table <- tibble(
    group = 1:100,
    outcome = sample(c(rep(0, n_common_class), rep(1, n_rare_class)))
  )
  observation_table <- tibble(
    group = sample(1:100, 1e5, replace = TRUE),
    observation = 1:1e5
  )
  sample_data <- dplyr::full_join(group_table, observation_table, by = "group")
  rs4 <- group_vfold_cv(sample_data, group, v = 5, strata = outcome)
  sizes4 <- dim_rset(rs4)
  expect_snapshot(sizes4)

  rate <- purrr::map_dbl(
    rs4$splits,
    function(x) {
      dat <- as.data.frame(x)$outcome
      mean(dat == "1")
    }
  )
  expect_equal(mean(rate), 0.3, tolerance = 1e-2)

  good_holdout <- purrr::map_lgl(
    rs4$splits,
    function(x) {
      length(intersect(x$in_ind, x$out_id)) == 0
    }
  )
  expect_true(all(good_holdout))

  expect_snapshot_warning(
    group_vfold_cv(sample_data, group, strata = outcome)
  )

  expect_equal(
    nrow(
      suppressWarnings(
        group_vfold_cv(sample_data, group, strata = outcome)
      )
    ),
    n_rare_class
  )

  rs5 <- group_vfold_cv(
    sample_data,
    group,
    v = 5,
    strata = outcome,
    balance = "observations"
  )
  sizes5 <- dim_rset(rs5)
  expect_snapshot(sizes5)

  rate <- purrr::map_dbl(
    rs5$splits,
    function(x) {
      dat <- as.data.frame(x)$outcome
      mean(dat == "1")
    }
  )
  expect_equal(mean(rate), 0.3, tolerance = 1e-2)

  good_holdout <- purrr::map_lgl(
    rs5$splits,
    function(x) {
      length(intersect(x$in_ind, x$out_id)) == 0
    }
  )
  expect_true(all(good_holdout))

  expect_snapshot_warning(
    group_vfold_cv(sample_data, group, strata = outcome)
  )

  expect_equal(
    nrow(
      suppressWarnings(
        group_vfold_cv(
          sample_data,
          group,
          strata = outcome,
          balance = "observations"
        )
      )
    ),
    n_rare_class
  )
})

test_that("grouping -- repeated", {
  set.seed(11)
  rs2 <- group_vfold_cv(dat1, c, v = 3, repeats = 4)
  sizes2 <- dim_rset(rs2)

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

test_that("grouping -- printing", {
  expect_snapshot(group_vfold_cv(warpbreaks, "tension"))
})

test_that("grouping -- printing with ...", {
  expect_snapshot(
    print(group_vfold_cv(warpbreaks, "tension"), n = 2)
  )
})

test_that("grouping -- rsplit labels", {
  rs <- group_vfold_cv(warpbreaks, "tension")
  all_labs <- purrr::map_df(rs$splits, labels)
  original_id <- rs[, grepl("^id", names(rs))]
  expect_equal(all_labs, original_id)
})
