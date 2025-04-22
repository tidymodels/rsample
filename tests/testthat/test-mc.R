test_that("default param", {
  set.seed(11)
  rs1 <- mc_cv(dat1)
  sizes1 <- dim_rset(rs1)

  expect_true(all(sizes1$analysis == 15))
  expect_true(all(sizes1$assessment == 5))
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

test_that("different percent", {
  set.seed(11)
  rs2 <- mc_cv(dat1, prop = .5)
  sizes2 <- dim_rset(rs2)

  expect_true(all(sizes2$analysis == 10))
  expect_true(all(sizes2$assessment == 10))
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

  rs3 <- mc_cv(warpbreaks, strata = "tension")
  sizes3 <- dim_rset(rs3)

  # sum(floor(table(warpbreaks$tension) * prop)) = 39
  expect_true(all(sizes3$analysis == 39))
  expect_true(all(sizes3$assessment == 15))

  rate <- purrr::map_dbl(
    rs3$splits,
    function(x) {
      dat <- as.data.frame(x)$tension
      mean(dat == "M")
    }
  )
  expect_true(length(unique(rate)) == 1)

  good_holdout <- purrr::map_lgl(
    rs3$splits,
    function(x) {
      length(intersect(x$in_ind, x$out_id)) == 0
    }
  )
  expect_true(all(good_holdout))
})


test_that("bad args", {
  expect_snapshot(error = TRUE, {
    mc_cv(mtcars, prop = -1)
  })
  expect_snapshot(error = TRUE, {
    mc_cv(mtcars, prop = 1)
  })
  expect_snapshot(error = TRUE, {
    mc_cv(warpbreaks, strata = warpbreaks$tension)
  })
  expect_snapshot(error = TRUE, {
    mc_cv(warpbreaks, strata = c("tension", "wool"))
  })
})


test_that("printing", {
  expect_snapshot(mc_cv(warpbreaks))
})


test_that("rsplit labels", {
  rs <- mc_cv(mtcars)
  all_labs <- purrr::map(rs$splits, labels) %>%
    list_rbind()
  original_id <- rs[, grepl("^id", names(rs))]
  expect_equal(all_labs, original_id)
})

test_that("grouping - bad args", {
  expect_snapshot(error = TRUE, {
    group_mc_cv(warpbreaks, group = warpbreaks$tension)
  })
  expect_snapshot(error = TRUE, {
    group_mc_cv(warpbreaks, group = c("tension", "wool"))
  })
  expect_snapshot(error = TRUE, {
    group_mc_cv(warpbreaks, group = "tensio")
  })
  expect_snapshot(error = TRUE, {
    group_mc_cv(warpbreaks)
  })
  expect_snapshot(error = TRUE, {
    group_mc_cv(mtcars, group = "cyl", prop = 1)
  })
  expect_snapshot(error = TRUE, {
    group_mc_cv(warpbreaks, group = "tension", balance = "groups")
  })
  set.seed(1)
  expect_snapshot(error = TRUE, {
    group_mc_cv(warpbreaks, group = "tension", prop = 0.99)
  })
})

test_that("grouping - default param", {
  set.seed(11)
  rs1 <- group_mc_cv(warpbreaks, "tension")
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
    group = sample(1:100, 5e4, replace = TRUE),
    observation = 1:5e4
  )
  sample_data <- dplyr::full_join(
    group_table,
    observation_table,
    by = "group",
    multiple = "all"
  )
  rs4 <- group_mc_cv(sample_data, group, times = 5, strata = outcome)
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
})

test_that("grouping - tibble input", {
  warpbreaks2 <- tibble::as_tibble(warpbreaks)
  set.seed(11)
  rs3 <- group_mc_cv(warpbreaks2, "tension")
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
})

test_that("grouping with times = 1 works", {
  set.seed(11)
  rs3 <- group_mc_cv(warpbreaks, "tension", times = 1)
  sizes3 <- dim_rset(rs3)

  expect_true(all(sizes3$analysis == 36))
  expect_true(all(sizes3$assessment == 18))
  same_data <-
    purrr::map_lgl(rs3$splits, function(x) {
      all.equal(x$data, warpbreaks)
    })
  expect_true(all(same_data))

  good_holdout <- purrr::map_lgl(
    rs3$splits,
    function(x) {
      length(intersect(x$in_ind, x$out_id)) == 0
    }
  )
  expect_true(all(good_holdout))
})

test_that("grouping - printing", {
  expect_snapshot(group_mc_cv(warpbreaks, "tension"))
})

test_that("grouping - printing with ...", {
  expect_snapshot(
    print(group_mc_cv(warpbreaks, "tension"), n = 2)
  )
})

test_that("grouping - rsplit labels", {
  rs <- group_mc_cv(warpbreaks, "tension")
  all_labs <- purrr::map(rs$splits, labels) %>%
    list_rbind()
  original_id <- rs[, grepl("^id", names(rs))]
  expect_equal(all_labs, original_id)
})
