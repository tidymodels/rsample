test_that("Bootstrap estimate of mean is close to estimate of mean from normal distribution", {
  skip_if_not_installed("broom")
  skip_on_cran()

  set.seed(888)
  rand_nums <- rnorm(n = 1000, mean = 10, sd = 1)
  dat <- data.frame(x = rand_nums)

  set.seed(456765)
  bt_norm <- bootstraps(dat, times = 1000, apparent = TRUE) %>%
    dplyr::mutate(
      stats = purrr::map(splits, ~ get_stats(.x))
    )

  ttest <- broom::tidy(t.test(rand_nums))
  ttest_lower_conf <- broom::tidy(t.test(rand_nums, conf.level = 0.8))
  single_pct_res <- int_pctl(bt_norm, stats)

  single_t_res <- int_t(bt_norm, stats)

  single_bca_res <- int_bca(bt_norm, stats, .fn = get_stats)

  single_bca_res_lower_conf <- int_bca(bt_norm, stats, .fn = get_stats, alpha = 0.2)

  expect_equal(ttest$conf.low,
               single_pct_res$.lower,
               tolerance = 0.001
  )
  expect_equal(unname(ttest$estimate),
               single_pct_res$.estimate,
               tolerance = 0.001
  )
  expect_equal(ttest$conf.high,
               single_pct_res$.upper,
               tolerance = 0.001
  )

  expect_equal(ttest$conf.low,
               single_t_res$.lower,
               tolerance = 0.001
  )
  expect_equal(unname(ttest$estimate),
               single_t_res$.estimate,
               tolerance = 0.001
  )
  expect_equal(ttest$conf.high,
               single_pct_res$.upper,
               tolerance = 0.001
  )

  expect_equal(ttest$conf.low,
               single_bca_res$.lower,
               tolerance = 0.001
  )
  expect_equal(unname(ttest$estimate),
               single_bca_res$.estimate,
               tolerance = 0.001
  )
  expect_equal(ttest$conf.high,
               single_bca_res$.upper,
               tolerance = 0.001
  )

  expect_equal(ttest_lower_conf$conf.low,
               single_bca_res_lower_conf$.lower,
               tolerance = 0.001
  )
  expect_equal(unname(ttest_lower_conf$estimate),
               single_bca_res_lower_conf$.estimate,
               tolerance = 0.001
  )
  expect_equal(ttest_lower_conf$conf.high,
               single_bca_res_lower_conf$.upper,
               tolerance = 0.001
  )
})

# ------------------------------------------------------------------------------

test_that("Wrappers -- selection of multiple variables works", {
  skip_if_not_installed("broom")
  skip_if_not_installed("modeldata")
  data("attrition", package = "modeldata")
  func <- function(split, ...) {
    lm(Age ~ HourlyRate + DistanceFromHome, data = analysis(split)) %>% tidy()
  }

  # generate boostrap resamples
  set.seed(888)
  bt_resamples <- bootstraps(attrition, times = 1000, apparent = TRUE) %>%
    mutate(res = purrr::map(splits, func))

  attrit_tidy <-
    lm(Age ~ HourlyRate + DistanceFromHome, data = attrition) %>%
    tidy(conf.int = TRUE) %>%
    dplyr::arrange(term)

  pct_res <-
    int_pctl(bt_resamples, res) %>%
    inner_join(attrit_tidy, by = "term")
  expect_equal(pct_res$conf.low, pct_res$.lower, tolerance = .01)
  expect_equal(pct_res$conf.high, pct_res$.upper, tolerance = .01)


  t_res <-
    int_t(bt_resamples, res) %>%
    inner_join(attrit_tidy, by = "term")
  expect_equal(t_res$conf.low, t_res$.lower, tolerance = .01)
  expect_equal(t_res$conf.high, t_res$.upper, tolerance = .01)


  bca_res <-
    int_bca(bt_resamples, res, .fn = func) %>%
    inner_join(attrit_tidy, by = "term")
  expect_equal(bca_res$conf.low, bca_res$.lower, tolerance = .01)
  expect_equal(bca_res$conf.high, bca_res$.upper, tolerance = .01)
})

# ------------------------------------------------------------------------------

test_that("Upper & lower confidence interval does not contain NA", {
  bad_stats <- function(split, ...) {
    tibble(
      term = "mean",
      estimate = NA_real_,
      std.error = runif(1)
    )
  }

  set.seed(888)
  bt_resamples <- bootstraps(data.frame(x = 1:100), times = 1000, apparent = TRUE) %>%
    mutate(res = purrr::map(splits, bad_stats))

  expect_snapshot(
    int_pctl(bt_resamples, res),
    error = TRUE
  )

  expect_snapshot(
    int_t(bt_resamples, res),
    error = TRUE
  )

  expect_snapshot(
    int_bca(bt_resamples, res, .fn = bad_stats),
    error = TRUE
  )
})

# ------------------------------------------------------------------------------

test_that(
  "Sufficient replications needed to sufficiently reduce Monte Carlo sampling Error for BCa method",
  {
    set.seed(888)
    rand_nums <- rnorm(n = 1000, mean = 10, sd = 1)
    dat <- data.frame(x = rand_nums)
    set.seed(456765)
    bt_small <-
      bootstraps(dat, times = 10, apparent = TRUE) %>%
      dplyr::mutate(
        stats = purrr::map(splits, ~ get_stats(.x)),
        junk = 1:11
     )

    expect_snapshot(int_pctl(bt_small, stats))
    expect_snapshot(int_t(bt_small, stats))
  }
)

test_that(
  "Sufficient replications needed to sufficiently reduce Monte Carlo sampling Error for BCa method",
  {
    skip("#539 message about loading purrr in the snapshot in R CMD check hard")
    # unskip this by moving the expectation back into the test_that block above

    set.seed(888)
    rand_nums <- rnorm(n = 1000, mean = 10, sd = 1)
    dat <- data.frame(x = rand_nums)
    set.seed(456765)
    bt_small <-
      bootstraps(dat, times = 10, apparent = TRUE) %>%
      dplyr::mutate(
        stats = purrr::map(splits, ~ get_stats(.x)),
        junk = 1:11
     )

    expect_snapshot(int_bca(bt_small, stats, .fn = get_stats))
  }
)

test_that("bad input", {
  set.seed(888)
  rand_nums <- rnorm(n = 1000, mean = 10, sd = 1)
  dat <- data.frame(x = rand_nums)
  set.seed(456765)
  bt_small <-
    bootstraps(dat, times = 10, apparent = TRUE) %>%
    dplyr::mutate(
      stats = purrr::map(splits, ~ get_stats(.x)),
      junk = 1:11
   )

  expect_snapshot(error = TRUE, {
    int_pctl(bt_small, id)
  })
  expect_snapshot(error = TRUE, {
    int_pctl(bt_small, junk)
  })
  expect_snapshot(error = TRUE, {
    int_pctl(bt_small, stats, alpha = c(0.05, 0.2))
  })
  expect_snapshot(error = TRUE, {
    int_t(bt_small, stats, alpha = "potato")
  })
  expect_snapshot(error = TRUE, {
    int_bca(bt_small, stats, alpha = 1:2, .fn = get_stats)
  })
  expect_snapshot(error = TRUE, {
    int_pctl(vfold_cv(mtcars))
  })
  expect_snapshot(error = TRUE, {
    int_t(vfold_cv(mtcars))
  })
  expect_snapshot(error = TRUE, {
    int_bca(vfold_cv(mtcars))
  })

  set.seed(888)
  rand_nums <- rnorm(n = 1000, mean = 10, sd = 1)
  dat <- data.frame(x = rand_nums)

  set.seed(456765)
  bt_norm <- bootstraps(dat, times = 1000, apparent = TRUE) %>%
    dplyr::mutate(
      stats = purrr::map(splits, ~ get_stats(.x))
    )

  bad_bt_norm <-
    bt_norm %>%
    mutate(stats = purrr::map(stats, ~ .x[, 1:2]))
  expect_snapshot(error = TRUE, {
    int_t(bad_bt_norm, stats)
  })

  no_dots <- function(split) {
    dat <- analysis(split)
    x <- dat[[1]]
    tibble(
      term = "mean",
      estimate = mean(x, na.rm = TRUE),
      std.error = sqrt(var(x, na.rm = TRUE) / sum(!is.na(x)))
    )
  }
  expect_snapshot(error = TRUE, {
    int_bca(bt_norm, stats, .fn = no_dots)
  })

  expect_snapshot(error = TRUE, {
    int_pctl(as.data.frame(bt_norm), stats)
  })
  expect_snapshot(error = TRUE, {
    int_t(as.data.frame(bt_norm), stats)
  })
  expect_snapshot(error = TRUE, {
    int_bca(as.data.frame(bt_norm), stats, .fn = get_stats)
  })

  expect_snapshot(error = TRUE, {
    int_t(bt_norm %>% dplyr::filter(id != "Apparent"), stats)
  })
  expect_snapshot(error = TRUE, {
    int_bca(bt_norm %>% dplyr::filter(id != "Apparent"), stats, .fn = get_stats)
  })

  poo <- function(x) {
    x$estimate <- "a"
    x
  }
  badder_bt_norm <-
    bt_norm %>%
    mutate(
      bad_term = purrr::map(stats, ~ .x %>% setNames(c("a", "estimate", "std.err"))),
      bad_est = purrr::map(stats, ~ .x %>% setNames(c("term", "b", "std.err"))),
      bad_err = purrr::map(stats, ~ .x %>% setNames(c("term", "estimate", "c"))),
      bad_num = purrr::map(stats, ~ poo(.x))
    )
  expect_snapshot(error = TRUE, {
    int_pctl(badder_bt_norm, bad_term)
  })
  expect_snapshot(error = TRUE, {
    int_t(badder_bt_norm, bad_err)
  })
  expect_snapshot(error = TRUE, {
    int_bca(badder_bt_norm, bad_est, .fn = get_stats)
  })
  expect_snapshot(error = TRUE, {
    int_pctl(badder_bt_norm, bad_num)
  })
})

test_that("checks for apparent bootstrap", {
  rs_boot <- bootstraps(mtcars, times = 10, apparent = FALSE)
  expect_snapshot(error = TRUE, {
    int_t(rs_boot)
  })
  expect_snapshot(error = TRUE, {
    int_bca(rs_boot)
  })
})

# ------------------------------------------------------------------------------

test_that("compute intervals with additional grouping terms", {
  skip_if_not_installed("broom")

  lm_coefs <- function(dat) {
    mod <- lm(mpg ~ I(1/disp), data = dat)
    tidy(mod)
  }

  coef_by_engine_shape <- function(split, ...) {
    split %>%
      analysis() %>%
      dplyr::rename(.vs = vs) %>%
      tidyr::nest(.by = .vs) %>%
      dplyr::mutate(coefs = map(data, lm_coefs)) %>%
      dplyr::select(-data) %>%
      tidyr::unnest(coefs)
  }

  set.seed(270)
  boot_rs <-
    bootstraps(mtcars, 1000, apparent = TRUE) %>%
    dplyr::mutate(results = purrr::map(splits, coef_by_engine_shape))

  pctl_res <- int_pctl(boot_rs, results)
  t_res <- int_t(boot_rs, results)
  bca_res <- int_bca(boot_rs, results, .fn = coef_by_engine_shape)

  exp_ptype <-
    tibble::tibble(
      term = character(0),
      .vs = numeric(0),
      .lower = numeric(0),
      .estimate = numeric(0),
      .upper = numeric(0),
      .alpha = numeric(0),
      .method = character(0)
    )

  expect_equal(pctl_res[0, ], exp_ptype)
  expect_equal(t_res[0, ], exp_ptype)
  expect_equal(bca_res[0, ], exp_ptype)
  
  exp_combos <-
    tibble::tribble(
      ~term,         ~.vs,
      "(Intercept)",    0,
      "(Intercept)",    1,
      "I(1/disp)",      0,
      "I(1/disp)",      1
    )

  group_patterns <- function(x) {
    dplyr::distinct(x, term, .vs) %>%
      dplyr::arrange(term, .vs)
  }

  expect_equal(group_patterns(pctl_res), exp_combos)
  expect_equal(group_patterns(t_res), exp_combos)
  expect_equal(group_patterns(bca_res), exp_combos)
})

