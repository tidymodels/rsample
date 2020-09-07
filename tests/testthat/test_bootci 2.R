library(rsample)
library(testthat)
library(purrr)
library(tibble)
library(dplyr)
library(broom)

data("attrition", package = "modeldata")

context("Bootstrap intervals")

# ------------------------------------------------------------------------------

get_stats <- function(split, ...) {
  dat <- analysis(split)
  x <- dat[[1]]
  tibble(
    term = "mean",
    estimate = mean(x, na.rm = TRUE),
    std.error = sqrt(var(x, na.rm = TRUE)/sum(!is.na(x)))
  )
}

# ------------------------------------------------------------------------------

n <- 1000
mu <- 10
sigma <- 1

set.seed(888)
rand_nums <- rnorm(n, mu, sigma)
ttest <- tidy(t.test(rand_nums))
ttest_lower_conf <- tidy(t.test(rand_nums, conf.level = 0.8))
dat <- data.frame(x = rand_nums)

set.seed(456765)
bt_norm <-
  bootstraps(dat, times = 1000, apparent = TRUE) %>%
  dplyr::mutate(
    stats = map(splits,  ~ get_stats(.x))
  )

test_that('Bootstrap estimate of mean is close to estimate of mean from normal distribution',{
  skip_on_cran()
  single_pct_res <- int_pctl(bt_norm, stats)

  single_t_res <- int_t(bt_norm, stats)

  single_bca_res <- int_bca(bt_norm, stats, .fn = get_stats)

  single_bca_res_lower_conf <- int_bca(bt_norm, stats, .fn = get_stats, alpha = 0.2)

  expect_equal(ttest$conf.low,
               single_pct_res$.lower,
               tolerance = 0.01)
  expect_equal(unname(ttest$estimate),
               single_pct_res$.estimate,
               tolerance = 0.01)
  expect_equal(ttest$conf.high,
               single_pct_res$.upper,
               tolerance = 0.01)

  expect_equal(ttest$conf.low,
               single_t_res$.lower,
               tolerance = 0.01)
  expect_equal(unname(ttest$estimate),
               single_t_res$.estimate,
               tolerance = 0.01)
  expect_equal(ttest$conf.high,
               single_pct_res$.upper,
               tolerance = 0.01)

  expect_equal(ttest$conf.low,
               single_bca_res$.lower,
               tolerance = 0.01)
  expect_equal(unname(ttest$estimate),
               single_bca_res$.estimate,
               tolerance = 0.01)
  expect_equal(ttest$conf.high,
               single_bca_res$.upper,
               tolerance = 0.01)


  expect_equal(ttest_lower_conf$conf.low,
               single_bca_res_lower_conf$.lower,
               tolerance = 0.01)
  expect_equal(unname(ttest_lower_conf$estimate),
               single_bca_res_lower_conf$.estimate,
               tolerance = 0.01)
  expect_equal(ttest_lower_conf$conf.high,
               single_bca_res_lower_conf$.upper,
               tolerance = 0.01)
})

# ------------------------------------------------------------------------------

context("Wrapper Functions")

test_that("Wrappers -- selection of multiple variables works", {

  func <- function(split, ...) {
    lm(Age ~ HourlyRate + DistanceFromHome, data = analysis(split)) %>% tidy()
  }

  # generate boostrap resamples
  set.seed(888)
  bt_resamples <- bootstraps(attrition, times = 1000, apparent = TRUE) %>%
    mutate(res = map(splits, func))

  attrit_tidy <-
    lm(Age ~ HourlyRate + DistanceFromHome, data = attrition) %>%
    tidy(conf.int = TRUE) %>%
    dplyr::arrange(term)

  pct_res <-
    int_pctl(bt_resamples, res) %>%
    inner_join(attrit_tidy, by = "term")
  expect_equal(pct_res$conf.low,  pct_res$.lower, tolerance = .01)
  expect_equal(pct_res$conf.high, pct_res$.upper, tolerance = .01)


  t_res <-
    int_t(bt_resamples, res) %>%
    inner_join(attrit_tidy, by = "term")
  expect_equal(t_res$conf.low,  t_res$.lower, tolerance = .01)
  expect_equal(t_res$conf.high, t_res$.upper, tolerance = .01)


  bca_res <-
    int_bca(bt_resamples, res, .fn = func) %>%
    inner_join(attrit_tidy, by = "term")
  expect_equal(bca_res$conf.low,  bca_res$.lower, tolerance = .01)
  expect_equal(bca_res$conf.high, bca_res$.upper, tolerance = .01)

})

# ------------------------------------------------------------------------------

context("boot_ci() Prompt Errors: Too Many NAs")

test_that('Upper & lower confidence interval does not contain NA', {

  bad_stats <- function(split, ...) {
    tibble(
      term = "mean",
      estimate = NA_real_,
      std.error = runif(1)
    )
  }

  set.seed(888)
  bt_resamples <- bootstraps(data.frame(x = 1:100), times = 1000, apparent = TRUE) %>%
    mutate(res = map(splits, bad_stats))

  expect_error(
    expect_warning(
      int_pctl(bt_resamples, res),
      "at least 1000 non-missing"
    ),
    "missing values"
  )

  expect_error(
    expect_warning(
      int_t(bt_resamples, res),
      "at least 1000 non-missing"
    ),
    "missing values"
  )

  expect_error(
    expect_warning(
      int_bca(bt_resamples, res, .fn = bad_stats),
      "at least 1000 non-missing"
    ),
    "missing values"
  )

})

# ------------------------------------------------------------------------------

context("boot_ci() Insufficient Number of Bootstrap Resamples")

set.seed(456765)
bt_small <-
  bootstraps(dat, times = 10, apparent = TRUE) %>%
  dplyr::mutate(
    stats = map(splits,  ~ get_stats(.x)),
    junk = 1:11
  )

test_that(
  "Sufficient replications needed to sufficiently reduce Monte Carlo sampling Error for BCa method", {

    expect_warning(int_pctl(bt_small, stats))
    expect_warning(int_t(bt_small, stats))
    expect_warning(int_bca(bt_small, stats, .fn = get_stats))

  }
)


context("boot_ci() Input Validation")


test_that("bad input", {
  expect_error(int_pctl(bt_small, id))
  expect_error(int_pctl(bt_small, junk))

  bad_bt_norm <-
    bt_norm %>%
    mutate(stats = map(stats, ~ .x[, 1:2]))
  expect_error(int_t(bad_bt_norm, stats))

  expect_error(int_bca(bad_bt_norm, stats))

  no_dots <- function(split) {
    dat <- analysis(split)
    x <- dat[[1]]
    tibble(
      term = "mean",
      estimate = mean(x, na.rm = TRUE),
      std.error = sqrt(var(x, na.rm = TRUE)/sum(!is.na(x)))
    )
  }
  expect_error(
    int_bca(bt_norm, stats, .fn = no_dots),
    "must have an argument"
  )

  expect_error(int_pctl(as.data.frame(bt_norm), stats))
  expect_error(int_t(as.data.frame(bt_norm), stats))
  expect_error(int_bca(as.data.frame(bt_norm), stats, .fn = get_stats))

  expect_error(
    int_t(bt_norm %>% dplyr::filter(id != "Apparent"), stats)
    )
  expect_error(
    int_bca(bt_norm %>% dplyr::filter(id != "Apparent"), stats, .fn = get_stats)
  )

  poo <- function(x) {
    x$estimate <- "a"
    x
  }
  badder_bt_norm <-
    bt_norm %>%
    mutate(
      bad_term = map(stats, ~ .x %>% setNames(c("a", "estimate", "std.err"))),
      bad_est = map(stats, ~ .x %>% setNames(c("term", "b", "std.err"))),
      bad_err = map(stats, ~ .x %>% setNames(c("term", "estimate", "c"))),
      bad_num = map(stats, ~ poo(.x))
      )
  expect_error(int_pctl(badder_bt_norm, bad_term))
  expect_error(int_t(badder_bt_norm, bad_err))
  expect_error(int_bca(badder_bt_norm, bad_est, .fn = get_stats))
  expect_error(int_pctl(badder_bt_norm, bad_num))

})
