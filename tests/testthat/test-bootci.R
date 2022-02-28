n <- 1000
mu <- 10
sigma <- 1

set.seed(888)
rand_nums <- rnorm(n, mu, sigma)
ttest <- broom::tidy(t.test(rand_nums))
ttest_lower_conf <- broom::tidy(t.test(rand_nums, conf.level = 0.8))
dat <- data.frame(x = rand_nums)

set.seed(456765)
bt_norm <-
  bootstraps(dat, times = 1000, apparent = TRUE) %>%
  dplyr::mutate(
    stats = purrr::map(splits, ~ get_stats(.x))
  )

test_that("Bootstrap estimate of mean is close to estimate of mean from normal distribution", {
  skip_on_cran()
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

set.seed(456765)
bt_small <-
  bootstraps(dat, times = 10, apparent = TRUE) %>%
  dplyr::mutate(
    stats = purrr::map(splits, ~ get_stats(.x)),
    junk = 1:11
  )

test_that(
  "Sufficient replications needed to sufficiently reduce Monte Carlo sampling Error for BCa method",
  {
    expect_warning(int_pctl(bt_small, stats))
    expect_warning(int_t(bt_small, stats))
    expect_warning(int_bca(bt_small, stats, .fn = get_stats))
  }
)


test_that("bad input", {
  expect_error(int_pctl(bt_small, id))
  expect_error(int_pctl(bt_small, junk))
  expect_error(int_pctl(bt_small, stats, alpha = c(0.05, 0.2)))
  expect_error(int_t(bt_small, stats, alpha = "potato"))
  expect_error(int_bca(bt_small, stats, alpha = 1:2, .fn = get_stats))


  bad_bt_norm <-
    bt_norm %>%
    mutate(stats = purrr::map(stats, ~ .x[, 1:2]))
  expect_error(int_t(bad_bt_norm, stats))

  expect_error(int_bca(bad_bt_norm, stats))

  no_dots <- function(split) {
    dat <- analysis(split)
    x <- dat[[1]]
    tibble(
      term = "mean",
      estimate = mean(x, na.rm = TRUE),
      std.error = sqrt(var(x, na.rm = TRUE) / sum(!is.na(x)))
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
      bad_term = purrr::map(stats, ~ .x %>% setNames(c("a", "estimate", "std.err"))),
      bad_est = purrr::map(stats, ~ .x %>% setNames(c("term", "b", "std.err"))),
      bad_err = purrr::map(stats, ~ .x %>% setNames(c("term", "estimate", "c"))),
      bad_num = purrr::map(stats, ~ poo(.x))
    )
  expect_error(int_pctl(badder_bt_norm, bad_term))
  expect_error(int_t(badder_bt_norm, bad_err))
  expect_error(int_bca(badder_bt_norm, bad_est, .fn = get_stats))
  expect_error(int_pctl(badder_bt_norm, bad_num))
})


# ------------------------------------------------------------------------------

test_that("regression intervals", {
  skip_on_cran()

  expect_error(
    {
      set.seed(1)
      int_1 <- reg_intervals(mpg ~ disp + wt, data = mtcars)
    },
    regex = NA
  )

  expect_equal(
    names(int_1),
    c("term", ".lower", ".estimate", ".upper", ".alpha", ".method")
  )

  expect_snapshot({
    set.seed(123)
    int_2 <- reg_intervals(
      mpg ~ disp + wt,
      data = mtcars,
      filter = term == "wt",
      model_fn = "glm",
      keep_reps = TRUE
    )
    int_2
  })

  expect_equal(
    names(int_2),
    c("term", ".lower", ".estimate", ".upper", ".alpha", ".method", ".replicates")
  )
  expect_true(nrow(int_2) == 1)
  expect_true(all(int_2$term == "wt"))


  expect_error(
    reg_intervals(mpg ~ disp + wt, data = mtcars, model_fn = "potato"),
    "`model_fn` must be one of"
  )
  expect_error(
    reg_intervals(mpg ~ disp + wt, data = mtcars, type = "random"),
    "`type` must be one of"
  )
  expect_error(
    reg_intervals(mpg ~ disp + wt, data = mtcars, alpha = "a"),
    "must be a single numeric value"
  )
})
