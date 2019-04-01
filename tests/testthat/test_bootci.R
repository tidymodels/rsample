library(rsample)
library(testthat)
library(purrr)
library(tibble)
library(dplyr)
library(broom)

context("Bootstrap intervals")

# ------------------------------------------------------------------------------

get_mean <- function(data) {
  data %>%
    pull(1) %>%
    mean(na.rm = TRUE)
}

get_var <- function(data) {
  n <- nrow(data)
  data %>%
    pull(1) %>%
    var(na.rm = TRUE)/n
}

# ------------------------------------------------------------------------------

test_that('Bootstrap estimate of mean is close to estimate of mean from normal distribution',{
  n <- 10000
  mu <- 10
  sigma <- 1

  set.seed(888)
  rand_nums <- rnorm(n, mu, sigma)
  random_nums <- as.data.frame(rand_nums)
  ttest <- t.test(random_nums)

  results_ttest <-
    tibble(
    lower = min(ttest$conf.int),
    upper = max(ttest$conf.int),
    alpha = 0.05,
    method = "t-test"
  )

  bt_norm <-
    bootstraps(random_nums, times = 1000, apparent = TRUE) %>%
    dplyr::mutate(
      tmean = map_dbl(splits,  ~ get_mean(analysis(.x))),
      tmean_var = map_dbl(splits, ~ get_var(analysis(.x)))
    )

  results_mean_boot_perc <- rsample:::pctl_single(bt_norm$tmean)

  results_mean_boot_t <- int_t(bt_norm, tmean, var_cols = vars(tmean_var))

  results_mean_boot_bca <- int_bca(bt_norm, tmean, fn = get_mean)

  expect_equal(results_ttest$lower,
               results_mean_boot_perc$lower,
               tolerance = 0.01)
  expect_equal(results_ttest$lower,
               results_mean_boot_perc$lower,
               tolerance = 0.01)
  expect_equal(results_ttest$upper,
               results_mean_boot_perc$upper,
               tolerance = 0.01)

  expect_equal(results_ttest$lower, results_mean_boot_t$lower, tolerance = 0.01)
  expect_equal(results_ttest$upper, results_mean_boot_t$upper, tolerance = 0.01)

  expect_equal(results_ttest$lower, results_mean_boot_bca$lower, tolerance = 0.01)
  expect_equal(results_ttest$upper, results_mean_boot_bca$upper, tolerance = 0.01)
})

# ------------------------------------------------------------------------------

context("Wrapper Functions")

test_that("Wrappers -- selection of multiple variables works", {
  # Fits a linear model, then collapses the columns to get the beta and variance estimates
  wide_lm <- function(data, variance = TRUE) {
    res <-
      lm(Petal.Width ~ Sepal.Length + Sepal.Width, data = data) %>%
      broom::tidy() %>%
      # keep only coefficients of interest
      dplyr::filter(term != "(Intercept)") %>%
      # change to the variance
      mutate(var = std.error ^ 2) %>%
      dplyr::select(term, var, estimate) %>%
      gather(type, value,-term) %>%
      # collapse term & type into a variable called "item"
      unite(item, term, type) %>%
      spread(item, value)

    # if we don't want the variance columns, get rid of it
    if (!variance)
      res <- res %>%
        dplyr::select(-ends_with("_var"))

    return(res)
  }

  # generate boostrap resamples
  set.seed(888)
  bt_resamples <- bootstraps(iris, times = 1000, apparent = TRUE)

  # compute function across each resample
  model_res <-
    map_dfr(bt_resamples$splits, ~ wide_lm(analysis(.x), variance = TRUE))

  bt_resamples <- bind_cols(bt_resamples, model_res)

  # baseline
  sepal_width_baseline <-
    quantile(bt_resamples$Sepal.Width_estimate,
             probs = c(0.025, 0.975))

  sepal_width_baseline <- tibble(
    lower = min(sepal_width_baseline),
    estimate = mean(sepal_width_baseline, na.rm = TRUE),
    upper = max(sepal_width_baseline),
    alpha = 0.05,
    method = "percentile baseline"
  )

  # OK - CI is reasonable
  perc_results <- int_pctl(bt_resamples,
                           Sepal.Width_estimate,
                           Sepal.Length_estimate,
                           alpha = 0.05)

  expect_equal(
    sepal_width_baseline$lower,
    perc_results %>% filter(statistic == "Sepal.Width_estimate") %>% pull(lower),
    tolerance = 0.01
  )

  expect_equal(
    sepal_width_baseline$estimate,
    perc_results %>% filter(statistic == "Sepal.Width_estimate") %>% pull(estimate),
    tolerance = 0.01
  )

  expect_equal(
    sepal_width_baseline$upper,
    perc_results %>% filter(statistic == "Sepal.Width_estimate") %>% pull(upper),
    tolerance = 0.01
  )

  t_results <- int_t(
    bt_resamples,
    Sepal.Width_estimate,
    Sepal.Length_estimate,
    var_cols = vars(Sepal.Width_var, Sepal.Length_var),
    alpha = 0.05
  )

  expect_equal(
    sepal_width_baseline$lower,
    t_results %>% filter(statistic == "Sepal.Width_estimate") %>% pull(lower),
    tolerance = 0.01
  )

  #  High-level BCa call
  bca_results <- int_bca(
    bt_resamples,
    Sepal.Width_estimate,
    Sepal.Length_estimate,
    fn = wide_lm,
    args = list(variance = FALSE),
    alpha = 0.05
  )

  expect_equal(
    sepal_width_baseline$lower,
    bca_results %>% filter(statistic == "Sepal.Width_estimate") %>% pull(lower),
    tolerance = 0.01
  )

})

# ------------------------------------------------------------------------------

context("boot_ci() Prompt Errors: Too Many NAs")

test_that('Upper & lower confidence interval does not contain NA', {
  iris_na <- iris
  iris_na$Sepal.Width[c(1, 51, 101)] <- NA

  set.seed(888)
  bt_na <- bootstraps(iris_na, apparent = TRUE, times = 1000) %>%
    dplyr::mutate(tmean = rep(NA_real_, 1001),
                  tvar = rep(NA_real_, 1001))

  expect_error(rsample:::pctl_single(bt_na$tmean, alpha = 0.05))

  expect_error(rsample:::t_single(bt_na, tmean, var_cols = vars(tvar), alpha = 0.1))

  expect_error(int_bca(bt_na, tmean, fn = median, alpha = 0.05))

})

# ------------------------------------------------------------------------------

context("boot_ci() Insufficient Number of Bootstrap Resamples")

set.seed(888)
bt_one <- bootstraps(iris, apparent = TRUE, times = 1) %>%
  dplyr::mutate(
    mean_sepal = map_dbl(splits, ~ get_mean(analysis(.x))),
    mean_sepal_var = map_dbl(splits, ~ get_var(analysis(.x)))
  )


# TODO replace with BCA later
test_that(
  "Sufficient replications needed to sufficiently reduce Monte Carlo sampling Error for BCa method", {

    expect_warning(
      rsample:::pctl_single(bt_one$mean_sepal, alpha = 0.05)
    )

    expect_warning(
      int_t(
        bt_one,
        mean_sepal,
        var_cols = vars(mean_sepal_var),
        alpha = 0.05
      )
    )

    expect_warning(
      int_bca(
        bt_one,
        mean_sepal,
        fn = mean,
        args = list(na.rm = TRUE, trim = 0.1),
        alpha = 0.05
      )
    )

  }
)


context("boot_ci() Input Validation")


test_that("statistic is entered", {
  expect_error(rsample:::pctl_single(bt_resamples$cat, alpha = 0.5))
})


test_that("bootstraps(apparent = TRUE)", {

  bt_without_apparent <-
    bootstraps(iris, times = 500, apparent = FALSE) %>%
    dplyr::mutate(
      tmean = map_dbl(splits, ~ get_mean(analysis(.x))),
      tmean_var = map_dbl(splits, ~ get_var(analysis(.x)))
    )

  expect_error(int_pctl(bt_without_apparent$tmean, alpha = 0.5))

  expect_error(
    int_t(
      bt_without_apparent,
      tmean,
      var_cols = vars(tmean_var),
      alpha = 0.5
    ),
    "`apparent = TRUE`"
  )

  expect_error(
    int_bca(
      bt_without_apparent,
      tmean,
      fn = ~ get_mean(analysis(.x)),
      alpha = 0.1
    ),
    "`apparent = TRUE`"
  )
})

test_that('must enter a bootstraps object', {
  expect_error(int_pctl("lal", startrek, alpha = 0.5))
  expect_error(rsample:::t_single("lal", startrek, alpha = 0.5))
})
