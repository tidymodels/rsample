context("Bootstrapping Confidence Intervals - Percentile Method")

library(rsample)
library(testthat)
library(purrr)
library(tibble)
library(dplyr)
library(broom)



context("boot_ci() Check Against Standard Confidence Interval")

test_that('Bootstrap estimate of mean is close to estimate of mean from normal distribution',
          {
            n <- 10000
            mean <- 10
            sd <- 1

            set.seed(888)
            rand_nums <- rnorm(n, mean, sd)
            random_nums <- as.data.frame(rand_nums)
            ttest <- t.test(random_nums)

            results_ttest <- tibble(
              lower = min(ttest$conf.int),
              upper = max(ttest$conf.int),
              alpha = 0.05,
              method = "t-test"
            )

            get_mean <- function(dat) {
              mean(dat[['rand_nums']], na.rm = TRUE)
            }

            get_var <- function(dat) {
              var(dat[['rand_nums']], na.rm = TRUE)
            }


            bt_norm <-
              bootstraps(random_nums, times = 500, apparent = TRUE) %>%
              dplyr::mutate(
                tmean = map_dbl(splits, function(x)
                  get_mean(analysis(x))),
                tmean_var = map_dbl(splits, function(x)
                  get_var(analysis(x)))
              )




            results_mean_boot_perc <- rsample:::perc_interval(bt_norm$tmean,
                                                              alpha = 0.05)


            results_mean_boot_t <- rsample:::student_t_all(bt_norm,
                                                           tmean,
                                                           var_cols = vars(tmean_var),
                                                           alpha = 0.05)

            expect_equal(results_ttest$lower, results_mean_boot_perc$lower, tolerance = 0.01)
            expect_equal(results_ttest$lower, results_mean_boot_perc$lower, tolerance = 0.01)
            expect_equal(results_ttest$upper, results_mean_boot_perc$upper, tolerance = 0.01)

            expect_equal(results_ttest$lower, results_mean_boot_t$lower, tolerance = 0.01)
            expect_equal(results_ttest$upper, results_mean_boot_t$upper, tolerance = 0.01)
          })


context("Wrapper Functions")
test_that("Wrappers -- selection of multiple variables works", {

# Fits a linear model, then collapses the columns to get the beta and variance estimates
  wide_lm <- function(dat, variance = TRUE) {
    res <- lm(Petal.Width ~ Sepal.Length + Sepal.Width, data = dat) %>%
      broom::tidy() %>%
      # keep only coefficients of interest
      filter(term != "(Intercept)") %>%
      # change to the variance
      mutate(var = std.error^2) %>%
      select(term, var, estimate) %>%
      gather(type, value, -term) %>%
      # collapse term & type into a variable called "item"
      unite(item, term, type) %>%
      spread(item, value)

  # if we don't want the variance columns, get rid of it
  if (!variance)
    res <- res %>%
      select(-ends_with("_var"))

  return(res)
  }

  # generate boostrap resamples
  set.seed(888)
  bt_resamples <- bootstraps(iris, times = 1000, apparent = TRUE)

  # compute function across each resample
  model_res <- map_dfr(bt_resamples$splits, ~ wide_lm(analysis(.x)))

  bt_resamples <- bind_cols(bt_resamples, model_res)




  # baseline
  sepal_width_baseline <- quantile(bt_resamples$Sepal.Width_estimate,
           probs = c(0.025, 0.975))

  sepal_width_baseline <- tibble(
    lower = min(sepal_width_baseline),
    estimate = mean(sepal_width_baseline, na.rm = TRUE),
    upper = max(sepal_width_baseline),
    alpha = 0.05,
    method = "percentile baseline"
  )


  # OK - CI is reasonable
  perc_results <- rsample:::perc_all(bt_resamples,
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
    tolerance = 0.01)


  expect_equal(
    sepal_width_baseline$upper,
    perc_results %>% filter(statistic == "Sepal.Width_estimate") %>% pull(upper),
    tolerance = 0.01)


  t_results <- rsample:::student_t_all(bt_resamples,
                                       Sepal.Width_estimate,
                                       Sepal.Length_estimate,
                                       var_cols = vars(Sepal.Width_var, Sepal.Length_var),
                                       alpha = 0.05)

  expect_equal(
    sepal_width_baseline$lower,
    t_results %>% filter(statistic == "Sepal.Width_estimate") %>% pull(lower),
    tolerance = 0.01
  )

})







context("boot_ci() Prompt Errors: Too Many NAs")
test_that('Upper & lower confidence interval does not contain NA', {
  iris_na <- iris
  iris_na$Sepal.Width[c(1, 51, 101)] <- NA

  set.seed(888)
  bt_na <- bootstraps(iris_na, apparent = TRUE, times = 1000) %>%
    dplyr::mutate(tmean = rep(NA_real_, 1001))

  expect_error(rsample:::perc_interval(bt_na$tmean, alpha = 0.05))

})



context("boot_ci() Insufficient Number of Bootstrap Resamples")

get_median <- function(dat) {
  median(dat$Sepal.Length, na.rm = TRUE)
}

set.seed(888)
bt_one <- bootstraps(iris, apparent = TRUE, times = 1) %>%
  dplyr::mutate(median_sepal = map_dbl(splits, function(x)
    get_median(analysis(x))))


# TODO replace with BCA later
test_that(
  "At least B=1000 replications needed to sufficiently reduce Monte Carlo sampling Error for BCa method",
  {
    expect_warning(rsample:::perc_interval(
      bt_one$median_sepal,
      alpha = 0.05
    ))
  }
)


context("boot_ci() Input Validation")


test_that("statistic is entered", {
  expect_error(rsample:::perc_interval(bt_resamples$cat, alpha = 0.5))
})

# test for apparent=true in perc_interval


# test for apparent=true in perc_all
# test_that("bootstraps(apparent = TRUE)", {
#   get_mean <- function(dat) {
#     mean(dat[['Sepal.Length']], na.rm = TRUE)
#   }
#
#   bt_small <- bootstraps(iris, times = 500, apparent = FALSE) %>%
#     dplyr::mutate(tmean = map_dbl(splits, function(x)
#       get_mean(analysis(x))))
#   expect_error(rsample:::perc_interval(
#     bt_small$tmean,
#     alpha = 0.5
#   ))
# })

#
test_that('must enter a bootstraps object', {
  expect_error(rsample:::perc_all("lal",
                        wage_diff,
                        alpha = 0.5))
})






