context("Bootstrapping Confidence Intervals - Percentile Method")

library(rsample)
library(testthat)
library(purrr)
library(tibble)
library(dplyr)



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
              bootstraps(random_nums, times = 1000, apparent = TRUE) %>%
              dplyr::mutate(
                tmean = map_dbl(splits, function(x)
                  get_mean(analysis(x))),
                tmean_var = map_dbl(splits, function(x)
                  get_var(analysis(x)))
              )

            results_mean_boot_perc <- rsample:::perc_interval(bt_norm$tmean, alpha = 0.05)

            expect_equal(results_ttest$lower, results_mean_boot_perc$lower, tolerance = 0.01)
            expect_equal(results_ttest$upper, results_mean_boot_perc$upper, tolerance = 0.01)
          })
