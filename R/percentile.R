library(rsample)
library(tidyverse)
library(rlang)
library(AmesHousing)

# percentile low-level api
perc_interval <- function(stats, alpha = 0.05) {

  # stats is a numeric vector of values
  ci <- stats %>% quantile(probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)

  # return a tibble with .lower, .estimate, .upper
  # options(pillar.sigfig = 3)
  tibble(
    lower = min(ci),
    estimate = mean(stats, na.rm = TRUE),
    upper = max(ci),
    alpha = alpha,
    method = "percentile"
  )
}

# percentile wrapper
perc_all <- function(object, ..., alpha = 0.05) {
  columns <- select_vars(names(object), !!!quos(...))
  res <- purrr::map_dfr(object[, columns], perc_interval, alpha = alpha)
  res %>% mutate(statistic = columns)
}


# generate boostrap resamples
data("attrition")
set.seed(353)
bt_resamples <- bootstraps(attrition, times = 1000)


# stat of interest
median_diff <- function(splits) {
  x <- analysis(splits)
  median(x$MonthlyIncome[x$Gender == "Female"]) -
    median(x$MonthlyIncome[x$Gender == "Male"])
}


# compute function across each resample
bt_resamples$wage_diff <- map_dbl(bt_resamples$splits, median_diff)


# stat of interest
get_trimmed_mean <- function(splits, stat_name, ...) {
  df <- analysis(splits)
  # columns <- select_vars(names(object), !!!quos(...))
  # purrr::map(object[, stat_name], perc_interval, alpha = alpha)
  mean(df[, stat_name], na.rm = TRUE, trim = 0.1)
}


bt_resamples$trim_mean <- map_dbl(bt_resamples$splits, get_trimmed_mean, 0.15)


# baseline
quantile(bt_resamples$wage_diff,
         probs = c(0.025, 0.975))


# OK - CI is reasonable
# but how do I get tibble to print a reasonable number trailing digits?? options(tibble) line 9
perc_results <- perc_all(bt_resamples,
                         wage_diff,
                         alpha = 0.05)
perc_results


# Now let's try to add additional arguments to test if (...) actually works
# okay the correct vars were selected on a wider df
perc_all(bt_resamples,
         trim_mean,
         alpha = 0.05)

# check - are the results practical? Looks reasonable.
mean(attrition$MonthlyIncome, na.rm = TRUE, trim = 0.15)

