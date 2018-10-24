library(tidyverse)
library(rlang)
library(AmesHousing)

#' @importFrom tibble tibble
#' @export
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
#' @importFrom purrr map map_dfr
perc_all <- function(object, ..., alpha = 0.05) {
  columns <- select_vars(names(object), !!!quos(...))
  res <- purrr::map_dfr(object[, columns], perc_interval, alpha = alpha)
  res %>% mutate(statistic = columns)
}




