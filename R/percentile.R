# library(tidyverse)
# library(rlang)
# library(AmesHousing)


#'  Bootstrap Confidence Intervals
#' @description
#' Calculate bootstrap confidence intervals for a statistic of interest.
#' @param stat A statistic of interest, a vector, from `rsplit` object created by the `bootstraps` function
#' @param alpha level of significance
#' @importFrom tibble tibble
#' @export
perc_interval <- function(stats, alpha = 0.05) {


  if(all(is.na(stats)))
    stop("All statistics (", stats, ") are missing values.", call. = FALSE)

  if (length(stats) < 500)
    warning("Recommend at least 500 bootstrap resamples.", call. = FALSE)

  # if (class(bt_resamples)[1] != "bootstraps")
  #   stop("Please enter a bootstraps sample using the rsample package.", call. = FALSE)

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
#' @importFrom rlang quos
#' @importFrom dplyr select_vars mutate
perc_all <- function(object, ..., alpha = 0.05) {
  columns <- select_vars(names(object), !!!quos(...))
  res <- purrr::map_dfr(object[, columns], perc_interval, alpha = alpha)
  res %>% mutate(statistic = columns)
}




