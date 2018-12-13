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


  if (!is.numeric(stats))
    stop("`stats` must be a numeric vector.", call. = FALSE)


  #
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
#' @importFrom dplyr select_vars mutate last
#' @export
perc_all <- function(object, ..., alpha = 0.05) {

  if (class(object)[1] != "bootstraps")
    stop("Please enter a bootstraps object using the rsample package.", call. = FALSE)

  if(object %>% dplyr::filter(id == "Apparent") %>% nrow() != 1)
    stop("Please set apparent=TRUE in bootstraps() function", call. = FALSE)


  columns <- select_vars(names(object), !!!quos(...))
  res <- purrr::map_dfr(object[, columns], perc_interval, alpha = alpha)
  res %>% mutate(statistic = columns)
}




# t-dist low-level api
t_interval <- function(stats, stat_var, theta_obs, var_obs, alpha = 0.05) {
  # stats is a numeric vector of values
  # vars is a numeric vector of variances
  # return a tibble with .lower, .estimate, .upper

  # T.b = (theta.i-theta.obs)/(sd(boot.sample)/sqrt(length(boot.sample)))
  # z_dist <-
  #   (bt_resamples[[stat]] - theta_obs) / sqrt(bt_resamples[[stat_var]])

  z_dist <-
    (stats - theta_obs) / sqrt(stat_var)

  z_pntl <-
    quantile(z_dist, probs = c(alpha / 2, 1 - (alpha) / 2), na.rm = TRUE)

  ci <- theta_obs - z_pntl * sqrt(var_obs)


  tibble(
    lower = min(ci),
    upper = max(ci),
    alpha = alpha,
    method = "bootstrap-t"
  )
}

# student-t wrapper
student_t_all <- function(object, ..., alpha = 0.05) {
  columns <- select_vars(names(object), !!!quos(...))
  purrr::map(object[, columns], t_interval, alpha = alpha)
}
