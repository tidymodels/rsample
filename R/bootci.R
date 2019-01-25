#'  Bootstrap Confidence Intervals
#' @description
#' Calculate bootstrap confidence intervals for a statistic of interest.
#' @param stats A statistic of interest, a vector, from `rsplit` object created by the `bootstraps` function
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

  # stats is a numeric vector of values
  ci <- stats %>% quantile(probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)

  # return a tibble with .lower, .estimate, .upper
  res <- tibble(
    lower = min(ci),
    estimate = mean(stats, na.rm = TRUE),
    upper = max(ci),
    alpha = alpha,
    .method = "percentile"
  )
}

#' Percentile wrapper for multiple statistics
#' @description
#' Calculate bootstrap confidence interval with percentile method
#' @param object bootstrap resamples created by the `bootstraps` function
#' @param ... parameters to pass to the specific confidence interval methods
#' @param alpha level of significance
#' @importFrom purrr map map_dfr
#' @importFrom rlang quos
#' @importFrom dplyr select_vars mutate last
#' @export
perc_all <- function(object, ..., alpha = 0.05) {

  if (class(object)[1] != "bootstraps")
    stop("Please enter a bootstraps object using the rsample package.", call. = FALSE)


  if(object %>% dplyr::filter(id == "Apparent") %>% nrow() != 1)
    stop("Please set apparent=TRUE in bootstraps() function", call. = FALSE)

  object <- object %>% dplyr::filter(id != "Apparent")

  column_stats <- select_vars(names(object), !!!quos(...))
  res <- purrr::map_dfr(object[, column_stats], perc_interval, alpha = alpha)
  res %>% mutate(.method = "percentile",
                 statistic = column_stats)
}

# t-dist low-level
#' @importFrom tibble tibble
t_interval <- function(stats, stat_var, theta_obs, var_obs, alpha = 0.05) {
  # stats is a numeric vector of values
  # vars is a numeric vector of variances
  # return a tibble with .lower, .estimate, .upper

  if(all(is.na(stats)))
    stop("All statistics (", stats, ") are missing values.", call. = FALSE)

  z_dist <-
    (stats - theta_obs) / sqrt(stat_var)

  z_pntl <-
    quantile(z_dist, probs = c(alpha / 2, 1 - (alpha) / 2), na.rm = TRUE)

  ci <- theta_obs - z_pntl * sqrt(var_obs)


  tibble(
    lower = min(ci),
    estimate = theta_obs,
    upper = max(ci),
    alpha = alpha,
    .method = "student-t"
  )
}

# student-t intermediate wrapper
#' @importFrom dplyr filter pull
#' @importFrom purrr map map_dfr
#' @importFrom rlang quos
t_interval_wrapper <- function(stat_name, var_name, dat, alpha){

  theta_obs <- dat %>% filter(id == "Apparent") %>% pull(stat_name)
  var_obs <- dat %>% filter(id == "Apparent")%>% pull(var_name)

  stats <- dat %>% filter(id != "Apparent") %>% pull(stat_name)
  stat_var <- dat %>% filter(id != "Apparent")%>% pull(var_name)

  t_interval(stats, stat_var, theta_obs, var_obs, alpha)

}


#' Student-t wrapper for multiple statistics
#' @description
#' Calculate bootstrap confidence interval with student-t method
#' @param object bootstrap resamples created by the `bootstraps` function
#' @param ... parameters to pass to the specific confidence interval methods
#' @param var_cols variance variables for statistics of interest. Must be quoted `vars(variance1, variance2)``
#' @param alpha level of significance
#' @importFrom dplyr select_vars as_tibble mutate
#' @importFrom rlang quos
#' @importFrom purrr map2 map_dfr
#' @export
student_t_all <- function(object, ..., var_cols, alpha = 0.05) {

  if (class(object)[1] != "bootstraps")
    stop("Please enter a bootstraps object using the rsample package.", call. = FALSE)


  if(object %>% dplyr::filter(id == "Apparent") %>% nrow() != 1)
    stop("`apparent = TRUE` in bootstraps() function", call. = FALSE)

  if (nrow(object) < 500)
    warning("Recommend at least 500 bootstrap resamples.", call. = FALSE)


  column_stats <- select_vars(names(object), !!!quos(...))
  column_vars <-  select_vars(names(object), !!!var_cols)
  res <- purrr::map2(column_stats, column_vars, t_interval_wrapper, dat=object, alpha = alpha)


  res <- res %>%
    purrr::map_dfr(as_tibble) %>%
    mutate(statistic = column_stats,
           .method = "student-t")

}

#' BCA Interval low-level
#' @description
#' Calculate bootstrap confidence intervals for a statistic of interest.
#' @param stats A statistic of interest, a vector, from `rsplit` object created by the `bootstraps` function
#' @param stat_name name of statistic of interest?
#' @param theta_hat statistics of interest
#' @param orig_data original dataset
#' @param fn function to calculate stastic of interest
#' @param args list of arguments passed to `fn`
#' @param alpha level of significance
#' @importFrom dplyr last
#' @importFrom rlang exec
#' @importFrom purrr pluck map_dbl map_dfr
#' @importFrom stats qnorm pnorm
#' @export
bca_interval <- function(stats, stat_name, theta_hat, orig_data, fn, args, alpha = 0.05) {
  # stats is a numeric vector of values
  # splits is a vector of rsplits
  # funcs is a function
  # args is a list
  # return a tibble with .lower, .estimate, .upper



  if(all(is.na(stats)))
    stop("All statistics (", stats, ") are missing values.", call. = FALSE)


  ### Estimating Z0 bias-correction
  po <- mean(stats <= theta_hat, na.rm = TRUE)
  Z0 <- stats::qnorm(po)
  Za <- stats::qnorm(1 - alpha / 2)


  #need the original data frame here
  # loo_rs <- loo_cv(splits %>% pluck(1, "data"))
  loo_rs <- loo_cv(orig_data)

  # We can't be sure what we will get back from the analysis function.
  # To test, we run on the first LOO data set and see if it is a vector or df
  loo_test <- rlang::exec(fn, analysis(loo_rs$splits[[1]]), !!!args)

  if (is.vector(loo_test)) {
    if (length(loo_test) > 1)
      stop("The function should return a single value or a data frame/",
           "tibble.", call. = FALSE)
    leave_one_out_theta <-
        map_dbl(loo_rs$splits, ~fn(analysis(.x)))

  } else {
    if (!is.data.frame(loo_test))
      stop("The function should return a single value or a data frame/",
           "tibble.", call. = FALSE)
    leave_one_out_theta <- map_dfr(loo_rs$splits, ~fn(analysis(.x))) %>%
      pull(stat_name)

  }


  theta_minus_one <- mean(leave_one_out_theta, na.rm = TRUE)

  a <- sum( (theta_minus_one - leave_one_out_theta) ^ 3) /
    ( 6 * (sum( (theta_minus_one - leave_one_out_theta) ^ 2)) ^ (3 / 2) )

  Zu <- (Z0 + Za) / ( 1 - a * (Z0 + Za)) + Z0 # upper limit for Z
  Zl <- (Z0 - Za) / (1 - a * (Z0 - Za)) + Z0 # lower limit for Z
  lower_percentile <- stats::pnorm(Zl, lower.tail = TRUE) # percentile for Z
  upper_percentile <- stats::pnorm(Zu, lower.tail = TRUE) # percentile for Z
  ci_bca <- as.numeric(quantile(stats, c(lower_percentile, upper_percentile)))


  tibble(
    lower = min(ci_bca),
    estimate = theta_hat,
    upper = max(ci_bca),
    alpha = alpha,
    .method = "BCa"
  )
}

#' BCA Interval wrapper
#' @param stat_name statistic name
#' @param fn function to calculate statistic of interest
#' @param args list of arguments passed to `fn`
#' @param dat resamples
#' @param alpha level of significance
#' @importFrom dplyr filter pull
#' @importFrom purrr pluck
bca_interval_wrapper <- function(stat_name, fn, args, dat, alpha){

  theta_hat <- dat %>% dplyr::filter(id == "Apparent") %>% pull(stat_name)

  stats <- dat %>% dplyr::filter(id != "Apparent") %>% pull(stat_name)

  orig_data <- dat %>% dplyr::filter(id == "Apparent") %>% pluck("splits", 1, "data")

  bca_interval(stats, stat_name, theta_hat, orig_data, fn, args = args, alpha = alpha)

}


#' BCA Interval high-level
#' @description
#' Calculate bootstrap confidence intervals for a statistic of interest.
#' @param object bootstrap resamples created by the `bootstraps` function
#' @param ... parameters to pass to the specific confidence interval methods
#' @param fn function to calculate statistic of interest
#' @param args list of arguments passed to `fn`
#' @param alpha level of significance
#' @importFrom dplyr select_vars
#' @importFrom purrr map_dfr
#' @export
bca_all <- function(object, ..., fn, args=list(), alpha = 0.05){

  if (class(object)[1] != "bootstraps")
    stop("Please enter a bootstraps object using the rsample package.", call. = FALSE)


  if(object %>% dplyr::filter(id == "Apparent") %>% nrow() != 1)
    stop("Please set apparent=TRUE in bootstraps() function", call. = FALSE)


  if (nrow(object) < 1000)
    warning("Recommend at least 1000 bootstrap resamples.", call. = FALSE)

  column_stats <- select_vars(names(object), !!!quos(...))

  res <-
    purrr::map_dfr(
      column_stats,
      bca_interval_wrapper,
      fn,
      args = args,
      dat = object,
      alpha = alpha
    )

  res %>% mutate(.method = "BCa", statistic = column_stats)

}

#' @importFrom utils globalVariables
utils::globalVariables(c("id"))


