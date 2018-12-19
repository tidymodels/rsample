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
  tibble(
    lower = min(ci),
    estimate = mean(stats, na.rm = TRUE),
    upper = max(ci),
    alpha = alpha,
    method = "percentile"
  )
}

#' Percentile wrapper for multiple statistics
#' @description
#' Calculate bootstrap confidence intervals for a statistic of interest.
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
    upper = max(ci),
    alpha = alpha,
    method = "student-t"
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
#' Calculate bootstrap confidence intervals for a statistic of interest.
#' @importFrom dplyr select_vars as_tibble mutate
#' @importFrom rlang quos
#' @importFrom purrr map2 map_dfr
#' @export
student_t_all <- function(object, ..., var_cols, alpha = 0.05) {

  if (class(object)[1] != "bootstraps")
    stop("Please enter a bootstraps object using the rsample package.", call. = FALSE)

  if(object %>% dplyr::filter(id == "Apparent") %>% nrow() != 1)
    stop("Please set apparent=TRUE in bootstraps() function", call. = FALSE)

  if (nrow(object) < 500)
    warning("Recommend at least 500 bootstrap resamples.", call. = FALSE)


  column_stats <- select_vars(names(object), !!!quos(...))
  column_vars <-  select_vars(names(object), !!!var_cols)
  res <- purrr::map2(column_stats, column_vars, t_interval_wrapper, dat=object, alpha = alpha)


  res <- res %>% purrr::map_dfr(as_tibble) %>% mutate(statistic = column_stats)

}

#' BCA Interval low-level
#' @description
#' Calculate bootstrap confidence intervals for a statistic of interest.
#' @importFrom dplyr last
#' @importFrom rlang exec
#' @importFrom purrr pluck map_dbl map_dfr
#' @export
bca_interval <- function(stats, splits, fn, args = list(), alpha = 0.05) {
  # stats is a numeric vector of values
  # splits is a vector of rsplits
  # funcs is a function
  # args is a list
  # return a tibble with .lower, .estimate, .upper

  #TODO get stats, theta_hat, call bca_interval from an intermediate wrapper
  # from APPARENT = TRU
  theta_hat <- stats %>% last()

  #TODO get x_i from a intermediate wrapper
  ### Estimating Z0 bias-correction
  po <- mean(head(stats,-1) <= theta_hat, na.rm = TRUE)
  Z0 <- qnorm(po)
  Za <- qnorm(1 - alpha / 2)

  #TODO parse splits from bt_rsamples in intermediate wrapper?
  loo_rs <- loo_cv(splits %>% pluck(1, "data"))

  #TODO Do we still need this type-check? (Since we will have an intermediate wrapper?)
  # Or should I create additional intermediate wrapper for this check?

  # We can't be sure what we will get back from the analysis function.
  # To test, we run on the first LOO data set and see if it is a vector or df


  loo_test <- rlang::exec(fn, analysis(loo_rs$splits[[1]]), !!!args)

  # TODO apply rlang::exec to each loo_rs$split
  if (is.vector(loo_test)) {
    if (length(loo_test) > 1)
      stop("The function should return a single value or a data frame/",
           "tibble.", call. = FALSE)
    leave_one_out_theta <-
        map_dbl(loo_rs$splits, ~fn(analysis(.x)))
    # leave_one_out_theta <-
    #   future_map_dbl(loo_rs$splits, function(x) fn(analysis(x), ...))

  } else {
    if (!is.data.frame(loo_test))
      stop("The function should return a single value or a data frame/",
           "tibble.", call. = FALSE)
    leave_one_out_theta <- map_dfr(loo_rs$splits, ~fn(analysis(.x)))
    # leave_one_out_theta <-
    # future_map_dfr(loo_rs$splits, function(x) fn(analysis(x), ...))[[stats]]

  }

# TODO evaluate leave_one_out_theta using rlang::exec
  # leave_one_out_theta <-
        # map_dfr(loo_rs %>% pull(splits), function(x) fn(analysis(x), ...))
  # map_dfr(loo_rs %>% pull(splits), function(x) fn(analysis(x), ...))[[stats]]
  # leave_one_out_theta <- map_dfr(loo_rs, function)


# TODO leave_one_out_theta needs to be a numeric vector -- not a df
# TODO get correct stat from intermediate wrapper
  # theta_minus_one <- mean(leave_one_out_theta, na.rm = TRUE)
  theta_minus_one <- mean(leave_one_out_theta %>% pull(Sepal.Width_estimate), na.rm = TRUE)
  a <- sum( (theta_minus_one - leave_one_out_theta) ^ 3) /
    ( 6 * (sum( (theta_minus_one - leave_one_out_theta) ^ 2)) ^ (3 / 2) )

  Zu <- (Z0 + Za) / ( 1 - a * (Z0 + Za)) + Z0 # upper limit for Z
  Zl <- (Z0 - Za) / (1 - a * (Z0 - Za)) + Z0 # lower limit for Z
  lower_percentile <- pnorm(Zl, lower.tail = TRUE) # percentile for Z
  upper_percentile <- pnorm(Zu, lower.tail = TRUE) # percentile for Z
  ci_bca <- as.numeric(quantile(stats, c(lower_percentile, upper_percentile)))


  tibble(
    lower = min(ci_bca),
    upper = max(ci_bca),
    alpha = alpha,
    method = "BCa"
  )


}

# TODO mini-test
# bca_int<- function(stats, splits, fn, args = list(), alpha = 0.05) {
#   # stats is a numeric vector of values
#   # splits is a vector of rsplits
#   # fn is a function
#   # args is a list
#   # return a tibble with .lower, .estimate, .upper
#
#   # evaluates the function on the stats numeric vector using list of args
#   # We will use this to evaluate the function on each LOO rplit
#   rlang::exec(fn, stats, !!!args)
#
# }
#
# bca_int(c(1:10, 100), fn = mean, args = list(trim = .1))
# bca_int(c(1:10, 100), fn = mean)
#
#
#
#
# bca_int(
#   object,
#   mean_value,
#   funcs = mean,
#   args = list(trim = .1)
# )

# TODO Will this be high-level wrapper?
# Or should user filter for splits & stats by themselves?
bca_all <- function(object, ..., alpha = 0.05){
  # stats is a numeric vector of values
  # splits is a vector of rsplits
  # funcs is a function
  # args is a list
  # return a tibble with .lower, .estimate, .upper

  column_stats <- select_vars(names(object), !!!quos(...))





  res <- rlang::exec(funcs, stats, !!!args)

  # TODO append statistic column
  res %>% mutate(statistic = columns)

}



