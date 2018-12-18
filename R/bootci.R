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

# student-t call
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



