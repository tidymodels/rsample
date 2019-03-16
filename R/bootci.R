pctl_single <- function(stats, alpha = 0.05) {

  if(all(is.na(stats)))
    stop("All statistics (", stats, ") are missing.", call. = FALSE)

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
  res
}

#' Bootstrap confidence intervals
#' @description
#' Calculate bootstrap confidence intervals using various methods.
#' @param object Bootstrap resamples created by the `bootstraps`
#'  function. For t- and BCa-intervals, the `apparent` argument
#'  should be set to `TRUE`.
#' @param ... One or more unquoted expressions separated by commas
#'  that select columns containing individual bootstrap estimates.
#'  You can treat variable names like they are positions.
#' @param alpha Level of significance
#' @return Each function returns a tibble with columns `lower`,
#'  `estimate`, `upper`, `alpha`, .`method`, and `statistics`.
#'  `.method` is the type of interval (eg. "percentile",
#'  "student-t", or "BCa"). `statistic` is the name of the
#'  column being analyzed.
#' @details Percentile intervals are the standard method of
#'  obtaining confidence intervals but require thousands of
#'  resamples to be accurate. t-intervals can require fewer
#'  resamples but required a corresponding variance estimate is
#'  required. Bias-correct intervals require the original function
#'  that was used to create the statistics of interest and are
#'  computationally taxing
#'
#' @references Davison, A., & Hinkley, D. (1997). _Bootstrap Methods and their
#'  Application_. Cambridge: Cambridge University Press.
#'  doi:10.1017/CBO9780511802843
#'
#' @importFrom purrr map map_dfr
#' @importFrom rlang quos
#' @importFrom dplyr select_vars mutate last
#' @export
int_pctl <- function(object, ..., alpha = 0.05) {

  check_rset(object)

  object <- object %>% dplyr::filter(id != "Apparent")

  column_stats <- select_vars(names(object), !!!quos(...))
  res <- purrr::map_dfr(object[, column_stats], pctl_single, alpha = alpha)
  res %>% mutate(.method = "percentile",
                 statistic = column_stats)
}

# ----------------------------------------------------------------

#' @importFrom tibble tibble
t_single <- function(stats, stat_var, theta_obs, var_obs, alpha = 0.05) {
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
    estimate = mean(stats, na.rm = TRUE),
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

  t_single(stats, stat_var, theta_obs, var_obs, alpha)
}


#' @rdname int_pctl
#' @inheritParams int_pctl
#' @param var_cols One or more unquoted expressions separated by commas
#'  that select columns containing individual bootstrap estimate
#'  of _variance_ for the statistics. These selections can be
#'  wrapped in [dplyr::vars()]. It is assumed that the order of
#'  these columns is in the same order as the statistics selected
#'  by `...`.
#' @importFrom dplyr select_vars as_tibble mutate
#' @importFrom rlang quos
#' @importFrom purrr map2 map_dfr
#' @export
int_t <- function(object, ..., var_cols, alpha = 0.05) {

  check_rset(object)

  if (nrow(object) < 500)
    warning("Recommend at least 500 bootstrap resamples.",
            call. = FALSE)

  column_stats <- select_vars(names(object), !!!quos(...))
  column_vars <-  select_vars(names(object), !!!var_cols)
  res <-
    purrr::map2(column_stats,
                column_vars,
                t_interval_wrapper,
                dat = object,
                alpha = alpha)

  res <- res %>%
    purrr::map_dfr(as_tibble) %>%
    mutate(statistic = column_stats,
           .method = "student-t")
  res

}

# ----------------------------------------------------------------

#' @importFrom dplyr last
#' @importFrom rlang exec
#' @importFrom purrr pluck map_dbl map_dfr
#' @importFrom stats qnorm pnorm
bca_single <- function(stats, stat_name, theta_hat, orig_data, fn, args, alpha = 0.05) {

  if(all(is.na(stats)))
    stop("All statistics (", stats, ") are missing.", call. = FALSE)

  ### Estimating Z0 bias-correction
  po <- mean(stats <= theta_hat, na.rm = TRUE)
  Z0 <- stats::qnorm(po)
  Za <- stats::qnorm(1 - alpha / 2)

  #need the original data frame here
  # loo_rs <- loo_cv(splits %>% pluck(1, "data"))
  loo_rs <- loo_cv(orig_data)

  # We can't be sure what we will get back from the analysis function.
  # To test, we run on the first LOO data set and see if it is a vector or df
  loo_test <- try(rlang::exec(fn, loo_rs$splits[[1]], !!!args), silent = TRUE)
  if (inherits(loo_test, "try-error")) {
    cat("Running `fn` on the LOO resamples produced an error:\n")
    print(loo_test)
    stop("`fn` failed.", call. = FALSE)
  }

  ## TODO use furrr
  if (is.vector(loo_test)) {
    if (length(loo_test) > 1)
      stop("The function should return a single value or a data frame/",
           "tibble.", call. = FALSE)
    leave_one_out_theta <- map_dbl(loo_rs$splits, rlang::exec, .fn = fn, !!!args)
  } else {
    if (!is.data.frame(loo_test))
      stop("The function should return a single value or a data frame/",
           "tibble.", call. = FALSE)
    leave_one_out_theta <- map_dfr(loo_rs$splits, rlang::exec, .fn = fn, !!!args) %>%
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
    estimate = mean(stats, na.rm = TRUE),
    upper = max(ci_bca),
    alpha = alpha,
    .method = "BCa"
  )
}

#' @importFrom dplyr filter pull
#' @importFrom purrr pluck
bca_single_wrapper <- function(stat_name, fn, args, dat, alpha){

  theta_hat <- dat %>% dplyr::filter(id == "Apparent") %>% pull(stat_name)

  stats <- dat %>% dplyr::filter(id != "Apparent") %>% pull(stat_name)

  orig_data <- dat %>% dplyr::filter(id == "Apparent") %>% pluck("splits", 1, "data")

  bca_single(stats, stat_name, theta_hat, orig_data, fn, args = args, alpha = alpha)

}


#' @rdname int_pctl
#' @inheritParams int_pctl
#' @param fn A function to calculate statistic of interest. The
#'  function should take an `rsplit` object as the first argument.
#' @param args A named list of arguments to pass to `fn`.
#' @importFrom dplyr select_vars
#' @importFrom purrr map_dfr
#' @export
int_bca <- function(object, ..., fn, args = list(), alpha = 0.05){

  check_rset(object)

  if (nrow(object) < 1000)
    warning("Recommend at least 1000 bootstrap resamples.",
            call. = FALSE)

  column_stats <- select_vars(names(object), !!!quos(...))

  res <-
    purrr::map_dfr(
      column_stats,
      bca_single_wrapper,
      fn,
      args = args,
      dat = object,
      alpha = alpha
    )

  res %>% mutate(.method = "BCa", statistic = column_stats)
}

# ----------------------------------------------------------------

check_rset <- function(x, app = TRUE) {
  if (!inherits(x, "bootstraps"))
    stop("`object` should be an `rset` object generated from `bootstraps()`",
         call. = FALSE)

  if(app) {
    if(x %>% dplyr::filter(id == "Apparent") %>% nrow() != 1)
      stop("Please set `apparent = TRUE` in `bootstraps()` function",
           call. = FALSE)
  }
  invisible(NULL)
}

# ----------------------------------------------------------------

#' @importFrom utils globalVariables
utils::globalVariables(c("id"))


