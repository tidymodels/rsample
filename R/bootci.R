# Bootstrap confidence interval code

# ------------------------------------------------------------------------------
# helpers


check_rset <- function(x, app = TRUE) {
  if (!inherits(x, "bootstraps"))
    stop("`.data` should be an `rset` object generated from `bootstraps()`",
         call. = FALSE)

  if (app) {
    if(x %>% dplyr::filter(id == "Apparent") %>% nrow() != 1)
      stop("Please set `apparent = TRUE` in `bootstraps()` function",
           call. = FALSE)
  }
  invisible(NULL)
}


stat_fmt_err <- paste("`statistics` should select a list column of tidy results.")
stat_nm_err <- paste("The tibble in `statistics` should have columns for",
                     "'estimate' and 'term`")
std_exp <- c("std.error", "robust.se")

check_tidy_names <- function(x, std_col) {
  # check for proper columns
  if (sum(colnames(x) == "estimate") != 1) {
    stop(stat_nm_err, call. = FALSE)
  }
  if (sum(colnames(x) == "term") != 1) {
    stop(stat_nm_err, call. = FALSE)
  }
  if (std_col) {
    std_candidates <- colnames(x) %in% std_exp
    if (sum(std_candidates) != 1) {
      stop("`statistics` should select a single column for the standard ",
           "error.", call. = FALSE)
    }
  }
  invisible(TRUE)
}

check_tidy <- function(x, std_col = FALSE) {
  if (!is.list(x)) {
    stop(stat_fmt_err, call. = FALSE)
  }

  # convert to data frame from list
  has_id <- any(names(x) == "id")
  if (has_id) {
    list_cols <- names(x)[map_lgl(x, is_list)]
    x <- try(tidyr::unnest(x, cols = list_cols), silent = TRUE)
  } else {
    x <- try(map_dfr(x, ~ .x), silent = TRUE)
  }

  if (inherits(x, "try-error")) {
    stop(stat_fmt_err, call. = FALSE)
  }

  check_tidy_names(x, std_col)

  if (std_col) {
    std_candidates <- colnames(x) %in% std_exp
    std_candidates <- colnames(x)[std_candidates]
    if (has_id) {
      x <-
        dplyr::select(x, term, estimate, id, tidyselect::one_of(std_candidates)) %>%
        mutate(id = (id == "Apparent")) %>%
        setNames(c("term", "estimate", "orig", "std_err"))
    } else {
      x <-
        dplyr::select(x, term, estimate, tidyselect::one_of(std_candidates)) %>%
        setNames(c("term", "estimate", "std_err"))
    }

  } else {
    if (has_id) {
      x <-
        dplyr::select(x, term, estimate, id) %>%
        mutate(orig = (id == "Apparent")) %>%
        dplyr::select(-id)
    } else {
      x <- dplyr::select(x, term, estimate)
    }
  }

  x
}


get_p0 <- function(x, alpha = 0.05) {
  orig <- x %>%
    group_by(term) %>%
    dplyr::filter(orig) %>%
    dplyr::select(term, theta_0 = estimate) %>%
    ungroup()
  x %>%
    dplyr::filter(!orig) %>%
    inner_join(orig, by = "term") %>%
    group_by(term) %>%
    summarize(p0 = mean(estimate <= theta_0, na.rm = TRUE)) %>%
    mutate(Z0 = stats::qnorm(p0),
           Za = stats::qnorm(1 - alpha / 2))
}

new_stats <- function(x, lo, hi) {
  res <- as.numeric(quantile(x, probs = c(lo, hi), na.rm = TRUE))
  tibble(.lower = min(res), .estimate = mean(x, na.rm = TRUE), .upper = max(res))
}

has_dots <- function(x) {
  nms <- names(formals(x))
  if (!any(nms == "...")) {
    stop("`.fn` must have an argument `...`.", call. = FALSE)
  }
  invisible(NULL)
}

check_num_resamples <- function(x, B = 1000) {
  x <-
    x %>%
    dplyr::group_by(term) %>%
    dplyr::summarize(n = sum(!is.na(estimate))) %>%
    dplyr::filter(n < B)

  if (nrow(x) > 0) {
    terms <- paste0("`", x$term, "`")
    msg <-
      paste0(
        "Recommend at least ", B, " non-missing bootstrap resamples for ",
        ifelse(length(terms) > 1, "terms: ", "term "),
        paste0(terms, collapse = ", "),
        "."
      )
    warning(msg, call. = FALSE)
  }
  invisible(NULL)
}

# ------------------------------------------------------------------------------
# percentile code


pctl_single <- function(stats, alpha = 0.05) {

  if (all(is.na(stats)))
    stop("All statistics have missing values..", call. = FALSE)

  if (!is.numeric(stats))
    stop("`stats` must be a numeric vector.", call. = FALSE)

  # stats is a numeric vector of values
  ci <- stats %>% quantile(probs = c(alpha / 2, 1 - alpha / 2), na.rm = TRUE)

  # return a tibble with .lower, .estimate, .upper
  res <- tibble(
    .lower = min(ci),
    .estimate = mean(stats, na.rm = TRUE),
    .upper = max(ci),
    .alpha = alpha,
    .method = "percentile"
  )
  res
}

#' Bootstrap confidence intervals
#' @description
#' Calculate bootstrap confidence intervals using various methods.
#' @param .data A data frame containing the bootstrap resamples created using
#'  `bootstraps()`. For t- and BCa-intervals, the `apparent` argument
#'  should be set to `TRUE`. Even if the `apparent` argument is set to
#'  `TRUE` for the percentile method, the apparent data is never used in calculating
#'  the percentile confidence interval.
#' @param statistics An unquoted column name or `dplyr` selector that identifies
#'  a single column in the data set that contains the individual bootstrap
#'  estimates. This can be a list column of tidy tibbles (that contains columns
#'  `term` and `estimate`) or a simple numeric column. For t-intervals, a
#'  standard tidy column (usually called `std.err`) is required.
#'  See the examples below.
#' @param alpha Level of significance
#' @return Each function returns a tibble with columns `.lower`,
#'  `.estimate`, `.upper`, `.alpha`, `.method`, and `term`.
#'  `.method` is the type of interval (eg. "percentile",
#'  "student-t", or "BCa"). `term` is the name of the estimate. Note
#'  the `.estimate` returned from `int_pctl()`
#'  is the mean of the estimates from the bootstrap resamples
#'  and not the estimate from the apparent model.
#' @details Percentile intervals are the standard method of
#'  obtaining confidence intervals but require thousands of
#'  resamples to be accurate. T-intervals may need fewer
#'  resamples but require a corresponding variance estimate.
#'  Bias-corrected and accelerated intervals require the original function
#'  that was used to create the statistics of interest and are
#'  computationally taxing.
#' @seealso [parametric_intervals()]
#' @references Davison, A., & Hinkley, D. (1997). _Bootstrap Methods and their
#'  Application_. Cambridge: Cambridge University Press.
#'  doi:10.1017/CBO9780511802843
#'
#' @examples
#' \donttest{
#' library(broom)
#' library(dplyr)
#' library(purrr)
#' library(tibble)
#'
#' lm_est <- function(split, ...) {
#'   lm(mpg ~ disp + hp, data = analysis(split)) %>%
#'     tidy()
#' }
#'
#' set.seed(52156)
#' car_rs <-
#'   bootstraps(mtcars, 500, apparent = TRUE) %>%
#'   mutate(results = map(splits, lm_est))
#'
#' int_pctl(car_rs, results)
#' int_t(car_rs, results)
#' int_bca(car_rs, results, .fn = lm_est)
#'
#' # putting results into a tidy format
#' rank_corr <- function(split) {
#'   dat <- analysis(split)
#'   tibble(
#'     term = "corr",
#'     estimate = cor(dat$sqft, dat$price, method = "spearman"),
#'     # don't know the analytical std.err so no t-intervals
#'     std.err = NA_real_
#'   )
#' }
#'
#' set.seed(69325)
#' data(Sacramento, package = "modeldata")
#' bootstraps(Sacramento, 1000, apparent = TRUE) %>%
#'   mutate(correlations = map(splits, rank_corr)) %>%
#'   int_pctl(correlations)
#' }
#' @export
int_pctl <- function(.data, statistics, alpha = 0.05) {

  check_rset(.data, app = FALSE)
  if (length(alpha) != 1 || !is.numeric(alpha)) {
    abort("`alpha` must be a single numeric value.")
  }

  .data <- .data %>% dplyr::filter(id != "Apparent")

  column_name <- tidyselect::vars_select(names(.data), !!rlang::enquo(statistics))
  if (length(column_name) != 1) {
    stop(stat_fmt_err, call. = FALSE)
  }
  stats <- .data[[column_name]]
  stats <- check_tidy(stats, std_col = FALSE)

  check_num_resamples(stats, B = 1000)

  vals <- stats %>%
    dplyr::group_by(term) %>%
    dplyr::do(pctl_single(.$estimate, alpha = alpha)) %>%
    dplyr::ungroup()
  vals

}

# ------------------------------------------------------------------------------
# t interval code

t_single <- function(stats, std_err, is_orig, alpha = 0.05) {
  # stats is a numeric vector of values
  # vars is a numeric vector of variances
  # return a tibble with .lower, .estimate, .upper
  # which_orig is the index of stats and std_err that has the original result

  if (all(is.na(stats)))
    stop("All statistics have missing values.", call. = FALSE)

  if (!is.logical(is_orig) || any(is.na(is_orig))) {
    stop("`is_orig` should be a logical column the same length as `stats` ",
         "with no missing values.", call. = FALSE)
  }
  if (length(stats) != length(std_err) && length(stats) != length(is_orig)) {
    stop("`stats`, `std_err`, and `is_orig` should have the same length.",
         call. = FALSE)
  }
  if (sum(is_orig) != 1) {
    stop("The original statistic must be in a single row.", call. = FALSE)
  }

  theta_obs   <- stats[is_orig]
  std_err_obs <- std_err[is_orig]

  stats   <- stats[!is_orig]
  std_err <- std_err[!is_orig]

  z_dist <-
    (stats - theta_obs) / std_err

  z_pntl <-
    quantile(z_dist, probs = c(alpha / 2, 1 - (alpha) / 2), na.rm = TRUE)

  ci <- theta_obs - z_pntl * std_err_obs

  tibble(
    .lower = min(ci),
    .estimate = mean(stats, na.rm = TRUE),
    .upper = max(ci),
    .alpha = alpha,
    .method = "student-t"
  )
}


#' @rdname int_pctl
#' @export
int_t <- function(.data, statistics, alpha = 0.05) {

  check_rset(.data)
  if (length(alpha) != 1 || !is.numeric(alpha)) {
    abort("`alpha` must be a single numeric value.")
  }

  column_name <- tidyselect::vars_select(names(.data), !!enquo(statistics))
  if (length(column_name) != 1) {
    stop(stat_fmt_err, call. = FALSE)
  }
  stats <- .data %>% dplyr::select(!!column_name, id)
  stats <- check_tidy(stats, std_col = TRUE)

  check_num_resamples(stats, B = 500)

  vals <-
    stats %>%
    dplyr::group_by(term) %>%
    dplyr::do(t_single(.$estimate, .$std_err, .$orig, alpha = alpha)) %>%
    dplyr::ungroup()
  vals
}


# ----------------------------------------------------------------

bca_calc <- function(stats, orig_data, alpha = 0.05, .fn, ...) {

  # TODO check per term
  if (all(is.na(stats$estimate))) {
    stop("All statistics have missing values.", call. = FALSE)
  }

  ### Estimating Z0 bias-correction
  bias_corr_stats <- get_p0(stats, alpha = alpha)

  # need the original data frame here
  loo_rs <- loo_cv(orig_data)

  # We can't be sure what we will get back from the analysis function.
  # To test, we run on the first LOO data set and see if it is a vector or df
  loo_test <- try(rlang::exec(.fn, loo_rs$splits[[1]], ...), silent = TRUE)
  if (inherits(loo_test, "try-error")) {
    cat("Running `.fn` on the LOO resamples produced an error:\n")
    print(loo_test)
    stop("`.fn` failed.", call. = FALSE)
  }

  loo_res <- furrr::future_map_dfr(loo_rs$splits, .fn, ...)

  loo_estimate <-
    loo_res %>%
    dplyr::group_by(term) %>%
    dplyr::summarize(loo = mean(estimate, na.rm = TRUE)) %>%
    dplyr::inner_join(loo_res, by = "term")  %>%
    dplyr::group_by(term) %>%
    dplyr::summarize(
      cubed = sum((loo - estimate)^3),
      squared = sum((loo - estimate)^2)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(bias_corr_stats, by = "term") %>%
    dplyr::mutate(
      a = cubed/(6 * (squared^(3 / 2))),
      Zu = (Z0 + Za) / ( 1 - a * (Z0 + Za)) + Z0,
      Zl = (Z0 - Za) / (1 - a * (Z0 - Za)) + Z0,
      lo = stats::pnorm(Zl, lower.tail = TRUE),
      hi = stats::pnorm(Zu, lower.tail = TRUE)
    )

  terms <- loo_estimate$term
  stats <- stats %>% dplyr::filter(!orig)
  for (i in seq_along(terms)) {
    tmp <- new_stats(stats$estimate[ stats$term == terms[i] ],
                     lo = loo_estimate$lo[i],
                     hi = loo_estimate$hi[i])
    tmp$term <- terms[i]
    if (i == 1) {
      ci_bca <- tmp
    } else {
      ci_bca <- bind_rows(ci_bca, tmp)
    }
  }
  ci_bca <-
    ci_bca %>%
    dplyr::select(term, .lower, .estimate, .upper) %>%
    dplyr::mutate(
      .alpha = alpha,
      .method = "BCa"
    )
}


#' @rdname int_pctl
#' @param .fn A function to calculate statistic of interest. The
#'  function should take an `rsplit` as the first argument and the `...` are
#'  required.
#' @param ... Arguments to pass to `.fn`.
#' @references \url{https://rsample.tidymodels.org/articles/Applications/Intervals.html}
#' @export
int_bca <- function(.data, statistics, alpha = 0.05, .fn, ...) {

  check_rset(.data)
  if (length(alpha) != 1 || !is.numeric(alpha)) {
    abort("`alpha` must be a single numeric value.")
  }

  has_dots(.fn)

  column_name <- tidyselect::vars_select(names(.data), !!enquo(statistics))
  if (length(column_name) != 1) {
    stop(stat_fmt_err, call. = FALSE)
  }
  stats <- .data %>% dplyr::select(!!column_name, id)
  stats <- check_tidy(stats)

  check_num_resamples(stats, B = 1000)

  vals <- bca_calc(stats, .data$splits[[1]]$data, alpha = alpha, .fn = .fn, ...)
  vals
}
