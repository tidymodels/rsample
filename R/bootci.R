# Bootstrap confidence interval code

# ------------------------------------------------------------------------------
# helpers


check_includes_apparent <- function(x, call = caller_env()) {
  if (x %>% dplyr::filter(id == "Apparent") %>% nrow() != 1) {
    cli_abort(c(
      "The bootstrap resamples must include an apparent sample.",
      i =  "Please set {.code apparent = TRUE} in the {.fn bootstraps} function."
      ), 
      call = call
    )
  }
  invisible(NULL)
}


statistics_format_error <- cli::format_inline(
  "{.arg statistics} should select a list column of tidy results."
)
std_exp <- c("std.error", "robust.se")

check_statistics_names <- function(x, std_col, call = caller_env()) {
  # check for proper columns
  if (sum(colnames(x) == "estimate") != 1) {
    cli_abort(
      "The tibble in {.arg statistics} must have a column for 'estimate'.",
      call = call
    )
  }
  if (sum(colnames(x) == "term") != 1) {
    cli_abort(
       "The tibble in {.arg statistics} must have a column for 'term'.",
       call = call
    )
  }
  if (std_col) {
    std_candidates <- colnames(x) %in% std_exp
    if (sum(std_candidates) != 1) {
      cli_abort(
        "{.arg statistics} should select a single column for the standard error.",
        call = call
      )
    }
  }
  invisible(TRUE)
}

check_statistics <- function(x, std_col = FALSE, call = caller_env()) {
  if (!is.list(x)) {
    cli_abort(statistics_format_error, call = call)
  }

  # convert to data frame from list
  has_id <- any(names(x) == "id")
  if (has_id) {
    list_cols <- names(x)[map_lgl(x, is_list)]
    x <- try(tidyr::unnest(x, cols = all_of(list_cols)), silent = TRUE)
  } else {
    x <- try(map(x, ~.x) %>% list_rbind(), silent = TRUE)
  }

  if (inherits(x, "try-error")) {
    cli_abort(statistics_format_error, call = call)
  }

  check_statistics_names(x, std_col, call = call)

  if (std_col) {
    std_candidates <- colnames(x) %in% std_exp
    std_candidates <- colnames(x)[std_candidates]
    re_name <- list(std_err = std_candidates)
    if (has_id) {
      x <-
        dplyr::select(x, term, estimate, id, tidyselect::one_of(std_candidates),
                      dplyr::starts_with(".")) %>%
        mutate(orig = (id == "Apparent"))  %>%
        dplyr::rename(!!!re_name)
    } else {
      x <-
        dplyr::select(x, term, estimate, tidyselect::one_of(std_candidates),
                      dplyr::starts_with(".")) %>%
        dplyr::rename(!!!re_name)
    }
  } else {
    if (has_id) {
      x <-
        dplyr::select(x, term, estimate, id, dplyr::starts_with(".")) %>%
        mutate(orig = (id == "Apparent")) %>%
        dplyr::select(-id)
    } else {
      x <- dplyr::select(x, term, estimate, dplyr::starts_with("."))
    }
  }

  x
}


get_p0 <- function(x, alpha = 0.05, groups) {
  group_sym <- rlang::syms(groups)

  orig <- x %>%
    group_by(!!!group_sym) %>%
    dplyr::filter(orig) %>%
    dplyr::select(!!!group_sym, theta_0 = estimate) %>%
    ungroup()
  x %>%
    dplyr::filter(!orig) %>%
    inner_join(orig, by = groups) %>%
    group_by(!!!group_sym) %>%
    summarize(p0 = mean(estimate <= theta_0, na.rm = TRUE)) %>%
    mutate(
      Z0 = stats::qnorm(p0),
      Za = stats::qnorm(1 - alpha / 2)
    )
}

new_stats <- function(x, lo, hi) {
  res <- as.numeric(quantile(x, probs = c(lo, hi), na.rm = TRUE))
  tibble(.lower = min(res), .estimate = mean(x, na.rm = TRUE), .upper = max(res))
}

check_has_dots <- function(x, call = caller_env()) {
  nms <- names(formals(x))
  if (!any(nms == "...")) {
    cli_abort("{.arg .fn} must have an argument {.arg ...}.", call = call)
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
    terms <- x$term
    cli_warn("Recommend at least {B} non-missing bootstrap resamples for {cli::qty(terms)} term{?s} {.code {terms}}.")
  }
  invisible(NULL)
}

# ------------------------------------------------------------------------------
# percentile code


pctl_single <- function(stats, alpha = 0.05) {
  if (all(is.na(stats))) {
    cli_abort("All statistics have missing values.")
  }

  if (!is.numeric(stats)) {
    cli_abort("{.arg stats} must be a numeric vector.")
  }

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
#'  a single column in the data set containing the individual bootstrap
#'  estimates. This must be a list column of tidy tibbles (with columns
#'  `term` and `estimate`). Optionally, users can include columns whose names
#'  begin with a period and the intervals will be created for each combination
#'  of these variables and the `term` column. For t-intervals, a standard tidy
#'  column (usually called `std.error`) is required. See the examples below.
#' @param alpha Level of significance.
#' @param .fn A function to calculate statistic of interest. The
#'  function should take an `rsplit` as the first argument and the `...` are
#'  required.
#' @param ... Arguments to pass to `.fn` (`int_bca()` only).
#' @references \url{https://rsample.tidymodels.org/articles/Applications/Intervals.html}
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
#' @seealso [reg_intervals()]
#' @references Davison, A., & Hinkley, D. (1997). _Bootstrap Methods and their
#'  Application_. Cambridge: Cambridge University Press.
#'  doi:10.1017/CBO9780511802843
#'
#' @examplesIf rlang::is_installed("broom") & rlang::is_installed("modeldata")
#' \donttest{
#' library(broom)
#' library(dplyr)
#' library(purrr)
#' library(tibble)
#' library(tidyr)
#'
#' # ------------------------------------------------------------------------------
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
#' # ------------------------------------------------------------------------------
#'
#' # putting results into a tidy format
#' rank_corr <- function(split) {
#'   dat <- analysis(split)
#'   tibble(
#'     term = "corr",
#'     estimate = cor(dat$sqft, dat$price, method = "spearman"),
#'     # don't know the analytical std.error so no t-intervals
#'     std.error = NA_real_
#'   )
#' }
#'
#' set.seed(69325)
#' data(Sacramento, package = "modeldata")
#' bootstraps(Sacramento, 1000, apparent = TRUE) %>%
#'   mutate(correlations = map(splits, rank_corr)) %>%
#'   int_pctl(correlations)
#'
#' # ------------------------------------------------------------------------------
#' # An example of computing the interval for each value of a custom grouping
#' # factor (type of house in this example)
#'
#' # Get regression estimates for each house type
#' lm_est <- function(split, ...) {
#'   analysis(split) %>%
#'     tidyr::nest(.by = c(type)) %>%
#'     # Compute regression estimates for each house type
#'     mutate(
#'       betas = purrr::map(data, ~ lm(log10(price) ~ sqft, data = .x) %>% tidy())
#'     ) %>%
#'     # Convert the column name to begin with a period
#'     rename(.type = type) %>%
#'     select(.type, betas) %>%
#'     unnest(cols = betas)
#' }
#'
#' set.seed(52156)
#' house_rs <-
#'   bootstraps(Sacramento, 1000, apparent = TRUE) %>%
#'   mutate(results = map(splits, lm_est))
#'
#' int_pctl(house_rs, results)
#' }
#' @export
int_pctl <- function(.data, ...) {
  UseMethod("int_pctl")
}

#' @export
#' @rdname int_pctl
int_pctl.bootstraps <- function(.data, statistics, alpha = 0.05, ...) {
  check_dots_empty()
  if (length(alpha) != 1 || !is.numeric(alpha)) {
    cli_abort("{.arg alpha} must be a single numeric value.")
  }

  .data <- .data %>% dplyr::filter(id != "Apparent")

  column_name <- tidyselect::vars_select(names(.data), !!rlang::enquo(statistics))
  if (length(column_name) != 1) {
    rlang::abort(statistics_format_error)
  }
  stats <- .data[[column_name]]
  stats <- check_statistics(stats, std_col = FALSE)

  check_num_resamples(stats, B = 1000)

  stat_groups <- c("term", grep("^\\.", names(stats), value = TRUE))
  stat_groups <- rlang::syms(stat_groups)

  vals <- stats %>%
    dplyr::group_by(!!!stat_groups) %>%
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

  if (all(is.na(stats))) {
    cli_abort("All statistics have missing values.")
  }

  if (!is.logical(is_orig) || any(is.na(is_orig))) {
    cli_abort(
      "{.arg is_orig} should be a logical column the same length as {.arg stats} with no missing values."
    )
  }
  if (length(stats) != length(std_err) && length(stats) != length(is_orig)) {
    function_args <- c('stats', 'std_err', 'is_orig')
    cli_abort("{.arg {function_args}} should have the same length.")
  }
  if (sum(is_orig) != 1) {
    cli_abort("The original statistic must be in a single row.")
  }

  theta_obs <- stats[is_orig]
  std_err_obs <- std_err[is_orig]

  stats <- stats[!is_orig]
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
int_t <- function(.data, ...) {
  UseMethod("int_t")
}

#' @rdname int_pctl
#' @export
int_t.bootstraps <- function(.data, statistics, alpha = 0.05, ...) {
  check_dots_empty()
  check_includes_apparent(.data)
  if (length(alpha) != 1 || !is.numeric(alpha)) {
    cli_abort("{.arg alpha} must be a single numeric value.")
  }

  column_name <- tidyselect::vars_select(names(.data), !!enquo(statistics))
  if (length(column_name) != 1) {
    cli_abort(statistics_format_error)
  }
  stats <- .data %>% dplyr::select(!!column_name, id)
  stats <- check_statistics(stats, std_col = TRUE)

  check_num_resamples(stats, B = 500)

  stat_groups <- c("term", grep("^\\.", names(stats), value = TRUE))
  stat_groups <- rlang::syms(stat_groups)
  vals <- stats %>%
    dplyr::group_by(!!!stat_groups) %>%
    dplyr::do(t_single(.$estimate, .$std_err, .$orig, alpha = alpha)) %>%
    dplyr::ungroup()
  vals
}


# ----------------------------------------------------------------

bca_calc <- function(stats, orig_data, alpha = 0.05, .fn, ...) {

  # TODO check per term
  if (all(is.na(stats$estimate))) {
    cli_abort("All statistics have missing values.")
  }

  stat_groups_chr <- c("term", grep("^\\.", names(stats), value = TRUE))
  stat_groups_sym <- rlang::syms(stat_groups_chr)

  ### Estimating Z0 bias-correction
  bias_corr_stats <- get_p0(stats, alpha = alpha, groups = stat_groups_chr)

  # need the original data frame here
  loo_rs <- loo_cv(orig_data)

  # We can't be sure what we will get back from the analysis function.
  # To test, we run on the first LOO data set and see if it is a vector or df
  loo_test <- try(rlang::exec(.fn, loo_rs$splits[[1]], ...), silent = TRUE)
  if (inherits(loo_test, "try-error")) {
    cat("Running `.fn` on the LOO resamples produced an error:\n")
    print(loo_test)
    cli_abort("{.arg .fn} failed.")
  }

  loo_res <- furrr::future_map(loo_rs$splits, .fn, ...) %>% list_rbind()

  loo_estimate <-
    loo_res %>%
    dplyr::group_by(!!!stat_groups_sym) %>%
    dplyr::summarize(loo = mean(estimate, na.rm = TRUE)) %>%
    dplyr::inner_join(loo_res, by = stat_groups_chr, multiple = "all") %>%
    dplyr::group_by(!!!stat_groups_sym) %>%
    dplyr::summarize(
      cubed = sum((loo - estimate)^3),
      squared = sum((loo - estimate)^2)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::inner_join(bias_corr_stats, by = stat_groups_chr) %>%
    dplyr::mutate(
      a = cubed / (6 * (squared^(3 / 2))),
      Zu = (Z0 + Za) / (1 - a * (Z0 + Za)) + Z0,
      Zl = (Z0 - Za) / (1 - a * (Z0 - Za)) + Z0,
      lo = stats::pnorm(Zl, lower.tail = TRUE),
      hi = stats::pnorm(Zu, lower.tail = TRUE)
    )

  terms <- loo_estimate$term
  stats <- stats %>% dplyr::filter(!orig)

  keys <- stats %>% dplyr::distinct(!!!stat_groups_sym)
  for (i in seq_len(nrow(keys))) {
    tmp_stats <- dplyr::inner_join(stats, keys[i,], by = stat_groups_chr)
    tmp_loo <- dplyr::inner_join(loo_estimate, keys[i,], by = stat_groups_chr)

    tmp <- new_stats(tmp_stats$estimate,
                     lo = tmp_loo$lo,
                     hi = tmp_loo$hi)
    tmp <- dplyr::bind_cols(tmp, keys[i,])
    if (i == 1) {
      ci_bca <- tmp
    } else {
      ci_bca <- dplyr::bind_rows(ci_bca, tmp)
    }
  }
  ci_bca <-
    ci_bca %>%
    dplyr::select(!!!stat_groups_sym, .lower, .estimate, .upper) %>%
    dplyr::mutate(
      .alpha = alpha,
      .method = "BCa"
    )
}

#' @rdname int_pctl
#' @export
int_bca <- function(.data, ...) {
  UseMethod("int_bca")
}

#' @rdname int_pctl
#' @export
int_bca.bootstraps <- function(.data, statistics, alpha = 0.05, .fn, ...) {
  check_includes_apparent(.data)
  if (length(alpha) != 1 || !is.numeric(alpha)) {
    cli_abort("{.arg alpha} must be a single numeric value.")
  }

  check_has_dots(.fn)

  column_name <- tidyselect::vars_select(names(.data), !!enquo(statistics))
  if (length(column_name) != 1) {
    cli_abort(statistics_format_error)
  }
  stats <- .data %>% dplyr::select(!!column_name, id, dplyr::starts_with("."))
  stats <- check_statistics(stats)

  check_num_resamples(stats, B = 1000)

  vals <- bca_calc(stats, .data$splits[[1]]$data, alpha = alpha, .fn = .fn, ...)
  vals
}
