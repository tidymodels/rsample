#' Create or Modify Stratification Variables
#'
#' This function can create strata from numeric data and make non-numeric data
#'  more conducive for stratification.
#'
#' @details
#' For numeric data, if the number of unique levels is less than
#' `nunique`, the data are treated as categorical data.
#'
#' For categorical inputs, the function will find levels of `x` than
#'   occur in the data with percentage less than `pool`. The values from
#'   these groups will be randomly assigned to the remaining strata (as will
#'   data points that have missing values in `x`).
#'
#' For numeric data with more unique values than `nunique`, the data
#'   will be converted to being categorical based on percentiles of the data.
#'   The percentile groups will have no more than 20 percent of the data in
#'   each group. Again, missing values in `x` are randomly assigned
#'   to groups.
#'
#' @param x An input vector.
#' @param breaks A single number giving the number of bins desired to stratify a
#'   numeric stratification variable.
#' @param nunique An integer for the number of unique value threshold in the
#'   algorithm.
#' @param pool A proportion of data used to determine if a particular group is
#'   too small and should be pooled into another group. We do not recommend
#'   decreasing this argument below its default of 0.1 because of the dangers
#'   of stratifying groups that are too small.
#' @param depth An integer that is used to determine the best number of
#'   percentiles that should be used. The number of bins are based on
#'   `min(5, floor(n / depth))` where `n = length(x)`.
#'   If `x` is numeric, there must be at least 40 rows in the data set
#'   (when `depth = 20`) to conduct stratified sampling.
#'
#' @export
#' @return  A factor vector.
#' @examples
#' set.seed(61)
#' x1 <- rpois(100, lambda = 5)
#' table(x1)
#' table(make_strata(x1))
#'
#' set.seed(554)
#' x2 <- rpois(100, lambda = 1)
#' table(x2)
#' table(make_strata(x2))
#'
#' # small groups are randomly assigned
#' x3 <- factor(x2)
#' table(x3)
#' table(make_strata(x3))
#'
#' x4 <- rep(LETTERS[1:7], c(37, 26, 3, 7, 11, 10, 2))
#' table(x4)
#' table(make_strata(x4))
#' table(make_strata(x4, pool = 0.1))
#' table(make_strata(x4, pool = 0.0))
#'
#' # not enough data to stratify
#' x5 <- rnorm(20)
#' table(make_strata(x5))
#'
#' set.seed(483)
#' x6 <- rnorm(200)
#' quantile(x6, probs = (0:10) / 10)
#' table(make_strata(x6, breaks = 10))
#' @export
make_strata <- function(x, breaks = 4, nunique = 5, pool = .1, depth = 20) {
  default_pool <- 0.1
  num_vals <- unique(stats::na.omit(x))
  n <- length(x)
  if (length(num_vals) <= nunique | is.character(x) | is.factor(x)) {
    x <- factor(x)
    xtab <- sort(table(x))
    pcts <- xtab / n

    ## This should really be based on some combo of rate and number.
    if (all(pcts < pool)) {
      cli_warn(c(
        "Too little data to stratify.",
        "*" = "Resampling will be unstratified."
      ))
      return(factor(rep("strata1", n)))
    }

    if (pool < default_pool & any(pcts < default_pool)) {
      cli_warn(c(
        "Stratifying groups that make up {round(100 * pool)}% of the data may 
        be statistically risky.",
        i = "Consider increasing {.arg pool} to at least 0.1."
      ))
    }

    ## Small groups will be randomly allocated to stratas at end
    ## These should probably go into adjacent groups but this works for now
    if (any(pcts < pool)) {
      x[x %in% names(pcts)[pcts < pool]] <- NA
    }
    ## The next line will also relevel the data if `x` was a factor
    out <- factor(as.character(x))
  } else {
    if (breaks < 2) {
      cli_warn(c(
        "The bins specified by {.arg breaks} must be >=2.",
        "*" = "Resampling will be unstratified."
      ))
      return(factor(rep("strata1", n)))
    } else if (floor(n / breaks) < depth) {
      cli_warn(c(
        "The number of observations in each quantile is below the recommended 
        threshold of {depth}.",
        "*" = "Stratification will use {floor(n / depth)} breaks instead."
      ))
    }
    breaks <- min(breaks, floor(n / depth))
    if (breaks < 2) {
      cli_warn(c(
        "Too little data to stratify.",
        "*" = "Resampling will be unstratified."
      ))
      return(factor(rep("strata1", n)))
    }
    pctls <- quantile(x, probs = (0:breaks) / breaks, na.rm = TRUE)
    pctls <- unique(pctls)
    out <- cut(x, breaks = pctls, include.lowest = TRUE)
  }

  num_miss <- sum(is.na(x))
  if (num_miss > 0) {
    out[is.na(x)] <- sample(levels(out), size = num_miss, replace = TRUE)
  }

  out
}
