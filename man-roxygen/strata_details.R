#' @details
#'
#'  With a `strata` argument, the random sampling is conducted
#'  *within the stratification variable*. This can help ensure that the
#'  resamples have equivalent proportions as the original data set. For
#'  a categorical variable, sampling is conducted separately within each class.
#'  For a numeric stratification variable, `strata` is binned into quartiles,
#'  which are then used to stratify. Strata below 10% of the total are
#'  pooled together; see [make_strata()] for more details.
#'

