#' Monte Carlo Cross-Validation
#'
#' One resample of Monte Carlo cross-validation takes a random sample (without
#'  replacement) of the original data set to be used for analysis. All other
#'  data points are added to the assessment set.
#' @details The `strata` argument causes the random sampling to be conducted
#'  *within the stratification variable*. This can help ensure that the number of
#'  data points in the analysis data is equivalent to the proportions in the
#'  original data set. (Strata below 10% of the total are pooled together.)
#' @inheritParams vfold_cv
#' @param prop The proportion of data to be retained for modeling/analysis.
#' @param times The number of times to repeat the sampling.
#' @param strata A variable that is used to conduct stratified sampling to
#'  create the resamples. This could be a single character value or a variable
#'  name that corresponds to a variable that exists in the data frame.
#' @param breaks A single number giving the number of bins desired to stratify
#'  a numeric stratification variable.
#' @export
#' @return An tibble with classes `mc_cv`, `rset`, `tbl_df`, `tbl`, and
#'  `data.frame`. The results include a column for the data split objects and a
#'  column called `id` that has a character string with the resample identifier.
#' @examples
#' mc_cv(mtcars, times = 2)
#' mc_cv(mtcars, prop = .5, times = 2)
#'
#' library(purrr)
#' data(wa_churn, package = "modeldata")
#'
#' set.seed(13)
#' resample1 <- mc_cv(wa_churn, times = 3, prop = .5)
#' map_dbl(resample1$splits,
#'         function(x) {
#'           dat <- as.data.frame(x)$churn
#'           mean(dat == "Yes")
#'         })
#'
#' set.seed(13)
#' resample2 <- mc_cv(wa_churn, strata = "churn", times = 3, prop = .5)
#' map_dbl(resample2$splits,
#'         function(x) {
#'           dat <- as.data.frame(x)$churn
#'           mean(dat == "Yes")
#'         })
#'
#' set.seed(13)
#' resample3 <- mc_cv(wa_churn, strata = "tenure", breaks = 6, times = 3, prop = .5)
#' map_dbl(resample3$splits,
#'         function(x) {
#'           dat <- as.data.frame(x)$churn
#'           mean(dat == "Yes")
#'         })
#' @export
mc_cv <- function(data, prop = 3/4, times = 25, strata = NULL, breaks = 4, ...) {

  if(!missing(strata)) {
    strata <- tidyselect::vars_select(names(data), !!enquo(strata))
    if(length(strata) == 0) strata <- NULL
  }

  strata_check(strata, names(data))

  split_objs <-
    mc_splits(data = data,
              prop = prop, # prop for train set
              times = times,
              strata = strata,
              breaks = breaks)

  ## We remove the holdout indices since it will save space and we can
  ## derive them later when they are needed.

  split_objs$splits <- map(split_objs$splits, rm_out)

  mc_att <- list(prop = prop,
                 times = times,
                 strata = !is.null(strata))

  new_rset(splits = split_objs$splits,
           ids = split_objs$id,
           attrib = mc_att,
           subclass = c("mc_cv", "rset"))
}

# Get the indices of the analysis set from the assessment set
mc_complement <- function(ind, n) {
  list(analysis = ind, # ind for analysis
       assessment = setdiff(1:n, ind))
}


mc_splits <- function(data, prop = 3/4, times = 25, strata = NULL, breaks = 4) {
  if (!is.numeric(prop) | prop >= 1 | prop <= 0)
    stop("`prop` must be a number on (0, 1).", call. = FALSE)

  n <- nrow(data)
  if (is.null(strata)) {
    indices <- purrr::map(rep(n, times), sample, size = floor(n * prop))
  } else {
    stratas <- tibble::tibble(idx = 1:n,
                              strata = make_strata(getElement(data, strata),
                                                   breaks = breaks))
    stratas <- split_unnamed(stratas, stratas$strata)
    stratas <-
      purrr::map_df(stratas, strat_sample, prop = prop, times = times)
    indices <- split_unnamed(stratas$idx, stratas$rs_id)
  }
  indices <- lapply(indices, mc_complement, n = n)
  split_objs <-
    purrr::map(indices, make_splits, data = data, class = "mc_split")
  list(splits = split_objs,
       id = names0(length(split_objs), "Resample"))
}

strat_sample <- function(x, prop, times, ...) {
  n <- nrow(x)
  idx <- purrr::map(rep(n, times), sample, size = floor(n*prop), ...)
  out <- purrr::map_df(idx, function(ind, x) x[sort(ind), "idx"], x = x)
  out$rs_id <- rep(1:times, each = floor(n*prop))
  out
}

#' @export
print.mc_cv <- function(x, ...) {
  cat("#", pretty(x), "\n")
  class(x) <- class(x)[!(class(x) %in% c("mc_cv", "rset"))]
  print(x, ...)
}
