#' Monte Carlo Cross-Validation
#'
#' One resample of Monte Carlo cross-validation takes a random sample (without replacement) of the original data set to be used for analysis. All other data points are added to the assessment set. 
#'
#' @details 
#' The \code{strata} argument causes the random sampling to be conducted \emph{within the stratification variable}. The can help ensure that the number of data points in the analysis data is equivalent to the proportions in the original data set.  
#'
#' @inheritParams vfold_cv
#' @param prop The proportion of data to be retained for modeling/analysis. 
#' @param times The number of times to repeat the sampling.. 
#' @param strata A variable that is used to conduct stratified sampling to create the resamples. 
#' @export
#' @return  An tibble with classes \code{mc_cv}, \code{rset}, \code{tbl_df}, \code{tbl}, and \code{data.frame}. The results include a column for the data split objects and a column called \code{id} that has a character string with the resample identifier.
#' @examples
#' mc_cv(mtcars, times = 2)
#' mc_cv(mtcars, prop = .5, times = 2)
#' 
#' library(purrr)
#' iris2 <- iris[1:130, ]
#' 
#' set.seed(13)
#' resample1 <- mc_cv(iris2, times = 3, prop = .5)
#' map_dbl(resample1$splits,
#'         function(x) {
#'           dat <- as.data.frame(x)$Species
#'           mean(dat == "virginica")
#'         })
#' 
#' set.seed(13)
#' resample2 <- mc_cv(iris2, strata = "Species", times = 3, prop = .5)
#' map_dbl(resample2$splits,
#'         function(x) {
#'           dat <- as.data.frame(x)$Species
#'           mean(dat == "virginica")
#'         })
#' @export
mc_cv <- function(data, prop = 3/4, times = 25, strata = NULL, ...) {
  
  if (!is.null(strata)) {
    if(!is.character(strata) | length(strata) != 1)
      stop("`strata` should be a single character value", call. = FALSE)
    if(!(strata %in% names(data)))
      stop(strata, " is not in `data`")
  }
  
  split_objs <-
    mc_splits(data = data,
              prop = 1 - prop,
              times = times, 
              strata = strata)
  
  attr(split_objs, "prop") <- prop
  attr(split_objs, "times") <- times
  attr(split_objs, "strata") <- !is.null(strata)
  
  split_objs <-
    add_class(split_objs,
              cls = c("mc_cv", "rset"),
              at_end = FALSE)
  
  split_objs
}

# Get the indices of the analysis set from the assessment set
mc_complement <- function(ind, n) {
  list(analysis = setdiff(1:n, ind),
       assessment = ind)
}

#' @importFrom purrr map map_df
#' @importFrom tibble tibble
mc_splits <- function(data, prop = 3/4, times = 25, strata = NULL) {
  if (!is.numeric(prop) | prop >= 1 | prop <= 0)
    stop("`prop` must be a number on (0, 1).", call. = FALSE)
  
  n <- nrow(data)
  if (is.null(strata)) {
    indices <- purrr::map(rep(n, times), sample, size = floor(n * prop))
  } else {
    stratas <- tibble::tibble(idx = 1:n,
                              strata = make_strata(getElement(data, strata)))
    stratas <- split(stratas, stratas$strata)
    stratas <-
      purrr::map_df(stratas, strat_sample, prop = prop, times = times)
    indices <- split(stratas$idx, stratas$rs_id)
  }
  indices <- lapply(indices, mc_complement, n = n)
  split_objs <-
    purrr::map(indices, make_splits, data = data, class = "mc_split")
  tibble::tibble(splits = split_objs,
                 id = names0(length(split_objs), "Resample"))
}

#' @importFrom purrr map map_df
strat_sample <- function(x, prop, times, ...) {
  n <- nrow(x)
  idx <- purrr::map(rep(n, times), sample, size = floor(n*prop), ...)
  out <- purrr::map_df(idx, function(ind, x) x[sort(ind), "idx"], x = x)
  out$rs_id <- rep(1:times, each = floor(n*prop))
  out
}

#' @export
print.mc_cv <- function(x, ...) {
  details <- attributes(x)
  cat(
    "# Monte Carlo cross-validation (",
    signif(details$prop, 2),
    "/",
    signif(1 - details$prop, 2),
    ") with ",
    details$times,
    " resamples ",
    sep = ""
  )
  if (details$strata) 
    cat("using stratification")
  cat("\n")
  class(x) <- class(x)[!(class(x) %in% c("mc_cv", "rset"))]
  print(x)
}