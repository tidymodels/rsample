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
#' @param strata A variable that is used to conduct stratified sampling to create the resamples.  (not working yet)
#' @export
#' @return  An tibble with classes \code{mc_cv}, \code{rset}, \code{tbl_df}, \code{tbl}, and \code{data.frame}. The results include a column for the data split objects and a column called \code{id} that has a character string with the resample identifier.
#' @examples
#' mc_cv(mtcars, times = 2)
#' mc_cv(mtcars, prop = .5, times = 2)
#' @export
mc_cv <- function(data, prop = 3/4, times = 25, strata = NULL, ...) {
  split_objs <- mc_splits(data = data, prop = 1 - prop, times = times)
  
  attr(split_objs, "prop") <- prop
  attr(split_objs, "times") <- times
  attr(split_objs, "strata") <- !is.null(strata)
  class(split_objs) <- c("mc_cv", "rset", class(split_objs))
  split_objs
}

# Get the indices of the analysis set from the assessment set
mc_complement <- function(ind, n) {
  list(analysis = setdiff(1:n, ind),
       assessment = ind)
}

mc_splits <- function(data, prop = 3/4, times = 25) {
  if (!is.numeric(prop) | prop >= 1 | prop <= 0) 
    stop("`prop` must be a number on (0, 1).", call. = FALSE)
  
  n <- nrow(data)
  indices <- purrr::map(rep(n, times), sample, size = floor(n*prop))
  indices <- lapply(indices, mc_complement, n = n)
  
  split_objs <- purrr::map(indices, make_splits, data = data, class = "mc_split")
  tibble::tibble(splits = split_objs, 
                 id = names0(length(split_objs), "Resample"))
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