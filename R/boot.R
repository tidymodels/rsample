#' Bootstrap Sampling
#'
#' A bootstrap sample is a sample that is the same size as the original data set that is made using replacement.  This results in analysis samples that have multiple replicates of some of the original rows of the data. The assessment set is defined as the rows of the original data that were not included in the bootstrap sample. This is often referred to as the "out-of-bag" (OOB) sample. 

#' @details
#' The argument \code{apparent} enables the option of an additional "resample" where the analysis and assessment data sets are the same as the original data set. This can be required for some types of analysis of the bootstrap results. 
#' 
#' The \code{strata} argument is based on a similar argument in the random forest package were the bootstrap samples are conducted \emph{within the stratification variable}. The can help ensure that the number of data points in the bootstrap sample is equivalent to the proportions in the original data set. (not working yet)
#'
#' @inheritParams vfold_cv
#' @param times The number of bootstrap samples. 
#' @param strata A variable that is used to conduct stratified sampling. When not \code{NULL}, each bootstrap sample is created within the stratification variable.
#' @param apparent A logical. Should an extra resample be added where the analysis and holdout subset are the entire data set. This is required for some estimators used by the \code{summary} function that require the apparent error rate.   
#' @param oob A logical. Should the out-of-bootstrap samples (aka "out-of-bag" aka "OOB") be retained? For traditional bootstrapping, these samples are not generally used.  
#' @export
#' @return  An tibble with classes \code{bootstraps}, \code{rset}, \code{tbl_df}, \code{tbl}, and \code{data.frame}. The results include a column for the data split objects and a column called \code{id} that has a character string with the resample identifier.
#' @examples
#' bootstraps(mtcars, times = 2)
#' bootstraps(mtcars, times = 2, apparent = TRUE)
#' bootstraps(mtcars, times = 2, oob = FALSE)
#' @export
bootstraps <- 
  function(data, times = 25, strata = NULL, apparent = FALSE, oob = TRUE, ...) {
  if (apparent & !oob)
    stop("The apparent error rate calculation requires the out-of-bag sample",
         call. = FALSE)
  split_objs <-
    boot_splits(
      data = data,
      times = times,
      apparent = apparent,
      oob = oob
    )
  attr(split_objs, "times") <- times
  attr(split_objs, "strata") <- !is.null(strata)
  attr(split_objs, "apparent") <- apparent
  attr(split_objs, "oob") <- oob
  class(split_objs) <- c("bootstraps", "rset", class(split_objs))
  split_objs
}

# Get the indices of the analysis set from the analysis set (= bootstrap sample)
boot_complement <- function(ind, n, assess) {
  if(assess)
    list(analysis = ind, assessment = (1:n)[-unique(ind)]) else
      list(analysis = ind, assessment = integer())
}

boot_splits <- function(data, times = 25, apparent = FALSE, oob = TRUE) {
  n <- nrow(data)
  indices <- purrr::map(rep(n, times), sample, replace = TRUE)
  indices <- lapply(indices, boot_complement, n = n, assess = oob)
  
  split_objs <- purrr::map(indices, make_splits, data = data, class = "boot_split")
  out <- tibble::tibble(splits = split_objs, 
                        id = names0(length(split_objs), "Bootstrap"))
  if(apparent) {
    app <- tibble::tibble(splits = list(rsplit(data, 1:n, 1:n)), 
                          id = "Apparent")
    out <- rbind(out, app)
  }
  out
  
}

#' @export
print.bootstraps<- function(x, ...) {
  details <- attributes(x)
  cat("# Bootstrap sampling with ", details$times, " resamples ", 
      sep = "")
  if (details$strata) 
    cat("+ strata")
  if (any(details$splits$id == "Apparent")) 
    cat(" (includes apparent error rate sample)")
  cat("\n")
  class(x) <- class(x)[!(class(x) %in% c("bootstraps", "rset"))]
  print(x)
}
