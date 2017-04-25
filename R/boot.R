#' Bootstrap Sampling
#'
#' @inheritParams vfold_cv
#' @param times The number of bootstrap samples. 
#' @param strata A variable that is used to conduct stratified sampling. When not \code{NULL}, each bootstrap sample is created within the stratification variable.
#' @param apparent A logical. Should an extra resample be added where the analysis and holdout subset are the entire data set. This is required for some estimators used by the \code{summary} function that require the apparent error rate.   
#' @param oob A logical. Should the out-of-bootstrap samples (aka "out-of-bag" aka "OOB") be retained? For traditional bootstrapping, these samples are not generally used.  
#' @export
#' @return  An object with classes \code{"boot"} and \code{"rset"}. The elements of the object include a tibble called \code{splits} that contains a column for the data split objects and a column called \code{id} that has a character string with the resample identifier.
#' @examples
#' boot(mtcars, times = 2)
#' boot(mtcars, times = 2, apparent = TRUE)
#' boot(mtcars, times = 2, oob = FALSE)
#' @export
boot <- function(data, times = 25, strata = NULL, apparent = FALSE, oob = TRUE, ...) {
  if(apparent & !oob)
    stop("The apparent error rate calculation requires the out-of-bag sample", call. = FALSE)
  split_objs <- boot_splits(data = data, times = times, apparent = apparent,
                            oob = oob)
  
  structure(list(splits = split_objs, 
                 strata = strata, 
                 times = times), 
            class = c("boot", "rset"))
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
  
  split_objs <- purrr::map(indices, make_splits, data = data)
  out <- tibble::tibble(splits = split_objs, 
                        id = paste0("Bootstrap", seq_along(split_objs)))
  if(apparent) {
    app <- tibble::tibble(splits = list(rsplit(data, 1:n, 1:n)), 
                          id = "Apparent")
    out <- rbind(out, app)
  }
  out
  
}

#' @export
print.boot<- function(x, ...) {
  cat("Bootstrap sampling with ", x$times, " resamples ", sep = "")
  if(!is.null(x$strata)) cat("using stratification")
  cat("\n")
  if(any(x$splits$id == "Apparent"))
    cat("(includes apparent error rate sample)\n")
}
