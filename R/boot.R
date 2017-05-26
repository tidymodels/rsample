#' Bootstrap Sampling
#'
#' A bootstrap sample is a sample that is the same size as the original data set that is made using replacement.  This results in analysis samples that have multiple replicates of some of the original rows of the data. The assessment set is defined as the rows of the original data that were not included in the bootstrap sample. This is often referred to as the "out-of-bag" (OOB) sample. 

#' @details
#' The argument \code{apparent} enables the option of an additional "resample" where the analysis and assessment data sets are the same as the original data set. This can be required for some types of analysis of the bootstrap results. 
#' 
#' The \code{strata} argument is based on a similar argument in the random forest package were the bootstrap samples are conducted \emph{within the stratification variable}. The can help ensure that the number of data points in the bootstrap sample is equivalent to the proportions in the original data set.
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
#' 
#' library(purrr)
#' iris2 <- iris[1:130, ]
#' 
#' set.seed(13)
#' resample1 <- bootstraps(iris2, times = 3)
#' map_dbl(resample1$splits,
#'         function(x) {
#'           dat <- as.data.frame(x)$Species
#'           mean(dat == "virginica")
#'         })
#' 
#' set.seed(13)
#' resample2 <- bootstraps(iris2, strata = "Species", times = 3)
#' map_dbl(resample2$splits,
#'         function(x) {
#'           dat <- as.data.frame(x)$Species
#'           mean(dat == "virginica")
#'         })
#' @export
bootstraps <-
  function(data,
           times = 25,
           strata = NULL,
           apparent = FALSE,
           oob = TRUE,
           ...) {
    
  if (apparent & !oob)
    stop("The apparent error rate calculation requires the out-of-bag sample",
         call. = FALSE)
    
  if (!is.null(strata)) {
    if (!is.character(strata) | length(strata) != 1)
      stop("`strata` should be a single character value", call. = FALSE)
    if (!(strata %in% names(data)))
      stop(strata, " is not in `data`")
  }    

  split_objs <-
    boot_splits(
      data = data,
      times = times,
      apparent = apparent,
      oob = oob,
      strata = strata
    )
  attr(split_objs, "times") <- times
  attr(split_objs, "strata") <- !is.null(strata)
  attr(split_objs, "apparent") <- apparent
  attr(split_objs, "oob") <- oob
  
  split_objs <-
    add_class(split_objs,
              cls = c("bootstraps", "rset"),
              at_end = FALSE)
  
  split_objs
}

# Get the indices of the analysis set from the analysis set (= bootstrap sample)
boot_complement <- function(ind, n, assess) {
  if (assess)
    list(analysis = ind, assessment = (1:n)[-unique(ind)])
  else
    list(analysis = ind, assessment = integer())
}

#' @importFrom purrr map map_df
#' @importFrom tibble tibble
boot_splits <-
  function(data,
           times = 25,
           apparent = FALSE,
           oob = TRUE,
           strata = NULL) {
    
  n <- nrow(data)

  if (is.null(strata)) {
    indices <- purrr::map(rep(n, times), sample, replace = TRUE)
  } else {
    stratas <- tibble::tibble(idx = 1:n,
                              strata = getElement(data, strata))
    stratas <- split(stratas, stratas$strata)
    stratas <-
      purrr::map_df(
        stratas,
        strat_sample,
        prop = 1,
        times = times,
        replace = TRUE
      )
    indices <- split(stratas$idx, stratas$rs_id)
  }  

  indices <- lapply(indices, boot_complement, n = n, assess = oob)
  
  split_objs <-
    purrr::map(indices, make_splits, data = data, class = "boot_split")
  out <- tibble::tibble(splits = split_objs,
                        id = names0(length(split_objs), "Bootstrap"))
  if (apparent) {
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
