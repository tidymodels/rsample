#' V-Fold Cross-Validation
#'
#' V-fold cross-validation randomly splits the data into V groups of roughly equal size (called "folds"). A resample of the analysis data consisted of V-1 of the folds while the assessment set contains the final fold. In basic V-fold cross-validation (i.e. no repeats), the number of resamples is equal to V. 

#' @details 
#' The \code{strata} argument causes the random sampling to be conducted \emph{within the stratification variable}. The can help ensure that the number of data points in the analysis data is equivalent to the proportions in the original data set.  
#'
#' When more than one repeat is requested, the basic V-fold cross-validation is conducted each time. For example, if three repeats are used with \code{v = 3}, there are a total of 30 splits which as three groups of 10 that are generated separately. 
#'
#' @param data A data frame.
#' @param v The number of partitions of the data set. 
#' @param repeats The number of times to repeat the V-fold partitioning. 
#' @param strata A variable that is used to conduct stratified sampling to create the folds. (not working yet)
#' @param ... Not currently used. 
#' @export
#' @return  An tibble with classes \code{vfold_cv}, \code{rset}, \code{tbl_df}, \code{tbl}, and \code{data.frame}. The results include a column for the data split objects and one or more identification variables. For a single repeats, there will be one column called \code{id} that has a character string with the fold identifier. For repeats, \code{id} is the repeat number and an additional column called \code{id2} that contains the fold information (within repeat). 
#' @examples
#' vfold_cv(mtcars, V = 10)
#' vfold_cv(mtcars, V = 10, repeats = 2)
#' @export
vfold_cv <- function(data, v = 10, repeats = 1, strata = NULL, ...) {
  if(repeats == 1) {
    split_objs <- vfold_splits(data = data, v = v)
  } else {
    for (i in 1:repeats) {
      tmp <- vfold_splits(data = data, v = v)
      tmp$id2 <- tmp$id
      tmp$id <- names0(repeats, "Repeat")[i]
      split_objs <- if (i == 1)
        tmp
      else
        rbind(split_objs, tmp)
    }
  }
  attr(split_objs, "v") <- v
  attr(split_objs, "repeats") <- repeats
  attr(split_objs, "strata") <- !is.null(strata)
  
  class(split_objs) <- c("vfold_cv", "rset", class(split_objs))
  split_objs
}

# Get the indices of the analysis set from the assessment set
vfold_complement <- function(ind, n) {
  list(analysis = setdiff(1:n, ind),
       assessment = ind)
}

vfold_splits <- function(data, v = 10) {
  if (!is.numeric(v) || length(v) != 1) 
    stop("`v` must be a single integer.", call. = FALSE)
  
  n <- nrow(data)
  folds <- sample(rep(1:v, length.out = n))
  
  idx <- seq_len(n)
  indices <- split(idx, folds)
  indices <- lapply(indices, vfold_complement, n = n)
  
  split_objs <- purrr::map(indices, make_splits, data = data, class = "vfold_split")
  tibble::tibble(splits = split_objs, 
                 id = names0(length(split_objs), "Fold"))
}

#' @export
print.vfold_cv <- function(x, ...) {
  details <- attributes(x)
  cat("# ", details$v, "-fold cross-validation ", sep = "")
  if (details$repeats > 1)
    cat("repeated", details$repeats, "times ")
  if (details$strata)
    cat("using stratification")
  cat("\n")
  class(x) <- class(x)[!(class(x) %in% c("vfold_cv", "rset"))]
  print(x)
}