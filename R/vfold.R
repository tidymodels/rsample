#' V-Fold Cross-Validation
#'
#'
#'
#' @param data A data frame.
#' @param v The number of partitions of the data set. 
#' @param repeats The number of times to repeat the V-fold partitioning. 
#' @param strata A variable that is used to conduct stratified sampling to create the folds. 
#' @param ... Not currently used. 
#' @export
#' @return  An object with classes \code{"vfold_cv"} and \code{"rset"}. The elements of the object include a tibble called \code{splits} that contains a column for the data split objects and one or more identification variables. For a single repeats, there will be one column called \code{id} that has a character string with the fold identifier. For repeats, \code{id} is the repeat number and an additional column called \code{id2} that contains the fold information (within repeat). 
#' @examples
#' vfold_cv(mtcars, V = 10)
#' vfold_cv(mtcars, V = 10, repeats = 2)
#' @export
vfold_cv <- function(data, v = 10, repeats = 1, strata = NULL, ...) {
  if(repeats == 1) {
    split_objs <- vfold_splits(data = data, v = v)
  } else {
    for(i in 1:repeats) {
      tmp <- vfold_splits(data = data, v = v)
      tmp$id2 <- tmp$id
      tmp$id <- paste0("Repeat", i)
      split_objs <- if(i == 1) tmp else rbind(split_objs, tmp)
    }
  }
  structure(list(splits = split_objs, 
                 v = v, strata = strata, 
                 repeats = repeats), 
            class = c("vfold_cv", "rset"))
}

# Get the indicies of the analysis set from the assessment set
vfold_complement <- function(ind, n) {
  list(analysis = setdiff(1:n, ind),
       assessment = ind)
}

make_splits <- function(ind, data) {
  rsplit(data, ind$analysis,  ind$assessment)
}

vfold_splits <- function(data, v = 10) {
  if (!is.numeric(v) || length(v) != 1) 
    stop("`v` must be a single integer.", call. = FALSE)
  
  n <- nrow(data)
  folds <- sample(rep(1:v, length.out = n))
  
  idx <- seq_len(n)
  indices <- split(idx, folds)
  indices <- lapply(indices, vfold_complement, n = n)
  
  split_objs <- purrr::map(indices, make_splits, data = data)
  tibble::tibble(splits = split_objs, 
                 id = paste0("Fold", seq_along(split_objs)))
}

#' @export
print.vfold_cv <- function(x, ...) {
  cat("  ", x$v, "-fold cross-validation ", sep = "")
  if(x$repeats > 1) cat("repeated", x$repeats, "times ")
  if(!is.null(x$strata)) cat("using stratification")
  cat("\n")
}