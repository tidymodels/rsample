#' Determine the Assessment Samples
#' 
#' Given an \code{rsplit} object, \code{complement} will determine which 
#'   of the data rows are contained in the assessment set. To save space, 
#'   many of the \code{rset} objects will not contain indicies for the 
#'   assessment split. 
#'   
#' @param x An \code{rsplit} object
#' @param ... Not currently used
#' @return A integer vector. 
#' @seealso \code{\link{fill}}
#' @examples 
#' set.seed(28432)
#' fold_rs <- vfold_cv(mtcars)
#' head(fold_rs$splits[[1]]$in_id)
#' fold_rs$splits[[1]]$out_id
#' complement(fold_rs$splits[[1]])
#' @export
complement <- function (x, ...)
  UseMethod("complement")

#' @export
complement.vfold_split <- function(x, ...) {
  if (!all(is.na(x$out_id))) {
    return(x$out_id)
  } else {
    setdiff(1:nrow(x$data), x$in_id)
  }
}
#' @export
complement.mc_split  <- complement.vfold_split
#' @export
complement.loo_split <- complement.vfold_split
#' @export
complement.group_vfold_split <- complement.vfold_split
#' @export
complement.boot_split <- function(x, ...) {
  if (!all(is.na(x$out_id))) {
    return(x$out_id)
  } else {
    (1:nrow(x$data))[-unique(x$in_id)]
  }
}
#' @export
complement.rof_split <- function(x, ...) {
  if (!all(is.na(x$out_id))) {
    return(x$out_id)
  } else {
    stop("Cannot derive the assessment set for this type of resampling.",
         call. = FALSE)
  }
}

#' @export
complement.apparent_split <- function(x, ...) {
  if (!all(is.na(x$out_id))) {
    return(x$out_id)
  } else {
    1:nrow(x$data)
  }
}


#' Add Assessment Indicies
#' 
#' Many \code{rsplit} and \code{rset} objects do not contain indicators for
#'   the assessment samples. \code{fill} can be used to populate the slot
#'   for the appropriate indices. 
#' @param x A \code{rsplit} and \code{rset} object.
#' @param ... Not currently used
#' @return An object of the same time with the integer indicies. 
#' @examples 
#' set.seed(28432)
#' fold_rs <- vfold_cv(mtcars)
#' 
#' fold_rs$splits[[1]]$out_id
#' complement(fold_rs$splits[[1]])
#' 
#' fill(fold_rs$splits[[1]])$out_id
#' 
#' fold_rs_all <- fill(fold_rs)
#' fold_rs_all$splits[[1]]$out_id
#' @export
fill <- function (x, ...) UseMethod("fill")

#' @export
fill.rsplit <- function(x, ...) {
  x$out_id <- complement(x, ...)
  x
}

#' @export
fill.rset <- function(x, ...) {
  x$splits <- map(x$splits, fill)
  x
}


## This will remove the assessment indices from an rsplit object
rm_out <- function(x) {
  x$out_id <- NA
  x
}

