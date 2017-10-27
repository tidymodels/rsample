#' Find Labels from rset Object
#'
#' Produce a vector of resampling labels (e.g. "Fold1") from
#'  an `rset` object. Currently, `nested_cv`
#'  is not supported.
#'
#' @param object An `rset` object
#' @param make_factor A logical for whether the results should be
#'  character or a factor.
#' @param ... Not currently used.
#' @return A single character or factor vector.
#' @export
#' @examples
#' labels(vfold_cv(mtcars))
labels.rset <- function(object, make_factor = FALSE, ...) {
  if (inherits(object, "nested_cv"))
    stop("`labels` not implemented for nested resampling",
         call. = FALSE)
  if (make_factor)
    as.factor(object$id)
  else
    as.character(object$id)
}

#' @rdname labels.rset
#' @export
labels.vfold_cv <- function(object, make_factor = FALSE, ...) {
  if (inherits(object, "nested_cv"))
    stop("`labels` not implemented for nested resampling",
         call. = FALSE)
  is_repeated <- attr(object, "repeats") > 1
  if (is_repeated) {
    out <- as.character(paste(object$id, object$id2, sep = "."))
  } else
    out <- as.character(object$id)
  if (make_factor)
    out <- as.factor(out)
  out
}

#' Find Labels from rsplit Object
#'
#' Produce a tibble of identification variables so that single 
#'  splits can be linked to a particular resample. 
#'
#' @param object An `rsplit` object
#' @param ... Not currently used.
#' @return A tibble.
#' @export
#' @examples
#' cv_splits <- vfold_cv(mtcars)
#' labels(cv_splits$splits[[1]])
labels.rsplit <- function(object, ...) {
  out <- if ("id" %in% names(object))
    object$id
  else
    tibble()
  out
}

## The `pretty` methods below are good for when you need to
## textually describe the resampling procedure. Note that they
## can have more than one element (in the case of nesting)


#' Short Decriptions of rsets
#'
#' Produce a chracter vector of describing the resampling method.
#'
#' @param x An `rset` object
#' @param ... Not currently used.
#' @return A character vector.
#' @exportMethod pretty.vfold_cv
#' @export pretty.vfold_cv
#' @export
#' @method pretty vfold_cv
#' @keywords internal
pretty.vfold_cv <- function(x, ...) {
  details <- attributes(x)
  res <- paste0(details$v, "-fold cross-validation")
  if (details$repeats > 1)
    res <- paste(res, "repeated", details$repeats, "times")
  if (details$strata)
    res <- paste(res, "using stratification")
  res
}

#' @exportMethod pretty.loo_cv
#' @export pretty.loo_cv
#' @export
#' @method pretty loo_cv
#' @rdname pretty.vfold_cv
pretty.loo_cv <- function(x, ...)
  "Leave-one-out cross-validation"

#' @exportMethod pretty.apparent
#' @export pretty.apparent
#' @export
#' @method pretty apparent
#' @rdname pretty.vfold_cv
pretty.apparent <- function(x, ...)
  "Apparent sampling"

#' @exportMethod pretty.rolling_origin
#' @export pretty.rolling_origin
#' @export
#' @method pretty rolling_origin
#' @rdname pretty.vfold_cv
pretty.rolling_origin <- function(x, ...)
  "Rolling origin forecast resampling"

#' @exportMethod pretty.mc_cv
#' @export pretty.mc_cv
#' @export
#' @method pretty mc_cv
#' @rdname pretty.vfold_cv
pretty.mc_cv <- function(x, ...) {
  details <- attributes(x)
  res <- paste0(
    "# Monte Carlo cross-validation (",
    signif(details$prop, 2),
    "/",
    signif(1 - details$prop, 2),
    ") with ",
    details$times,
    " resamples "
  )
  if (details$strata)
    res <- paste(res, "using stratification")
  res
}

#' @exportMethod pretty.nested_cv
#' @export pretty.nested_cv
#' @export
#' @method pretty nested_cv
#' @rdname pretty.vfold_cv
pretty.nested_cv <- function(x, ...) {
  print(class(x))
  details <- attributes(x)

  if (is_lang(details$outside)) {
    class(x) <- class(x)[!(class(x) == "nested_cv")]
    outer_label <- pretty(x)
  } else {
    outer_label <- paste0("`", deparse(details$outside), "`")
  }

  inner_label <- if (is_lang(details$inside))
    pretty(x$inner_resamples[[1]])
  else
    paste0("`", deparse(details$inside), "`")

  res <- c("Nested resampling:",
           paste(" outer:", outer_label),
           paste(" inner:", inner_label))
  res
}


#' @exportMethod pretty.bootstraps
#' @export pretty.bootstraps
#' @export
#' @method pretty bootstraps
#' @rdname pretty.vfold_cv
pretty.bootstraps <- function(x, ...) {
  details <- attributes(x)
  res <- "Bootstrap sampling"
  if (details$strata)
    res <- paste(res, "using stratification")
  if (details$apparent)
    res <- paste(res, "with apparent sample")
  res
}


#' @exportMethod pretty.group_vfold_cv
#' @export pretty.group_vfold_cv
#' @export
#' @method pretty group_vfold_cv
#' @rdname pretty.vfold_cv
pretty.group_vfold_cv  <- function(x, ...) {
  details <- attributes(x)
  paste0("Group ", details$v, "-fold cross-validation")
}

