#' Create a Validation Set
#'
#' `validation_split()` takes a single random sample (without replacement) of
#'  the original data set to be used for analysis. All other data points are
#'  added to the assessment set (to be used as the validation set).
#' @template strata_details
#' @inheritParams vfold_cv
#' @inheritParams make_strata
#' @param prop The proportion of data to be retained for modeling/analysis.
#' @export
#' @return An tibble with classes `validation_split`, `rset`, `tbl_df`, `tbl`,
#'  and `data.frame`. The results include a column for the data split objects
#'  and a column called `id` that has a character string with the resample
#'  identifier.
#' @examples
#' validation_split(mtcars, prop = .9)
#' @export
validation_split <- function(data, prop = 3/4,
                             strata = NULL, breaks = 4, pool = 0.1, ...) {

  if (!missing(strata)) {
    strata <- tidyselect::vars_select(names(data), !!enquo(strata))
    if (length(strata) == 0) {
      strata <- NULL
    }
  }

  strata_check(strata, data)

  split_objs <-
    mc_splits(data = data,
              prop = prop,
              times = 1,
              strata = strata,
              breaks = breaks,
              pool = pool)

  ## We remove the holdout indices since it will save space and we can
  ## derive them later when they are needed.

  split_objs$splits <- map(split_objs$splits, rm_out)
  class(split_objs$splits[[1]]) <- c("val_split", "rsplit")

  val_att <- list(prop = prop,
                 strata = !is.null(strata))

  new_rset(splits = split_objs$splits,
           ids = "validation",
           attrib = val_att,
           subclass = c("validation_split", "rset"))
}

#' @export
print.validation_split <- function(x, ...) {
  cat("#", pretty(x), "\n")
  class(x) <- class(x)[!(class(x) %in% c("validation_split", "rset"))]
  print(x, ...)
}


#' @export
print.val_split<- function(x, ...) {

  if (is_missing_out_id(x)) {
    out_char <- paste(length(complement(x)))
  } else {
    out_char <- paste(length(x$out_id))
  }

  cat("<Training/Validation/Total>\n")
  cat("<",
      length(x$in_id), "/",
      out_char, "/",
      nrow(x$data), ">\n",
      sep = "")
}
