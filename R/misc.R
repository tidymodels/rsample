#' Constructor for split objects
#' @param ind A list of integers with names "analysis" and "assessment".
#' @param data A data frame.
#' @param class An optional class to give the object.
#' @keywords internal
#' @export
make_splits <- function(ind, data, class = NULL) {
  res <- rsplit(data, ind$analysis,  ind$assessment)
  if (!is.null(class))
    res <- add_class(res, class)
  res
}

merge_lists <- function(a, b) list(analysis = a, assessment = b)

dim_rset <- function(x, ...) {
  dims <- purrr::map(x$splits, dim)
  dims <- do.call("rbind", dims)
  dims <- tibble::as_tibble(dims)
  id_cols <- grep("^id", colnames(x), value = TRUE)
  for (i in seq_along(id_cols)) {
    dims[id_cols[i]] <- getElement(x, id_cols[i])
  }
  dims
}

names0 <- function(num, prefix = "x") {
  if (num == 0L) {
    return(character())
  }
  ind <- format(1:num)
  ind <- gsub(" ", "0", ind)
  paste0(prefix, ind)
}

add_class <- function(x, cls) {
  class(x) <- c(cls, class(x))
  x
}

strata_check <- function(strata, data) {
  if (!is.null(strata)) {
    if (!is.character(strata) | length(strata) != 1) {
      rlang::abort("`strata` should be a single character value.")
    }
    if (inherits(data[, strata], "Surv")) {
      rlang::abort("`strata` cannot be a `Surv` object. Use the time or event variable directly.")
    }
    if (!(strata %in% names(data))) {
      rlang::abort(strata, " is not in `data`.")
    }
  }
  invisible(NULL)
}

split_unnamed <- function(x, f) {
  out <- split(x, f)
  unname(out)
}

#' Obtain a identifier for the resamples
#'
#' This function returns a hash (or NA) for an attribute that is created when
#' the `rset` was initially constructed. This can be used to compare with other
#' resampling objects to see if they are the same.
#' @param x An `rset` or `tune_results` object.
#' @param ... Not currently used.
#' @return A character value or `NA_character_` if the object was created prior
#' to `rsample` version 0.1.0.
#' @rdname get_fingerprint
#' @aliases .get_fingerprint
#' @examples
#' set.seed(1)
#' .get_fingerprint(vfold_cv(mtcars))
#'
#' set.seed(1)
#' .get_fingerprint(vfold_cv(mtcars))
#'
#' set.seed(2)
#' .get_fingerprint(vfold_cv(mtcars))
#'
#' set.seed(1)
#' .get_fingerprint(vfold_cv(mtcars, repeats = 2))
#' @export
.get_fingerprint <- function(x, ...) {
  UseMethod(".get_fingerprint")
}

#' @export
#' @rdname get_fingerprint
.get_fingerprint.default <- function(x, ...) {
  cls <- paste0("'", class(x), "'", collapse = ", ")
  rlang::abort(
    paste("No `.get_fingerprint()` method for this class(es)", cls)
  )
}

#' @export
#' @rdname get_fingerprint
.get_fingerprint.rset <- function(x, ...) {
  att <- attributes(x)
  if (any(names(att) == "fingerprint")) {
    res <- att$fingerprint
  } else {
    res <- NA_character_
  }
  res
}


#' Create an rsplit object from dataframes
#'
#' @param training A dataframe containing the training set.
#' @param testing A dataframe containing the testing set.
#'
#' @return An rsplit object created from the specified dataframes.
#' @export
split_from_dataframe <- function(training, testing) {

}
