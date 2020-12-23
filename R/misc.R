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

add_class <- function(x, cls, at_end = TRUE) {
  class(x) <- if (at_end)
    c(class(x), cls)
  else
    c(cls, class(x))
  x
}

strata_check <- function(strata, vars) {
  if (!is.null(strata)) {
    if (!is.character(strata) | length(strata) != 1) {
      rlang::abort("`strata` should be a single character value.")
    }
    if (!(strata %in% vars)) {
      rlang::abort(strata, " is not in `data`.")
    }
  }
  invisible(NULL)
}

split_unnamed <- function(x, f) {
  out <- split(x, f)
  unname(out)
}


## -----------------------------------------------------------------------------

#' Create a cryptographical hash value for `rset` objects.
#'
#' This function uses the distinct rows in the data set and the column(s) for the
#' resample identifier and the splits to produce a character string that can be
#' used to determine if another object shares the same splits.
#'
#' The comparison is based on the unique contents of the `id` and `splits`
#' columns. Attributes are not used in the comparison.
#' @param x An `rset` object.
#' @param ... Not currently used.
#' @return A character string.
#' @examples
#' set.seed(1)
#' fingerprint(vfold_cv(mtcars))
#'
#' set.seed(1)
#' fingerprint(vfold_cv(mtcars))
#'
#' set.seed(2)
#' fingerprint(vfold_cv(mtcars))
#'
#' set.seed(1)
#' fingerprint(vfold_cv(mtcars, repeats = 2))
#' @export
fingerprint <- function(x, ...) {
  # For iterative models, the splits are replicated multiple times. Get the
  # unique id values and has those rows
  id_vars <- grep("^id", names(x), value = TRUE)
  if (length(id_vars) == 0) {
    rlang::abort("No ID columns were found.")
  }
  if (!any(names(x) == "splits")) {
    rlang::abort("The 'split' column was not found.")
  }

  x <-
    dplyr::select(x, splits, dplyr::matches("^id")) %>%
    dplyr::distinct() %>%
    dplyr::arrange(!!!id_vars) %>%
    tibble::as_tibble()
  attr_x <- names(attributes(x))
  attr_rm <- attr_x[!(attr_x %in% c("row.names", "names", "class"))]
  for (i in attr_rm) {
    attr(x, i) <- NULL
  }
  rlang::hash(x)
}

