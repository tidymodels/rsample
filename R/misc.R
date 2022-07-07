#' Constructors for split objects
#' @export
make_splits <- function(x, ...) {
  UseMethod("make_splits")
}

#' @rdname make_splits
#' @param x A list of integers with names "analysis" and "assessment", or a
#' data frame of analysis or training data.
#' @export
make_splits.default <- function(x, ...) {
  rlang::abort("There is no method available to make an rsplit from `x`.")
}

#' @rdname make_splits
#' @param data A data frame.
#' @param class An optional class to give the object.
#' @param ... Further arguments passed to or from other methods (not currently
#' used).
#' @export
make_splits.list <- function(x, data, class = NULL, ...) {
  ellipsis::check_dots_empty()
  res <- rsplit(data, x$analysis, x$assessment)
  if (!is.null(class)) {
    res <- add_class(res, class)
  }
  res
}

#' @rdname make_splits
#' @param assessment A data frame of assessment or testing data, which can be empty.
#' @export
make_splits.data.frame <- function(x, assessment, ...) {
  ellipsis::check_dots_empty()
  if (nrow(x) == 0) {
    rlang::abort("The analysis set must contain at least one row.")
  }

  ind_analysis <- 1:nrow(x)
  if (nrow(assessment) == 0) {
    ind_assessment <- integer()
  } else {
    if (!identical(colnames(x), colnames(assessment))) {
      rlang::abort("The analysis and assessment sets must have the same columns.")
    }
    ind_assessment <- nrow(x) + 1:nrow(assessment)
  }

  data <- bind_rows(x, assessment)
  ind <- list(
    analysis = ind_analysis,
    assessment = ind_assessment
  )

  make_splits(ind, data)
}

merge_lists <- function(a, b) list(analysis = a, assessment = b)

dim_rset <- function(x, ...) {
  dims <- purrr::map(x$splits, dim)
  dims <- do.call("rbind", dims)
  dims <- tibble::as_tibble(dims)
  id_cols <- grep("(^id$)|(^id[1-9]$)", colnames(x), value = TRUE)
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
      rlang::abort("`strata` should be a single name or character value.")
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

#' Reverse the analysis and assessment sets
#'
#' This functions "swaps" the analysis and assessment sets of either a single
#' `rsplit` or all `rsplit`s in the `splits` column of an `rset` object.
#'
#' @param x An `rset` or `rsplit` object.
#' @inheritParams rlang::args_dots_empty
#'
#' @return An object of the same class as `x`
#'
#' @examples
#' set.seed(123)
#' starting_splits <- vfold_cv(mtcars, v = 3)
#' reverse_splits(starting_splits)
#' reverse_splits(starting_splits$splits[[1]])
#'
#' @rdname reverse_splits
#' @export
reverse_splits <- function(x, ...) {
  UseMethod("reverse_splits")
}

#' @rdname reverse_splits
#' @export
reverse_splits.default <- function(x, ...) {
  rlang::abort(
    "`x` must be either an `rsplit` or an `rset` object"
  )
}

#' @rdname reverse_splits
#' @export
reverse_splits.permutations <- function(x, ...) {
  rlang::abort(
    "Permutations cannot have their splits reversed"
  )
}

#' @rdname reverse_splits
#' @export
reverse_splits.perm_split <- reverse_splits.permutations

#' @rdname reverse_splits
#' @export
reverse_splits.rsplit <- function(x, ...) {

  rlang::check_dots_empty()

  out_splits <- list(
    analysis = as.integer(x, data = "assessment"),
    assessment = as.integer(x, data = "analysis")
  )
  out_splits <- make_splits(out_splits, x$data)
  class(out_splits) <- class(x)
  out_splits

}

#' @rdname reverse_splits
#' @export
reverse_splits.rset <- function(x, ...) {

  rlang::check_dots_empty()

  x$splits <- purrr::map(x$splits, reverse_splits)

  x
}

#' "Reshuffle" an rset to re-generate a new rset with the same parameters
#'
#' This function re-generates an rset object, using the same arguments used
#' to generate the original.
#'
#' @param rset The `rset` object to be reshuffled
#'
#' @return An rset of the same class as `rset`.
#'
#' @examples
#' set.seed(123)
#' (starting_splits <- group_vfold_cv(mtcars, cyl, v = 3))
#' reshuffle_rset(starting_splits)
#'
#' @export
reshuffle_rset <- function(rset) {
  if (!inherits(rset, "rset")) {
    rlang::abort("`rset` must be an rset object")
  }

  if (inherits(rset, "manual_rset")) {
    rlang::abort("`manual_rset` objects cannot be reshuffled")
  }

  arguments <- attributes(rset)
  useful_arguments <- names(formals(arguments$class[[1]]))
  useful_arguments <- arguments[useful_arguments]
  useful_arguments <- useful_arguments[!is.na(names(useful_arguments))]
  if (identical(useful_arguments$strata, FALSE)) {
    useful_arguments$strata <- NULL
  } else if (identical(useful_arguments$strata, TRUE)) {
    rlang::abort(
      "Cannot reshuffle this rset (`attr(rset, 'strata')` is `TRUE`, not a column identifier)",
      i = "If the original object was created with an older version of rsample, try recreating it with the newest version of the package"
    )
  }

  do.call(
    arguments$class[[1]],
    c(list(data = rset$splits[[1]]$data), useful_arguments)
  )
}
