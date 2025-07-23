#' Constructors for split objects
#' @export
#' @examples
#' df <- data.frame(
#'   year = 1900:1999,
#'   value = 10 + 8*1900:1999 + runif(100L, 0, 100)
#' )
#' split_from_indices <- make_splits(
#'   x = list(analysis = which(df$year <= 1980),
#'            assessment = which(df$year > 1980)),
#'   data = df
#' )
#' split_from_data_frame <- make_splits(
#'   x = df[df$year <= 1980,],
#'   assessment = df[df$year > 1980,]
#' )
#' identical(split_from_indices, split_from_data_frame)
make_splits <- function(x, ...) {
  UseMethod("make_splits")
}

#' @rdname make_splits
#' @param x A list of integers with names "analysis" and "assessment", or a
#' data frame of analysis or training data.
#' @export
make_splits.default <- function(x, ...) {
  cls <- class(x)
  cli_abort("No method for objects of class{?es}: {cls}")
}

#' @rdname make_splits
#' @param data A data frame.
#' @param class An optional class to give the object.
#' @param ... Not currently used.
#' @export
make_splits.list <- function(x, data, class = NULL, ...) {
  rlang::check_dots_empty()
  res <- rsplit(data, x$analysis, x$assessment)
  if (!is.null(class)) {
    res <- add_class(res, class)
  }
  res
}

#' @rdname make_splits
#' @param assessment A data frame of assessment or testing data, which can be empty.
#' @export
make_splits.data.frame <- function(x, assessment, class = NULL, ...) {
  rlang::check_dots_empty()
  if (nrow(x) == 0) {
    cli_abort("The analysis set must contain at least one row.")
  }

  ind_analysis <- seq_len(nrow(x))
  if (nrow(assessment) == 0) {
    ind_assessment <- integer()
  } else {
    if (!identical(colnames(x), colnames(assessment))) {
      cli_abort("The analysis and assessment sets must have the same columns.")
    }
    ind_assessment <- nrow(x) + seq_len(nrow(assessment))
  }

  data <- bind_rows(x, assessment)
  ind <- list(
    analysis = ind_analysis,
    assessment = ind_assessment
  )

  make_splits(ind, data, class = class)
}

merge_lists <- function(a, b) list(analysis = a, assessment = b)

dim_rset <- function(x, ...) {
  check_dots_empty()
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

check_prop <- function(prop, call = caller_env()) {
  check_number_decimal(prop, call = call)
  if (!(prop > 0)) {
    cli_abort("{.arg prop} must be greater than 0.", call = call)
  }
  if (!(prop < 1)) {
    cli_abort("{.arg prop} must be less than 1.", call = call)
  }
  invisible(NULL)
}

check_strata <- function(strata, data, call = caller_env()) {
  check_string(strata, allow_null = TRUE, call = call)

  if (!is.null(strata)) {
    if (inherits(data[, strata], "Surv")) {
      cli_abort(
        c(
          "{.field strata} cannot be a {.cls Surv} object.",
          "i" = "Use the time or event variable directly."
        ),
        call = call
      )
    }
  }
  invisible(NULL)
}

split_unnamed <- function(x, f) {
  out <- split(x, f)
  unname(out)
}

#' Get the split arguments from an rset
#' @param x An `rset` or `initial_split` object.
#' @param allow_strata_false A logical to specify which value to use if no
#' stratification was specified. The default is to use `strata = NULL`, the
#' alternative is `strata = FALSE`.
#' @return A list of arguments used to create the rset.
#' @keywords internal
#' @export
.get_split_args <- function(x, allow_strata_false = FALSE) {
  all_attributes <- attributes(x)
  function_used_to_create <- switch(
    all_attributes$class[[1]],
    "validation_set" = "initial_validation_split",
    "group_validation_set" = "group_initial_validation_split",
    "time_validation_set" = "initial_validation_time_split",
    all_attributes$class[[1]]
  )
  args <- names(formals(function_used_to_create))

  if ("initial_validation_split" %in% all_attributes$class) {
    split_args <- all_attributes$val_att
  } else {
    split_args <- all_attributes[args]
  }
  split_args <- split_args[!is.na(names(split_args))]

  if (identical(split_args$strata, FALSE) && !allow_strata_false) {
    split_args$strata <- NULL
  }
  split_args
}

#' Retrieve individual rsplits objects from an rset
#'
#' @param x The `rset` object to retrieve an rsplit from.
#' @param index An integer indicating which rsplit to retrieve: `1` for the
#' rsplit in the first row of the rset, `2` for the second, and so on.
#' @param ... Not currently used.
#'
#' @return The rsplit object in row `index` of `rset`
#'
#' @examples
#' set.seed(123)
#' (starting_splits <- group_vfold_cv(mtcars, cyl, v = 3))
#' get_rsplit(starting_splits, 1)
#'
#' @rdname get_rsplit
#' @export
get_rsplit <- function(x, index, ...) {
  UseMethod("get_rsplit")
}

#' @rdname get_rsplit
#' @export
get_rsplit.rset <- function(x, index, ...) {
  rlang::check_dots_empty()

  n_rows <- nrow(x)

  acceptable_index <- length(index) == 1 &&
    rlang::is_integerish(index) &&
    index > 0 &&
    index <= n_rows

  if (!acceptable_index) {
    msg <- ifelse(
      length(index) != 1,
      glue::glue("Index was of length {length(index)}."),
      glue::glue("A value of {index} was provided.")
    )

    cli_abort(
      c(
        "{.arg index} must be a length-1 integer between 1 and {n_rows}.",
        "*" = msg
      )
    )
  }

  x$splits[[index]]
}

#' @rdname get_rsplit
#' @export
get_rsplit.default <- function(x, index, ...) {
  cls <- class(x)
  cli_abort("No method for objects of class{?es}: {cls}")
}
