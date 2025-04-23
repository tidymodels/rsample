rsplit <- function(data, in_id, out_id) {
  if (!is.data.frame(data) & !is.matrix(data)) {
    cli_abort("{.arg data} must be a data frame.")
  }

  if (!is.integer(in_id) | any(in_id < 1)) {
    cli_abort("{.arg in_id} must be a positive integer vector.")
  }

  if (!all(is.na(out_id))) {
    if (!is.integer(out_id) | any(out_id < 1)) {
      cli_abort("{.arg out_id} must be a positive integer vector.")
    }
  }

  if (length(in_id) == 0) {
    cli_abort("At least one row should be selected for the analysis set.")
  }

  structure(
    list(
      data = data,
      in_id = in_id,
      out_id = out_id
    ),
    class = "rsplit"
  )
}

#' @export
print.rsplit <- function(x, ...) {
  out_char <-
    if (is_missing_out_id(x)) {
      paste(length(complement(x)))
    } else {
      paste(length(x$out_id))
    }

  cat("<Analysis/Assess/Total>\n")
  cat("<", length(x$in_id), "/", out_char, "/", nrow(x$data), ">\n", sep = "")
}

#' @export
as.integer.rsplit <-
  function(x, data = c("analysis", "assessment"), ...) {
    data <- match.arg(data)
    if (data == "analysis") {
      out <- x$in_id
    } else {
      out <- if (is_missing_out_id(x)) {
        complement(x)
      } else {
        x$out_id
      }
    }
    out
  }


#' Convert an `rsplit` object to a data frame
#'
#' The analysis or assessment code can be returned as a data
#'   frame (as dictated by the `data` argument) using
#'   `as.data.frame.rsplit()`. `analysis()` and
#'   `assessment()` are shortcuts.
#' @param x An `rsplit` object.
#' @param row.names `NULL` or a character vector giving the row names for the data frame. Missing values are not allowed.
#' @param optional A logical: should the column names of the data be checked for legality?
#' @param data Either `"analysis"` or `"assessment"` to specify which data are returned.
#' @param ... Not currently used.
#' @examples
#' library(dplyr)
#' set.seed(104)
#' folds <- vfold_cv(mtcars)
#'
#' model_data_1 <- folds$splits[[1]] |> analysis()
#' holdout_data_1 <- folds$splits[[1]] |> assessment()
#' @export
as.data.frame.rsplit <-
  function(x, row.names = NULL, optional = FALSE, data = "analysis", ...) {
    if (!is.null(row.names)) {
      cli::cli_warn(
        "{.arg row.names} is kept for consistency with the underlying class but 
        non-NULL values will be ignored."
      )
    }
    if (optional) {
      cli::cli_warn(
        "{.arg optional} is kept for consistency with the underlying class but
        TRUE values will be ignored."
      )
    }
    if (!is.null(x$col_id)) {
      if (identical(data, "assessment")) {
        rsplit_class <- class(x)[[1]]
        cli_abort(
          "There is no assessment data set for an {.arg rsplit} object
          with class {.cls {rsplit_class}}."
        )
      }
      ind <- as.integer(x, data = data, ...)
      permuted_col <- vctrs::vec_slice(x$data, ind) |>
        dplyr::select(x$col_id)
      x$data[, x$col_id] <- permuted_col
      return(x$data)
    }
    row_ind <- as.integer(x, data = data, ...)
    vctrs::vec_slice(x$data, row_ind)
  }

#' @rdname as.data.frame.rsplit
#' @export
analysis <- function(x, ...) {
  UseMethod("analysis")
}

#' @export
#' @rdname as.data.frame.rsplit
analysis.default <- function(x, ...) {
  cls <- class(x)
  cli::cli_abort(
    "No method for objects of class{?es}: {cls}"
  )
}

#' @export
#' @rdname as.data.frame.rsplit
analysis.rsplit <- function(x, ...) {
  as.data.frame(x, data = "analysis", ...)
}
#' @rdname as.data.frame.rsplit
#' @export
assessment <- function(x, ...) {
  UseMethod("assessment")
}

#' @export
#' @rdname as.data.frame.rsplit
assessment.default <- function(x, ...) {
  cls <- class(x)
  cli::cli_abort(
    "No method for objects of class{?es}: {cls}"
  )
}

#' @rdname as.data.frame.rsplit
#' @export
assessment.rsplit <- function(x, ...) {
  as.data.frame(x, data = "assessment", ...)
}

#' @export
dim.rsplit <- function(x, ...) {
  c(
    analysis = length(x$in_id),
    assessment = length(complement(x)),
    n = nrow(x$data),
    p = ncol(x$data)
  )
}

#' @method obj_sum rsplit
#' @export
obj_sum.rsplit <- function(x, ...) {
  out_char <-
    if (is_missing_out_id(x)) {
      paste(length(complement(x)))
    } else {
      paste(length(x$out_id))
    }

  paste0(
    "split [",
    length(x$in_id),
    "/",
    out_char,
    "]"
  )
}


#' @method type_sum rsplit
#' @export
type_sum.rsplit <- function(x, ...) {
  out_char <-
    if (is_missing_out_id(x)) {
      format_n(length(complement(x)))
    } else {
      format_n(length(x$out_id))
    }

  paste0(
    "split [",
    format_n(length(x$in_id)),
    "/",
    out_char,
    "]"
  )
}


format_n <- function(x, digits = 1) {
  case_when(
    log10(x) < 3 ~ paste(x),
    log10(x) >= 3 & log10(x) < 6 ~
      paste0(round(x / 1000, digits = digits), "K"),
    TRUE ~ paste0(round(x / 1000000, digits = digits), "M"),
  )
}

is_rsplit <- function(x) {
  inherits(x, "rsplit")
}
