#' Gather an `rset` Object
#'
#' @description
#' `r lifecycle::badge("deprecated")`
#'
#' This method uses `gather` on an `rset` object to stack all of
#'  the non-ID or split columns in the data and is useful for
#'  stacking model evaluation statistics. The resulting data frame
#'  has a column based on the column names of `data` and another for
#'  the values. This method is now deprecated in favor of using
#'  [tidyr::pivot_longer()] directly.
#'
#' @param data An `rset` object.
#' @param key,value,... Not specified in this method and will be
#'  ignored. Note that this means that selectors are ignored if
#'  they are passed to the function.
#' @param na.rm If `TRUE`, will remove rows from output where the
#'  value column in `NA`.
#' @param convert If `TRUE` will automatically run
#'  `type.convert()` on the key column. This is useful if the column
#'  names are actually numeric, integer, or logical.
#' @param factor_key If FALSE, the default, the key values will be
#'  stored as a character vector. If `TRUE`, will be stored as a
#'  factor, which preserves the original ordering of the columns.
#' @return A data frame with the ID columns, a column called
#'  `model` (with the previous column names), and a column called
#'  `statistic` (with the values).
#' @examples
#' library(rsample)
#' cv_obj <- vfold_cv(mtcars, v = 10)
#' cv_obj$lm_rmse <- rnorm(10, mean = 2)
#' cv_obj$nnet_rmse <- rnorm(10, mean = 1)
#'
#' ## now deprecated for rset objects:
#' gather(cv_obj)
#'
#' ## instead of gather, use tidyr::pivot_longer:
#' library(tidyr)
#' library(dplyr)
#' cv_obj %>%
#'   select(-splits) %>%
#'   pivot_longer(-id)
#'
#' @export gather.rset
#' @export
#' @keywords internal
#' @method gather rset
gather.rset <- function(data, key = NULL, value = NULL, ..., na.rm = TRUE,
                        convert = FALSE, factor_key = TRUE) {

  lifecycle::deprecate_warn("0.1.0", "gather.rset()", "tidyr::pivot_longer()")

  if (any(names(data) == "splits")) {
    data <- data %>% dplyr::select(-splits)
  }

  data <- as.data.frame(data)

  id_vars <- grep("^id", names(data), value = TRUE)

  other_vars <- names(data)[!(names(data) %in% id_vars)]
  if (length(other_vars) < 2) {
    rlang::abort(
      paste0(
        "There should be at least two other columns ",
        "(besides `id` variables) in the data set to ",
        "use `gather.rset()`."
      )
    )
  }

  tidyr::gather(
    data,
    key = model,
    value = statistic,
    - !!id_vars,
    na.rm = na.rm,
    convert = convert,
    factor_key = factor_key
  )
}

