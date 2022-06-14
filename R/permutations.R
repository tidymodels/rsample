#' Permutation sampling
#'
#' @description
#' A permutation sample is the same size as the original data set and is made
#'   by permuting/shuffling one or more columns. This results in analysis
#'   samples where some columns are in their original order and some columns
#'   are permuted to a random order. Unlike other sampling functions in
#'   `rsample`, there is no assessment set and calling `assessment()` on a
#'   permutation split will throw an error.
#'
#' @param data A data frame.
#' @param permute One or more columns to shuffle. This argument supports
#'   `tidyselect` selectors. Multiple expressions can be combined with `c()`.
#'   Variable names can be used as if they were positions in the data frame, so
#'   expressions like `x:y` can be used to select a range of variables.
#'   See \code{\link[tidyselect]{language}} for more details.
#' @param times The number of permutation samples.
#' @param apparent A logical. Should an extra resample be added where the
#'   analysis is the standard data set.
#' @param ... Not currently used.
#'
#' @details The argument `apparent` enables the option of an additional
#'   "resample" where the analysis data set is the same as the original data
#'   set. Permutation-based resampling can be especially helpful for computing
#'   a statistic under the null hypothesis (e.g. t-statistic). This forms the
#'   basis of a permutation test, which computes a test statistic under all
#'   possible permutations of the data.
#'
#' @return A `tibble` with classes `permutations`, `rset`, `tbl_df`, `tbl`, and
#'   `data.frame`. The results include a column for the data split objects and a
#'   column called `id` that has a character string with the resample
#'   identifier.
#'
#' @examples
#' permutations(mtcars, mpg, times = 2)
#' permutations(mtcars, mpg, times = 2, apparent = TRUE)
#'
#' library(purrr)
#' resample1 <- permutations(mtcars, starts_with("c"), times = 1)
#' resample1$splits[[1]] %>% analysis()
#'
#' resample2 <- permutations(mtcars, hp, times = 10, apparent = TRUE)
#' map_dbl(resample2$splits, function(x) {
#'   t.test(hp ~ vs, data = analysis(x))$statistic
#' })
#' @export
permutations <- function(data,
                         permute = NULL,
                         times = 25,
                         apparent = FALSE,
                         ...) {
  permute <- rlang::enquo(permute)
  if (is.null(permute)) {
    rlang::abort("You must specify at least one column to permute!")
  }
  col_id <- tidyselect::eval_select(permute, data)

  if (identical(length(col_id), 0L)) {
    rlang::abort("You must specify at least one column to permute!")
  } else if (identical(length(col_id), ncol(data))) {
    rlang::abort("You have selected all columns to permute. This effectively reorders the rows in the original data without changing the data structure. Please select fewer columns to permute.")
  }

  split_objs <- perm_splits(data, times)

  if (apparent) {
    split_objs <- dplyr::bind_rows(split_objs, apparent(data))
  }

  split_objs$splits <-
    purrr::map(split_objs$splits, function(x) {
      x$col_id <- col_id
      x
    })

  perm_att <- list(
    times = times,
    apparent = apparent,
    col_id = col_id
  )

  new_rset(
    splits = split_objs$splits,
    ids = split_objs$id,
    attrib = perm_att,
    subclass = c("permutations", "rset")
  )
}

perm_complement <- function(ind, n) {
  list(analysis = ind, assessment = NA)
}

perm_splits <- function(data, times = 25) {
  n <- nrow(data)
  indices <- purrr::map(rep(n, times), sample, replace = FALSE)

  indices <- lapply(indices, perm_complement, n = n)

  split_objs <-
    purrr::map(indices, make_splits, data = data, class = "perm_split")

  list(
    splits = split_objs,
    id = names0(length(split_objs), "Permutations")
  )
}
