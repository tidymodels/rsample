#' Compatibility with dplyr
#'
#' @description
#' This page lays out the compatibility between rsample and dplyr. The `rset`
#' objects from rsample are a sepeciifc subclass of tibbles, hence standard
#' dyplyr operations like joins as well row or column modifications work.
#' However, whether the operation returns an rset or a tibble depends on the
#' details of the operation.
#'
#' The overarching principle is that any operation which leaves the specific
#' characteristics of an rset intact will return an rset. If an operation
#' modifies any of the following characteristics, the result will be a `tibble`
#' rather than an `rset`:
#'
#' * Rows: The number of rows needs to remain unchanged to retain the rset
#' property. For example, you can't have a 10-fold CV object without 10 rows.
#' The order of the rows can be changed though and the object remains an rset.
#'
#' * Columns: The `splits` column and the `id` column(s) are required for an
#' rset and need to remain untouched. They cannot be dropped, renamed, or
#' modified if the result should remain an rset.
#'
#' ## Joins
#'
#' The following affect all of the dplyr joins, such as `left_join()`,
#' `right_join()`, `full_join()`, and `inner_join()`.
#'
#' The resulting object is an `rset` if the number of rows is unaffected.
#' Rows can be reordered but not added or removed, otherwise the resulting object
#' is a `tibble`.
#'
#' | operation          | same rows, possibly reordered | add or remove rows
#' | :----------------- | :---------------------------: | :---------------------:
#' | `join(rset, tbl)`  | `rset`                        | `tibble`
#'
#' ## Row Operations
#'
#' The resulting object is an `rset` if the number of rows is unaffected.
#' Rows can be reordered but not added or removed, otherwise the resulting object
#' is a `tibble`.
#'
#' | operation          | same rows, possibly reordered | add or remove rows
#' | :----------------- | :---------------------------: | :---------------------:
#' | `rset[ind,]`       | `rset`                        | `tibble`
#' | `slice(rset)`      | `rset`                        | `tibble`
#' | `filter(rset)`     | `rset`                        | `tibble`
#' | `arrange(rset)`    | `rset`                        | `tibble`
#'
#' ## Column Operations
#'
#' The resulting object is an `rset` if the required `splits` and `id` columns
#' remain unaltered. Otherwise the resulting object is a `tibble`.
#'
#' | operation          | required columns unaltered    | required columns removed, renamed, or modified
#' | :----------------- | :---------------------------: | :---------------------:
#' | `rset[,ind]`       | `rset`                        | `tibble`
#' | `select(rset)`     | `rset`                        | `tibble`
#' | `rename(rset)`     | `rset`                        | `tibble`
#' | `mutate(rset)`     | `rset`                        | `tibble`
#'
#' @name rsample-dplyr
NULL

# `dplyr_reconstruct()`
#
# `dplyr_reconstruct()` is called:
# - After a complex dplyr operation, like a `left_join()`, to restore to the
#   type of the first input, `x`.
# - At the end of a call to `dplyr_col_modify()`
# - At the end of a call to `dplyr_row_slice()`
# - See `?dplyr_reconstruct` for the full list.
#
# Because `dplyr_reconstruct()` is called at the end of `dplyr_col_modify()`
# and `dplyr_row_slice()`, we don't need methods for them. The default methods
# in dplyr do the right thing automatically, and then our reconstruction
# method decides whether or not the result should still be an rset.
#
# The implementation for rsample is the same as `vec_restore()`. Generally
# it will fall back to reconstructing a bare tibble, unless the rset structure
# is still completely intact. This happens when rset specific rows and columns
# (splits, id cols) are still exactly identical to how they were before the
# dplyr operation (with the exception of column reordering).

# Registered in `.onLoad()`
dplyr_reconstruct_rset <- function(data, template) {
  rset_reconstruct(data, template)
}
