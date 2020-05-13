#' Compatibility with dplyr
#'
#' @description
#' rsample should be fully compatible with dplyr 1.0.0.
#'
#' With older versions of dplyr, there is partial support for the following
#' verbs: `mutate()`, `arrange()`, `filter()`, `rename()`, `select()`, and
#' `slice()`. We strongly recommend updating to dplyr 1.0.0 if possible to
#' get more complete integration with dplyr.
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

dplyr_post_1.0.0 <- function() {
  utils::packageVersion("dplyr") > "0.8.5"
}


if (dplyr_post_1.0.0()) {


# Registered in `.onLoad()`
dplyr_reconstruct.rset <- function(data, template) {
  vec_restore(data, template)
}


} # if (dplyr_post_1.0.0())
