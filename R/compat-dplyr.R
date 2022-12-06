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
