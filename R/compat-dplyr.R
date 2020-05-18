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
#' @section Version Specific Behavior:
#'
#' rsample performs somewhat differently depending on whether you have
#' dplyr >= 1.0.0 (new) or dplyr < 1.0.0 (old). Additionally, version
#' 0.0.7 of rsample (new) introduced some changes to how rsample objects
#' work with dplyr, even on old dplyr. Most of these changes influence the
#' return value of a dplyr verb and determine whether it will be a tibble
#' or an rsample rset subclass.
#'
#' The table below attempts to capture most of these changes. These examples
#' are not exhaustive and may not capture some edge-cases.
#'
#' ## Joins
#'
#' The following affect all of the dplyr joins, such as `left_join()`,
#' `right_join()`, `full_join()`, and `inner_join()`.
#'
#' Joins that alter the rows of the original rset object:
#'
#' | operation                  | old rsample + old dplyr | new rsample + old dplyr | new rsample + new dplyr
#' | :------------------------- | :---------------------: | :---------------------: | :---------------------:
#' | `join(rset, tbl)`          | error                   | error                   | tibble
#'
#' The idea here is that, if there are less rows in the result, the result should
#' not be an rset object. For example, you can't have a 10-fold CV object
#' without 10 rows.
#'
#' Joins that keep the rows of the original rset object:
#'
#' | operation                  | old rsample + old dplyr | new rsample + old dplyr | new rsample + new dplyr
#' | :------------------------- | :---------------------: | :---------------------: | :---------------------:
#' | `join(rset, tbl)`          | error                   | error                   | rset
#'
#' As with the logic above, if the original rset object (defined by the split
#' column and the id column(s)) is left intact, the results should be an rset.
#'
#' ## Row Subsetting
#'
#' As mentioned above, this should result in a tibble if any rows are removed
#' or added. Simply reordering rows still results in a valid rset with new
#' rsample.
#'
#' Cases where rows are removed or added:
#'
#' | operation       | old rsample + old dplyr | new rsample + old dplyr | new rsample + new dplyr
#' | :-------------- | :---------------------: | :---------------------: | :---------------------:
#' | `rset[ind,]`    | tibble                  | tibble                  | tibble
#' | `slice(rset)`   | rset                    | tibble                  | tibble
#' | `filter(rset)`  | rset                    | tibble                  | tibble
#'
#' Cases where all rows are kept, but are possibly reordered:
#'
#' | operation       | old rsample + old dplyr | new rsample + old dplyr | new rsample + new dplyr
#' | :-------------- | :---------------------: | :---------------------: | :---------------------:
#' | `rset[ind,]`    | tibble                  | rset                    | rset
#' | `slice(rset)`   | rset                    | rset                    | rset
#' | `filter(rset)`  | rset                    | rset                    | rset
#' | `arrange(rset)` | rset                    | rset                    | rset
#'
#' ## Column Subsetting
#'
#' When the `splits` column or any `id` columns are dropped or renamed,
#' the result should no longer be considered a valid rset.
#'
#' Cases when the required columns are removed or renamed:
#'
#' | operation       | old rsample + old dplyr | new rsample + old dplyr | new rsample + new dplyr
#' | :-------------- | :---------------------: | :---------------------: | :---------------------:
#' | `rset[,ind]`    | tibble                  | tibble                  | tibble
#' | `select(rset)`  | rset                    | tibble                  | tibble
#' | `rename(rset)`  | tibble                  | tibble                  | tibble
#'
#' Cases when no required columns are affected:
#'
#' | operation       | old rsample + old dplyr | new rsample + old dplyr | new rsample + new dplyr
#' | :-------------- | :---------------------: | :---------------------: | :---------------------:
#' | `rset[,ind]`    | tibble                  | rset                    | rset
#' | `select(rset)`  | rset                    | rset                    | rset
#' | `rename(rset)`  | rset                    | rset                    | rset
#'
#' ## Other Column Operations
#'
#' Cases when the required columns are altered:
#'
#' | operation       | old rsample + old dplyr | new rsample + old dplyr | new rsample + new dplyr
#' | :-------------- | :---------------------: | :---------------------: | :---------------------:
#' | `mutate(rset)`  | rset                    | tibble                  | tibble
#'
#' Cases when no required columns are affected:
#'
#' | operation       | old rsample + old dplyr | new rsample + old dplyr | new rsample + new dplyr
#' | :-------------- | :---------------------: | :---------------------: | :---------------------:
#' | `mutate(rset)`  | rset                    | rset                    | rset
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
  vec_restore(data, template)
}
