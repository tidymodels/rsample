#' Make groupings for grouped rsplits
#'
#' This function powers [group_vfold_cv] by splitting the data based upon
#' a grouping variable and returning the assessment set indices for each
#' split.
#'
#' @inheritParams vfold_cv
#' @param group A variable in `data` (single character or name) used for
#'  grouping observations with the same value to either the analysis or
#'  assessment set within a fold.
#'
#' @keywords internal
make_groups <- function(data, group, v) {
  data_ind <- data.frame(..index = 1:nrow(data), ..group = group)
  data_ind$..group <- as.character(data_ind$..group)

  res <- balance_groups(data_ind, v)
  data_ind <- res$data_ind
  keys <- res$keys

  keys$..group <- as.character(keys$..group)

  data_ind <- data_ind %>%
    full_join(keys, by = "..group") %>%
    arrange(..index)
  split_unnamed(data_ind$..index, data_ind$..folds)

}

balance_groups <- function(data_ind, v) {
  unique_groups <- unique(data_ind$..group)
  keys <- data.frame(
    ..group = unique_groups,
    ..folds = sample(rep(seq_len(v), length.out = length(unique_groups)))
  )
  list(
    data_ind = data_ind,
    keys = keys
  )
}
