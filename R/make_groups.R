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
#' @param balance If `v` is less than the number of unique groups, how should
#'  groups be combined into folds? If `"groups"`, the default, then groups are
#'  combined randomly to balance the number of groups in each fold.
#'  If `"observations"`, then groups are combined to balance the number of
#'  observations in each fold.
make_groups <- function(data, group, v, balance) {
  data_ind <- data.frame(..index = 1:nrow(data), ..group = group)
  data_ind$..group <- as.character(data_ind$..group)

  res <- switch(
    balance,
    "groups" = balance_groups(data_ind, v),
    "observations" = balance_observations(data_ind, v)
  )
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

balance_observations <- function(data_ind, v) {
  while (vec_unique_count(data_ind$..group) > v) {
    freq_table <- vec_count(data_ind$..group)
    # Recategorize the largest group to be collapsed
    # as the smallest group to be kept
    group_to_keep <- vec_slice(freq_table, v)
    group_to_collapse <- vec_slice(freq_table, v + 1)
    collapse_lgl <- vec_in(data_ind$..group, group_to_collapse$key)
    vec_slice(data_ind$..group, collapse_lgl) <- group_to_keep$key
  }
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
