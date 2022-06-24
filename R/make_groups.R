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
#'  groups be combined into folds? See the available functions listed in
#'  [balance_groups()].
#'
#' @keywords internal
make_groups <- function(data, group, v, balance = balance_groups()) {
  data_ind <- data.frame(..index = 1:nrow(data), ..group = group)
  data_ind$..group <- as.character(data_ind$..group)

  if (!rlang::is_list(balance)) {
    rlang::abort(
      c(
        "`balance` must be a list created by a balancer function",
        i = "See the available options in ?rsample::balance"
      ),
      call = rlang::caller_env()
    )
  }

  res <- do.call(
    balance$fn,
    c(
      balance$args,
      list(
        data_ind = data_ind,
        v = v
      )
    ))
  data_ind <- res$data_ind
  keys <- res$keys

  data_ind$..group <- as.character(data_ind$..group)
  keys$..group <- as.character(keys$..group)

  data_ind <- data_ind %>%
    full_join(keys, by = "..group") %>%
    arrange(..index)
  split_unnamed(data_ind$..index, data_ind$..folds)

}

#' Balance group distribution across folds
#'
#' [balance_groups()] assigns groups to folds so that approximately equal
#' numbers of groups are in each fold.
#'
#' @inheritParams rlang::args_dots_empty
#'
#' @return A list with three elements: `fn`, the function which will actually
#'  produce groups for the resampling method, `args`, a list of
#'  any balancing-method specific arguments that `fn` requires, and `type`,
#'  indicating what balancing method is being used.
#'
#' @rdname balance
#' @export
balance_groups <- function(...) {
  rlang::check_dots_empty()
  list(
    fn = make_balance_groups,
    args = list(),
    type = "balance_groups"
  )
}

make_balance_groups <- function(..., data_ind, v) {
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

#' @rdname balance
#' @export
balance_observations <- function(...) {
  rlang::check_dots_empty()
  list(
    fn = make_balance_observations,
    args = list(),
    type = "balance_observations"
  )
}

make_balance_observations <- function(..., data_ind, v) {
  n_obs <- nrow(data_ind)
  target_per_fold <- 1 / v

  freq_table <- vec_count(data_ind$..group)
  freq_table <- freq_table[sample.int(nrow(freq_table)), ]
  freq_table$assignment <- NA
  freq_table$assignment[seq_len(v)] <- seq_len(v)

  while (any(is.na(freq_table$assignment))) {
    next_row <- which(is.na(freq_table$assignment))[[1]]
    next_size <- freq_table[next_row, ]$count

    group_breakdown <- freq_table %>%
      stats::na.omit() %>%
      dplyr::group_by(.data$assignment) %>%
      dplyr::summarise(count = sum(.data$count), .groups = "drop") |>
      dplyr::mutate(prop = .data$count / n_obs,
                    pre_error = abs(.data$prop - target_per_fold),
                    if_added_count = .data$count + next_size,
                    if_added_prop = .data$if_added_count / n_obs,
                    post_error = abs(.data$if_added_prop - target_per_fold),
                    improvement = .data$post_error - .data$pre_error)

    most_improved <- which.min(group_breakdown$improvement)
    freq_table[next_row, ]$assignment <- group_breakdown[most_improved, ]$assignment
  }
  data_ind <- dplyr::left_join(data_ind, freq_table, by = c("..group" = "key"))
  data_ind$..group <- data_ind$assignment
  data_ind <- data_ind[c("..index", "..group")]

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
