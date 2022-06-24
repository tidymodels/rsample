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
  data_ind <- tibble(..index = 1:nrow(data), ..group = group)
  data_ind$..group <- as.character(data_ind$..group)

  validate_balance(balance, call = rlang::caller_env())

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
      dplyr::summarise(count = sum(.data$count), .groups = "drop") %>%
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

#' @inheritParams mc_cv
#' @rdname balance
#' @export
balance_prop <- function(prop = 3 / 4, ...) {
  rlang::check_dots_empty()
  list(
    fn = make_balance_prop,
    args = list(
      prop = prop
    ),
    type = "balance_prop"
  )
}

make_balance_prop <- function(prop, ..., data_ind, v) {
  if (!is.numeric(prop) | prop >= 1 | prop <= 0) {
    rlang::abort("`prop` must be a number on (0, 1).")
  }
  n_obs <- nrow(data_ind)

  freq_table <- vec_count(data_ind$..group)

  freq_table <- purrr::map_dfr(
    seq_len(v),
    function(x) {
      freq_table <- freq_table[sample.int(nrow(freq_table)), ]
      cumulative_proportion <- cumsum(freq_table$count) / sum(freq_table$count)
      crosses_target <- which(cumulative_proportion > prop)[[1]]
      is_closest <- cumulative_proportion[c(crosses_target, crosses_target - 1)]
      is_closest <- which.min(abs(is_closest - prop)) - 1
      crosses_target <- crosses_target - is_closest
      out <- freq_table[seq_len(crosses_target), ]
      out$assignment <- x
      out
    }
  )

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

validate_balance <- function(balance, banned_balancers = NULL, call = rlang::caller_env()) {
  if (!rlang::is_list(balance)) {
    rlang::abort(
      c(
        "`balance` must be a list created by a balancer function",
        i = "See the available options in `?rsample::balance_groups`"
      ),
      call = call
    )
  }

  if (!is.null(banned_balancers)) {
    if (balance$type %in% banned_balancers) {
      rlang::abort(
        "`{balancer}` is not a permitted balancer for this resampling method",
        balancer = balance$type
      )
    }
  }
}

validate_group <- function(group, data, call = rlang::caller_env()) {
  if (!missing(group)) {
    group <- tidyselect::vars_select(names(data), !!enquo(group))
    if (length(group) == 0) {
      group <- NULL
    }
  }

  if (is.null(group) || !is.character(group) || length(group) != 1) {
    rlang::abort(
      "`group` should be a single character value for the column that will be used for splitting.",
      call = call
    )
  }
  if (!any(names(data) == group)) {
    rlang::abort("`group` should be a column in `data`.", call = call)
  }

  group
}
