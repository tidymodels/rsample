#' Make groupings for grouped rsplits
#'
#' This function powers grouped resampling by splitting the data based upon
#' a grouping variable and returning the assessment set indices for each
#' split.
#'
#' @inheritParams vfold_cv
#' @param group A variable in `data` (single character or name) used for
#'  grouping observations with the same value to either the analysis or
#'  assessment set within a fold.
#' @param balance If `v` is less than the number of unique groups, how should
#'  groups be combined into folds? Should be one of
#'  `"groups"`, `"observations"`, `"prop"`.
#' @param ... Arguments passed to balance functions.
#'
#' @details
#' Not all `balance` options are accepted -- or make sense -- for all resampling
#'  functions. For instance, `balance = "prop"` assigns groups to folds at
#'  random, meaning that any given observation is not guaranteed to be in one
#'  (and only one) assessment set. That means `balance = "prop"` can't
#'  be used with [group_vfold_cv()], and so isn't an option available for that
#'  function.
#'
#' Similarly, [group_mc_cv()] and its derivatives don't assign data to one (and
#'  only one) assessment set, but rather allow each observation to be in an
#'  assessment set zero-or-more times. As a result, those functions don't have
#'  a `balance` argument, and under the hood always specify `balance = "prop"`
#'  when they call `make_groups()`.
#'
#' @keywords internal
make_groups <- function(data,
                        group,
                        v,
                        balance = c("groups", "observations", "prop"),
                        strata = NULL,
                        ...) {
  rlang::check_dots_used(call = rlang::caller_env())
  balance <- rlang::arg_match(balance, error_call = rlang::caller_env())

  data_ind <- tibble(
    ..index = seq_len(nrow(data)),
    ..group = group
  )
  data_ind$..group <- as.character(data_ind$..group)

  res <- switch(
    balance,
    "groups" = balance_groups(
      data_ind = data_ind,
      v = v,
      strata = strata,
      ...
    ),
    "observations" = balance_observations(
      data_ind = data_ind,
      v = v,
      strata = strata,
      ...
    ),
    "prop" = balance_prop(
      data_ind = data_ind,
      v = v,
      strata = strata,
      ...
    )
  )

  data_ind <- res$data_ind
  keys <- res$keys

  data_ind$..group <- as.character(data_ind$..group)
  keys$..group <- as.character(keys$..group)

  data_ind <- data_ind %>%
    full_join(keys, by = "..group") %>%
    arrange(..index)
  split_unnamed(data_ind$..index, data_ind$..folds)

}

balance_groups <- function(data_ind, v, strata = NULL, ...) {
  if (is.null(strata)) {
    balance_groups_normal(data_ind, v, ...)
  } else {
    balance_groups_strata(data_ind, v, strata, ...)
  }
}

balance_groups_normal <- function(data_ind, v, ...) {
  rlang::check_dots_empty()
  unique_groups <- unique(data_ind$..group)
  keys <- data.frame(
    ..group = unique_groups,
    ..folds = sample(
      rep(seq_len(v), length.out = length(unique_groups))
    )
  )
  list(
    data_ind = data_ind,
    keys = keys
  )
}

balance_groups_strata <- function(data_ind, v, strata, ...) {
  rlang::check_dots_empty()

  data_ind$..strata <- strata
  # Create a table that's all the unique group x strata combinations:
  keys <- vctrs::vec_unique(data_ind[c("..group", "..strata")])
  # Create as many fold IDs as there are group x strata,
  # in repeating order (1, 2, ..., n, 1, 2, ..., n)
  folds <- rep(1:v, length.out = nrow(keys))

  # Split the folds based on how many groups are within each strata
  # So if the first strata in sort is 3, and v is 2, that strata gets a
  # c(1, 2, 1) for fold IDs
  #
  # This means that, if nrow(keys) %% v == 0, each fold should have
  # the same number of groups from each strata
  #
  # We randomize "keys" here so that the function is stochastic even for
  # strata with only one group:
  unique_strata <- unique(keys$..strata)
  keys_order <- sample.int(length(unique_strata))

  # Re-order the keys data.frame based on the reshuffled strata variable:
  keys <- keys[
    order(match(keys$..strata, unique_strata[keys_order])),
  ]

  # And split both folds and keys with the reordered strata vector:
  folds <- split_unnamed(folds, keys$..strata)
  keys <- split_unnamed(keys, keys$..strata)

  # Randomly assign fold IDs to each group within each strata
  keys <- purrr::map2(
    keys,
    folds,
    function(x, y) {
      x$..folds <- sample(y)
      x
    }
  )

  keys <- dplyr::bind_rows(keys)
  keys <- keys[c("..group", "..folds")]
  list(
    data_ind = data_ind,
    keys = keys
  )
}

balance_observations <- function(data_ind, v, strata = NULL, ...) {
  rlang::check_dots_empty()
  n_obs <- nrow(data_ind)
  target_per_fold <- 1 / v

  # This is the core difference between stratification and not:
  #
  # Without stratification, data_ind is broken into v groups,
  # which are roughly balanced based on the number of observations
  #
  # With strata, data_ind is split up by strata, and then each _split_
  # is broken into v groups (which are then combined with the other strata);
  # the balancing for each fold is done separately inside each strata "split"
  data_splits <- if (is.null(strata)) {
    list(data_ind)
  } else {
    split_unnamed(data_ind, strata)
  }

  freq_table <- purrr::map(
    data_splits,
    balance_observations_helper,
    v = v,
    target_per_fold = target_per_fold
  ) %>%
    list_rbind()

  collapse_groups(freq_table, data_ind, v)
}

balance_observations_helper <- function(data_split, v, target_per_fold) {

  n_obs <- nrow(data_split)
  # Create a frequency table counting how many of each group are in the data:
  freq_table <- vec_count(data_split$..group, sort = "location")
  # Randomly shuffle that table, then assign the first few rows to folds
  # (to ensure that each fold gets at least one group assigned):
  freq_table <- freq_table[sample.int(nrow(freq_table)), ]
  freq_table$assignment <- NA
  # Assign the first `v` rows to folds, so that each fold has _some_ data:
  freq_table$assignment[seq_len(v)] <- seq_len(v)

  # Each run of this loop assigns one "NA" assignment to a fold,
  # so we won't get caught in an endless loop here
  while (any(is.na(freq_table$assignment))) {
    # Get the index of the next row to be assigned, and its count:
    next_row <- which(is.na(freq_table$assignment))[[1]]
    next_size <- freq_table[next_row, ]$count

    # Calculate which fold to assign this new row into:
    group_breakdown <- freq_table %>%
      # The only NA column in freq_table should be assignment
      # So this should only drop un-assigned groups:
      stats::na.omit() %>%
      # Group by fold assignments and count data in each fold:
      dplyr::group_by(.data$assignment) %>%
      dplyr::summarise(count = sum(.data$count), .groups = "drop") %>%
      # Calculate...:
      dplyr::mutate(
        # The proportion of data in each fold so far,
        prop = .data$count / n_obs,
        # The amount off from the target proportion so far,
        pre_error = abs(.data$prop - target_per_fold),
        # The amount off from the target proportion if we add this new group,
        if_added_count = .data$count + next_size,
        if_added_prop = .data$if_added_count / n_obs,
        post_error = abs(.data$if_added_prop - target_per_fold),
        # And how much better or worse adding this new group would make things
        improvement = .data$post_error - .data$pre_error
      )

    # Assign the group in question to the best fold and move on to the next one:
    most_improved <- which.min(group_breakdown$improvement)
    freq_table[next_row, ]$assignment <-
      group_breakdown[most_improved, ]$assignment
  }
  freq_table
}

balance_prop <- function(prop, data_ind, v, replace = FALSE, strata = NULL, ...) {
  rlang::check_dots_empty()
  check_prop(prop, replace)

  # This is the core difference between stratification and not:
  #
  # Without stratification, `prop`% of `data_ind` is sampled `v` times;
  # the resampling is done with the entire set of groups
  #
  # With strata, data_ind is split up by strata, and then each _split_
  # has `prop`% of `data_ind` is sampled `v` times;
  # the resampling for each iteration is done inside each strata "split"
  data_splits <- if (is.null(strata)) {
    list(data_ind)
  } else {
    split_unnamed(data_ind, strata)
  }

  freq_table <- purrr::map(
    data_splits,
    balance_prop_helper,
    prop = prop,
    v = v,
    replace = replace
  ) %>%
    list_rbind()

  collapse_groups(freq_table, data_ind, v)
}

balance_prop_helper <- function(prop, data_ind, v, replace) {

  freq_table <- vec_count(data_ind$..group, sort = "location")

  # Calculate how many groups to sample each iteration
  # If sampling with replacement,
  # set `n` to the number of resamples we'd need
  # if we somehow got the smallest group every time.
  # If sampling without replacement, just reshuffle all the groups.
  n <- nrow(freq_table)
  if (replace) n <- n * prop * sum(freq_table$count) / min(freq_table$count)
  n <- ceiling(n)

  purrr::map(
    seq_len(v),
    function(x) {
      row_idx <- sample.int(nrow(freq_table), n, replace = replace)
      work_table <- freq_table[row_idx, ]
      cumulative_proportion <- cumsum(work_table$count) / sum(freq_table$count)
      crosses_target <- which(cumulative_proportion > prop)[[1]]
      is_closest <- cumulative_proportion[c(crosses_target, crosses_target - 1)]
      is_closest <- which.min(abs(is_closest - prop)) - 1
      crosses_target <- crosses_target - is_closest
      out <- work_table[seq_len(crosses_target), ]
      out$assignment <- x
      out
    }
  ) %>%
    list_rbind()
}

check_prop <- function(prop, replace) {
  acceptable_prop <- is.numeric(prop)
  acceptable_prop <- acceptable_prop &&
    ((prop <= 1 && replace) || (prop < 1 && !replace))
  acceptable_prop <- acceptable_prop && prop > 0
  if (!acceptable_prop) {
    cli_abort(
      "{.arg prop} must be a number between 0 and 1.",
      call = rlang::caller_env()
    )
  }
}


collapse_groups <- function(freq_table, data_ind, v) {
  data_ind <- dplyr::left_join(
    data_ind,
    freq_table,
    by = c("..group" = "key"),
    multiple = "all",
    relationship = "many-to-many"
  )
  data_ind$..group <- data_ind$assignment
  data_ind <- data_ind[c("..index", "..group")]

  # If a group was never assigned a fold, then its `..group` is NA
  #
  # If we leave that alone, it winds up messing up our fold assignments,
  # because it will be assigned some value in `seq_len(v)`
  #
  # So instead, we drop those groups here:
  data_ind <- stats::na.omit(data_ind)

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

validate_group <- function(group, data, call = rlang::caller_env()) {
  if (!missing(group)) {
    group <- tidyselect::vars_select(names(data), !!enquo(group))
    if (length(group) == 0) {
      group <- NULL
    }
  }

  check_string(group, call = call)

  group
}
