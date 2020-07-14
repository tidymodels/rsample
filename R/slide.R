# TODO: Ad-hoc `step` for `sliding_index()`?
# TODO: Add "safe" attributes and vctrs glue
# TODO: Minimum lookback `min_lookback` using `min_before`

sliding_window <- function(data,
                           ...,
                           lookback = 0L,
                           assess_start = 1L,
                           assess_stop = 1L,
                           step = 1L,
                           complete = FALSE) {
  ellipsis::check_dots_empty()

  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame.")
  }

  lookback <- check_lookback(lookback)
  assess_start <- check_assess(assess_start, "assess_start")
  assess_stop <- check_assess(assess_stop, "assess_stop")

  if (assess_start > assess_stop) {
    rlang::abort("`assess_start` must be less than or equal to `assess_stop`.")
  }

  seq <- vctrs::vec_seq_along(data)

  id_in <- slider::slide(
    .x = seq,
    .f = identity,
    .before = lookback,
    .after = 0L,
    .step = step,
    .complete = complete
  )

  id_out <- slider::slide(
    .x = seq,
    .f = identity,
    .before = -assess_start,
    .after = assess_stop,
    .step = 1L,
    .complete = TRUE
  )

  indices <- compute_complete_indices(id_in, id_out)

  splits <- purrr::map(
    indices,
    ~ make_splits(.x, data = data, class = "sliding_window_split")
  )

  n_indices <- length(indices)
  ids <- names0(n_indices, prefix = "Slice")

  attrib <- list(
    lookback = lookback,
    assess_start = assess_start,
    assess_stop = assess_stop,
    step = step,
    complete = complete
  )

  new_rset(
    splits = splits,
    ids = ids,
    attrib = attrib,
    subclass = c("sliding_index", "rset")
  )
}

# ------------------------------------------------------------------------------

sliding_index <- function(data,
                          index,
                          ...,
                          lookback = 0L,
                          assess_start = 1L,
                          assess_stop = 1L,
                          complete = FALSE) {
  ellipsis::check_dots_empty()

  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame.")
  }

  index <- rlang::enexpr(index)
  loc <- tidyselect::eval_select(index, data)

  if (length(loc) != 1L) {
    rlang::abort("`index` must specify exactly one column in `data`.")
  }

  index <- data[[loc]]

  seq <- vctrs::vec_seq_along(data)

  id_in <- slider::slide_index(
    .x = seq,
    .i = index,
    .f = identity,
    .before = lookback,
    .after = 0L,
    .complete = complete
  )

  # TODO: Should `complete` be `TRUE`? Maybe use `min_after`?
  # If it is `FALSE`, we end up with assessment values at the end with
  # no indices
  id_out <- slider::slide_index(
    .x = seq,
    .i = index,
    .f = identity,
    .before = -assess_start,
    .after = assess_stop,
    .complete = TRUE
  )

  indices <- compute_complete_indices(id_in, id_out)

  splits <- purrr::map(
    indices,
    ~ make_splits(.x, data = data, class = "sliding_index_split")
  )

  n_indices <- length(indices)
  ids <- names0(n_indices, prefix = "Slice")

  attrib <- list(
    lookback = lookback,
    assess_start = assess_start,
    assess_stop = assess_stop,
    complete = complete
  )

  new_rset(
    splits = splits,
    ids = ids,
    attrib = attrib,
    subclass = c("sliding_index", "rset")
  )
}

# ------------------------------------------------------------------------------

sliding_period <- function(data,
                           index,
                           period,
                           ...,
                           lookback = 0L,
                           assess_start = 1L,
                           assess_stop = 1L,
                           every = 1L,
                           origin = NULL,
                           complete = FALSE) {
  ellipsis::check_dots_empty()

  if (!is.data.frame(data)) {
    rlang::abort("`data` must be a data frame.")
  }

  lookback <- check_lookback(lookback)
  assess_start <- check_assess(assess_start, "assess_start")
  assess_stop <- check_assess(assess_stop, "assess_stop")

  if (assess_start > assess_stop) {
    rlang::abort("`assess_start` must be less than or equal to `assess_stop`.")
  }

  index <- rlang::enexpr(index)
  loc <- tidyselect::eval_select(index, data)

  if (length(loc) != 1L) {
    rlang::abort("`index` must specify exactly one column in `data`.")
  }

  index <- data[[loc]]

  seq <- vctrs::vec_seq_along(data)

  id_in <- slider::slide_period(
    .x = seq,
    .i = index,
    .period = period,
    .f = identity,
    .every = every,
    .origin = origin,
    .before = lookback,
    .after = 0L,
    .complete = complete
  )

  id_out <- slider::slide_period(
    .x = seq,
    .i = index,
    .period = period,
    .f = identity,
    .every = every,
    .origin = origin,
    .before = -assess_start,
    .after = assess_stop,
    .complete = TRUE
  )

  indices <- compute_complete_indices(id_in, id_out)

  splits <- purrr::map(
    indices,
    ~ make_splits(.x, data = data, class = "sliding_period_split")
  )

  n_indices <- length(indices)
  ids <- names0(n_indices, prefix = "Slice")

  attrib <- list(
    period = period,
    every = every,
    origin = origin,
    lookback = lookback,
    assess_start = assess_start,
    assess_stop = assess_stop,
    complete = complete
  )

  new_rset(
    splits = splits,
    ids = ids,
    attrib = attrib,
    subclass = c("sliding_period", "rset")
  )
}

# ------------------------------------------------------------------------------

check_lookback <- function(x) {
  if (vctrs::vec_size(x) != 1L) {
    rlang::abort(paste0("`lookback` must have size 1."))
  }

  if (identical(x, Inf)) {
    return(x)
  }

  if (!rlang::is_integerish(x, finite = TRUE)) {
    rlang::abort(paste0("`lookback` must be an integer of size 1, or `Inf`."))
  }

  if (x < 0L) {
    rlang::abort(paste0("`lookback` must be positive, or zero."))
  }

  vctrs::vec_cast(x, integer(), x_arg = "lookback")
}

check_assess <- function(x, arg) {
  if (vctrs::vec_size(x) != 1L) {
    rlang::abort(paste0("`", arg, "` must have size 1."))
  }

  if (identical(x, Inf)) {
    return(x)
  }

  if (!rlang::is_integerish(x, finite = TRUE)) {
    rlang::abort(paste0("`", arg, "` must be an integer of size 1, or `Inf`."))
  }

  if (x <= 0L) {
    rlang::abort(paste0("`", arg, "` must be positive."))
  }

  vctrs::vec_cast(x, integer(), x_arg = arg)
}

compute_complete_indices <- function(id_in, id_out) {
  # Remove where either list has a `NULL` element.
  # These are incomplete windows or skipped slices.
  id_in_na <- vctrs::vec_equal_na(id_in)
  id_out_na <- vctrs::vec_equal_na(id_out)

  id_either_na <- id_in_na | id_out_na

  id_in <- vctrs::vec_slice(id_in, !id_either_na)
  id_out <- vctrs::vec_slice(id_out, !id_either_na)

  purrr::map2(id_in, id_out, merge_lists)
}
