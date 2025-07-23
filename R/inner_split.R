#' Inner split of the analysis set for fitting a post-processor
#'
#' @param x An `rsplit` object.
#' @param split_args A list of arguments to be used for the inner split.
#' @param ... Not currently used.
#' @return An `rsplit` object.
#' @details
#' `rsplit` objects live most commonly inside of an `rset` object. The
#' `split_args` argument can be the output of [.get_split_args()] on that
#' corresponding `rset` object, even if some of the arguments used to create the
#' `rset` object are not needed for the inner split.
#' * For `mc_split` and `group_mc_split` objects, `inner_split()` will ignore
#' `split_args$times`.
#' * For `vfold_split` and `group_vfold_split` objects, it will ignore
#' `split_args$times` and `split_args$repeats`. `split_args$v` will be used to
#' set `split_args$prop` to `1 - 1/v` if `prop` is not already set and otherwise
#' ignored. The method for `group_vfold_split` will always use
#' `split_args$balance = NULL`.
#' * For `boot_split` and `group_boot_split` objects, it will ignore
#' `split_args$times`.
#' * For `val_split`, `group_val_split`, and `time_val_split` objects, it will
#' interpret a length-2 `split_args$prop` as a ratio between the training and
#' validation sets and split into inner analysis and inner assessment set in
#' the same ratio. If `split_args$prop` is a single value, it will be used as
#' the proportion of the inner analysis set.
#' * For `clustering_split` objects, it will ignore `split_args$repeats`.
#'
#' @keywords internal
#' @export
inner_split <- function(x, ...) {
  UseMethod("inner_split")
}

# mc ---------------------------------------------------------------------

#' @rdname inner_split
#' @export
inner_split.mc_split <- function(x, split_args, ...) {
  check_dots_empty()

  analysis_set <- analysis(x)

  split_args$times <- 1
  split_inner <- try(
    {
      split_inner <- rlang::inject(
        mc_splits(analysis_set, !!!split_args)
      )
      split_inner <- split_inner$splits[[1]]
    },
    silent = TRUE
  )

  if (inherits(split_inner, "try-error")) {
    cli::cli_warn(
      "Cannot create calibration split; creating an empty calibration set."
    )
    split_inner <- mock_internal_calibration_split(analysis_set)
  }

  class_inner <- "mc_split_inner"
  split_inner <- add_class(split_inner, class_inner)
  split_inner
}

#' @rdname inner_split
#' @export
inner_split.group_mc_split <- function(x, split_args, ...) {
  check_dots_empty()

  analysis_set <- analysis(x)

  split_args$times <- 1
  split_inner <- try(
    {
      split_inner <- rlang::inject(
        group_mc_splits(analysis_set, !!!split_args)
      )
      split_inner <- split_inner$splits[[1]]
    },
    silent = TRUE
  )

  if (inherits(split_inner, "try-error")) {
    cli::cli_warn(
      "Cannot create calibration split; creating an empty calibration set."
    )
    split_inner <- mock_internal_calibration_split(analysis_set)
  }

  class_inner <- "group_mc_split_inner"
  split_inner <- add_class(split_inner, class_inner)
  split_inner
}


# vfold ------------------------------------------------------------------

#' @rdname inner_split
#' @export
inner_split.vfold_split <- function(x, split_args, ...) {
  check_dots_empty()

  analysis_set <- analysis(x)

  # TODO should this be done outside of rsample,
  # in workflows or tune?
  if (is.null(split_args$prop)) {
    split_args$prop <- 1 - 1 / split_args$v
  }
  # use mc_splits for a random split
  split_args$times <- 1
  split_args$v <- NULL
  split_args$repeats <- NULL
  split_inner <- try(
    {
      split_inner <- rlang::inject(
        mc_splits(analysis_set, !!!split_args)
      )
      split_inner <- split_inner$splits[[1]]
    },
    silent = TRUE
  )

  if (inherits(split_inner, "try-error")) {
    cli::cli_warn(
      "Cannot create calibration split; creating an empty calibration set."
    )
    split_inner <- mock_internal_calibration_split(analysis_set)
  }

  class_inner <- "vfold_split_inner"
  class(split_inner) <- c(class_inner, class(x))
  split_inner
}

#' @rdname inner_split
#' @export
inner_split.group_vfold_split <- function(x, split_args, ...) {
  check_dots_empty()

  analysis_set <- analysis(x)

  # TODO should this be done outside of rsample,
  # in workflows or tune?
  if (is.null(split_args$prop)) {
    split_args$prop <- 1 - 1 / split_args$v
  }

  # use group_mc_splits for a random split
  split_args$times <- 1
  split_args$v <- NULL
  split_args$repeats <- NULL
  split_args$balance <- NULL
  split_inner <- try(
    {
      split_inner <- rlang::inject(
        group_mc_splits(analysis_set, !!!split_args)
      )
      split_inner <- split_inner$splits[[1]]
    },
    silent = TRUE
  )

  if (inherits(split_inner, "try-error")) {
    cli::cli_warn(
      "Cannot create calibration split; creating an empty calibration set."
    )
    split_inner <- mock_internal_calibration_split(analysis_set)
  }

  class_inner <- "group_vfold_split_inner"
  class(split_inner) <- c(class_inner, class(x))
  split_inner
}


# bootstrap --------------------------------------------------------------

#' @rdname inner_split
#' @export
inner_split.boot_split <- function(x, split_args, ...) {
  check_dots_empty()

  # use unique rows to prevent the same information from entering
  # both the inner analysis and inner assessment set
  id_outer_analysis <- unique(x$in_id)
  analysis_set <- x$data[id_outer_analysis, , drop = FALSE]

  split_args$times <- 1
  split_inner <- rlang::try_fetch(
    {
      split_inner <- rlang::inject(
        bootstraps(analysis_set, !!!split_args)
      )
      split_inner <- split_inner$splits[[1]]
    },
    warning = function(cnd) {
      if (grepl("assessment sets contained zero rows", conditionMessage(cnd))) {
        return("mock_needed")
      } else {
        rlang::zap()
      }
    },
    error = function(cnd) {
      return("mock_needed")
    }
  )

  if (identical(split_inner, "mock_needed")) {
    cli::cli_warn(
      "Cannot create calibration split; creating an empty calibration set."
    )
    # with a 0-row calibration set, we can't end up with rows in both
    # calibration and analysis, thus use the full analysis set with duplicate
    # rows here
    analysis_set <- analysis(x)
    split_inner <- mock_internal_calibration_split(analysis_set)
  }

  class_inner <- "boot_split_inner"
  class(split_inner) <- c(class_inner, class(x))
  split_inner
}

#' @rdname inner_split
#' @export
inner_split.group_boot_split <- function(x, split_args, ...) {
  check_dots_empty()

  # use unique rows to prevent the same information from entering
  # both the inner analysis and inner assessment set
  id_outer_analysis <- unique(x$in_id)
  analysis_set <- x$data[id_outer_analysis, , drop = FALSE]

  split_args$times <- 1
  split_inner <- rlang::try_fetch(
    {
      split_inner <- rlang::inject(
        group_bootstraps(analysis_set, !!!split_args)
      )
      split_inner <- split_inner$splits[[1]]
    },
    warning = function(cnd) {
      if (grepl("assessment sets contained zero rows", conditionMessage(cnd))) {
        return("mock_needed")
      } else {
        rlang::zap()
      }
    },
    error = function(cnd) {
      return("mock_needed")
    }
  )

  if (identical(split_inner, "mock_needed")) {
    cli::cli_warn(
      "Cannot create calibration split; creating an empty calibration set."
    )
    # with a 0-row calibration set, we can't end up with rows in both
    # calibration and analysis, thus use the full analysis set with duplicate
    # rows here
    analysis_set <- analysis(x)
    split_inner <- mock_internal_calibration_split(analysis_set)
  }

  class_inner <- "group_boot_split_inner"
  class(split_inner) <- c(class_inner, class(x))
  split_inner
}


# validation set ---------------------------------------------------------

#' @rdname inner_split
#' @export
inner_split.val_split <- function(x, split_args, ...) {
  check_dots_empty()

  analysis_set <- analysis(x)

  if (length(split_args$prop) == 2) {
    # keep ratio between training and validation as ratio between
    # inner analysis and inner assessment
    split_args$prop <- split_args$prop[[1]] / sum(split_args$prop)
  } else {
    split_args$prop <- split_args$prop[[1]]
  }
  split_args$times <- 1
  split_inner <- try(
    {
      split_inner <- rlang::inject(
        mc_splits(analysis_set, !!!split_args)
      )
      split_inner <- split_inner$splits[[1]]
    },
    silent = TRUE
  )

  if (inherits(split_inner, "try-error")) {
    cli::cli_warn(
      "Cannot create calibration split; creating an empty calibration set."
    )
    split_inner <- mock_internal_calibration_split(analysis_set)
  }

  class_inner <- "val_split_inner"
  class(split_inner) <- c(class_inner, class(x))
  split_inner
}

#' @rdname inner_split
#' @export
inner_split.group_val_split <- function(x, split_args, ...) {
  check_dots_empty()

  analysis_set <- analysis(x)

  if (length(split_args$prop) == 2) {
    # keep ratio between training and validation as ratio between
    # inner analysis and inner assessment
    split_args$prop <- split_args$prop[[1]] / sum(split_args$prop)
  } else {
    split_args$prop <- split_args$prop[[1]]
  }
  split_args$times <- 1
  split_inner <- try(
    {
      split_inner <- rlang::inject(
        group_mc_splits(analysis_set, !!!split_args)
      )
      split_inner <- split_inner$splits[[1]]
    },
    silent = TRUE
  )

  if (inherits(split_inner, "try-error")) {
    cli::cli_warn(
      "Cannot create calibration split; creating an empty calibration set."
    )
    split_inner <- mock_internal_calibration_split(analysis_set)
  }

  class_inner <- "group_val_split_inner"
  class(split_inner) <- c(class_inner, class(x))
  split_inner
}

#' @rdname inner_split
#' @export
inner_split.time_val_split <- function(x, split_args, ...) {
  check_dots_empty()

  analysis_set <- analysis(x)

  if (length(split_args$prop) == 2) {
    # keep ratio between training and validation as ratio between
    # inner analysis and inner assessment
    split_args$prop <- split_args$prop[[1]] / sum(split_args$prop)
  } else {
    split_args$prop <- split_args$prop[[1]]
  }
  split_inner <- try(
    {
      split_inner <- rlang::inject(
        initial_time_split(analysis_set, !!!split_args)
      )
      # no need to pick the first split, as `initial_time_split()` only returns one
    },
    silent = TRUE
  )

  if (inherits(split_inner, "try-error")) {
    cli::cli_warn(
      "Cannot create calibration split; creating an empty calibration set."
    )
    split_inner <- mock_internal_calibration_split(analysis_set)
  }

  class_inner <- "time_val_split_inner"
  class(split_inner) <- c(class_inner, class(x))
  split_inner
}


# clustering -------------------------------------------------------------

#' @rdname inner_split
#' @export
inner_split.clustering_split <- function(x, split_args, ...) {
  check_dots_empty()

  analysis_set <- analysis(x)

  # TODO: reduce the number of clusters by 1 in tune?
  split_args$repeats <- 1
  split_inner <- try(
    {
      split_inner <- rlang::inject(
        clustering_cv(analysis_set, !!!split_args)
      )
      split_inner <- split_inner$splits[[1]]
    },
    silent = TRUE
  )

  if (inherits(split_inner, "try-error")) {
    cli::cli_warn(
      "Cannot create calibration split; creating an empty calibration set."
    )
    split_inner <- mock_internal_calibration_split(analysis_set)
  }

  class_inner <- "clustering_split_inner"
  class(split_inner) <- c(class_inner, class(x))
  split_inner
}


# apparent ---------------------------------------------------------------

#' @rdname inner_split
#' @export
inner_split.apparent_split <- function(x, ...) {
  check_dots_empty()

  analysis_set <- analysis(x)

  split_inner <- apparent(analysis_set)
  split_inner <- split_inner$splits[[1]]

  class_inner <- "apparent_split_inner"
  class(split_inner) <- c(class_inner, class(x))
  split_inner
}


# slide ------------------------------------------------------------------

#' @rdname inner_split
#' @export
inner_split.sliding_window_split <- function(x, split_args, ...) {
  check_dots_empty()

  analysis_set <- analysis(x)

  if (nrow(analysis_set) < 2) {
    cli::cli_warn(
      "This set cannot be split into an analysis and a calibration set as there 
      is only one row; creating an empty calibration set."
    )
    split_inner <- mock_internal_calibration_split(analysis_set)
    class_inner <- "sliding_window_split_inner"
    split_inner <- add_class(split_inner, class_inner)
    return(split_inner)
  }

  split_args_inner <- translate_window_definition(
    split_args$lookback,
    split_args$assess_start,
    split_args$assess_stop
  )

  lookback <- split_args_inner$lookback
  assess_start <- split_args_inner$assess_start
  assess_stop <- split_args_inner$assess_stop

  seq <- vctrs::vec_seq_along(analysis_set)

  id_in <- slider::slide(
    .x = seq,
    .f = identity,
    .before = lookback,
    .after = 0L,
    .step = 1L,
    .complete = split_args$complete
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

  if (length(indices) < 1) {
    cli::cli_warn(
      "Cannot create calibration split; creating an empty calibration set."
    )
    split_inner <- mock_internal_calibration_split(analysis_set)
  } else {
    indices <- indices[[length(indices)]]
    split_inner <- make_splits(
      indices,
      data = analysis_set,
      class = "sliding_window_split"
    )
  }

  # no need to use skip and step args since they don't apply to _within_ an rsplit

  class_inner <- "sliding_window_split_inner"
  split_inner <- add_class(split_inner, class_inner)
  split_inner
}

#' @rdname inner_split
#' @export
inner_split.sliding_index_split <- function(x, split_args, ...) {
  check_dots_empty()

  analysis_set <- analysis(x)

  if (nrow(analysis_set) < 2) {
    cli::cli_warn(
      "This set cannot be split into an analysis and a calibration set as there 
      is only one row; creating an empty calibration set."
    )
    split_inner <- mock_internal_calibration_split(analysis_set)
    class_inner <- "sliding_index_split_inner"
    split_inner <- add_class(split_inner, class_inner)
    return(split_inner)
  }

  split_args_inner <- translate_window_definition(
    split_args$lookback,
    split_args$assess_start,
    split_args$assess_stop
  )

  lookback <- split_args_inner$lookback
  assess_start <- split_args_inner$assess_start
  assess_stop <- split_args_inner$assess_stop

  loc <- tidyselect::eval_select(split_args$index, analysis_set)
  index <- analysis_set[[loc]]

  seq <- vctrs::vec_seq_along(analysis_set)

  id_in <- slider::slide_index(
    .x = seq,
    .i = index,
    .f = identity,
    .before = lookback,
    .after = 0L,
    .complete = split_args$complete
  )

  id_out <- slider::slide_index(
    .x = seq,
    .i = index,
    .f = identity,
    .before = -assess_start,
    .after = assess_stop,
    .complete = TRUE
  )

  indices <- compute_complete_indices(id_in, id_out)

  if (length(indices) < 1) {
    cli::cli_warn(
      "Cannot create calibration split; creating an empty calibration set."
    )
    split_inner <- mock_internal_calibration_split(analysis_set)
  } else {
    indices <- indices[[length(indices)]]
    split_inner <- make_splits(
      indices,
      data = analysis_set,
      class = "sliding_index_split"
    )
  }

  # no need to use skip and step args since they don't apply to _within_ an rsplit

  class_inner <- "sliding_index_split_inner"
  split_inner <- add_class(split_inner, class_inner)
  split_inner
}

#' @rdname inner_split
#' @export
inner_split.sliding_period_split <- function(x, split_args, ...) {
  check_dots_empty()

  analysis_set <- analysis(x)

  if (nrow(analysis_set) < 2) {
    cli::cli_warn(
      "This set cannot be split into an analysis and a calibration set as there 
      is only one row; creating an empty calibration set."
    )
    split_inner <- mock_internal_calibration_split(analysis_set)
    class_inner <- "sliding_period_split_inner"
    split_inner <- add_class(split_inner, class_inner)
    return(split_inner)
  }
  if (split_args$lookback < 1) {
    cli::cli_warn(
      "Cannot create calibration split; creating an empty calibration set."
    )
    split_inner <- mock_internal_calibration_split(analysis_set)
    class_inner <- "sliding_window_split_inner"
    split_inner <- add_class(split_inner, class_inner)
    return(split_inner)
  }

  split_args_inner <- translate_window_definition(
    split_args$lookback,
    split_args$assess_start,
    split_args$assess_stop
  )

  lookback <- split_args_inner$lookback
  assess_start <- split_args_inner$assess_start
  assess_stop <- split_args_inner$assess_stop

  loc <- tidyselect::eval_select(split_args$index, analysis_set)
  index <- analysis_set[[loc]]

  seq <- vctrs::vec_seq_along(analysis_set)

  id_in <- slider::slide_period(
    .x = seq,
    .i = index,
    .period = split_args$period,
    .f = identity,
    .every = split_args$every,
    .origin = split_args$origin,
    .before = lookback,
    .after = 0L,
    .complete = split_args$complete
  )

  id_out <- slider::slide_period(
    .x = seq,
    .i = index,
    .period = split_args$period,
    .f = identity,
    .every = split_args$every,
    .origin = split_args$origin,
    .before = -assess_start,
    .after = assess_stop,
    .complete = TRUE
  )

  indices <- compute_complete_indices(id_in, id_out)

  if (length(indices) < 1) {
    cli::cli_warn(
      "Cannot create calibration split; creating an empty calibration set."
    )
    split_inner <- mock_internal_calibration_split(analysis_set)
  } else {
    indices <- indices[[length(indices)]]
    split_inner <- make_splits(
      indices,
      data = analysis_set,
      class = "sliding_period_split"
    )
  }

  # no need to use skip and step args since they don't apply to _within_ an rsplit

  class_inner <- "sliding_period_split_inner"
  split_inner <- add_class(split_inner, class_inner)
  split_inner
}

translate_window_definition <- function(lookback, assess_start, assess_stop) {
  length_window <- lookback + 1 + assess_stop
  length_analysis <- lookback + 1

  prop_analysis <- length_analysis / length_window
  prop_assess <- (assess_stop - assess_start + 1) /
    length_window

  length_inner_analysis <- ceiling(prop_analysis * length_analysis)
  length_calibration <- ceiling(prop_assess * length_analysis)
  if (length_inner_analysis + length_calibration > length_analysis) {
    if (length_calibration > 1) {
      length_calibration <- length_calibration - 1
    } else {
      length_inner_analysis <- length_inner_analysis - 1
    }
  }

  lookback <- length_inner_analysis - 1
  assess_stop <- length_analysis - length_inner_analysis
  assess_start <- assess_stop - length_calibration + 1

  lookback <- check_lookback(lookback)
  assess_start <- check_assess(assess_start, "assess_start")
  assess_stop <- check_assess(assess_stop, "assess_stop")
  if (assess_start > assess_stop) {
    cli_abort(
      "{.arg assess_start} must be less than or equal to {.arg assess_stop}."
    )
  }

  list(
    lookback = lookback,
    assess_start = assess_start,
    assess_stop = assess_stop
  )
}


# initial split ----------------------------------------------------------

#' @rdname inner_split
#' @export
inner_split.initial_time_split <- function(x, split_args, ...) {
  check_dots_empty()

  training_set <- training(x)

  split_inner <- try(
    rlang::inject(initial_time_split(training_set, !!!split_args)),
    silent = TRUE
  )

  if (inherits(split_inner, "try-error")) {
    cli::cli_warn(
      "Cannot create calibration split; creating an empty calibration set."
    )
    split_inner <- mock_internal_calibration_split(training_set)
  }

  class_inner <- "initial_time_split_inner"
  class(split_inner) <- c(class_inner, class(x))
  split_inner
}


# initial validation split -----------------------------------------------

#' @rdname inner_split
#' @export
inner_split.initial_validation_split <- function(x, split_args, ...) {
  check_dots_empty()

  training_set <- training(x)

  split_args$prop <- split_args$prop[1] / sum(split_args$prop)
  split_args$times <- 1
  split_inner <- try(
    {
      split_inner <- rlang::inject(
        mc_splits(training_set, !!!split_args)
      )
      split_inner <- split_inner$splits[[1]]
    },
    silent = TRUE
  )

  if (inherits(split_inner, "try-error")) {
    cli::cli_warn(
      "Cannot create calibration split; creating an empty calibration set."
    )
    split_inner <- mock_internal_calibration_split(training_set)
  }

  class_inner <- "initial_validation_split_inner"
  class(split_inner) <- c(class_inner, class(split_inner))
  split_inner
}

#' @rdname inner_split
#' @export
inner_split.group_initial_validation_split <- function(x, split_args, ...) {
  check_dots_empty()

  training_set <- training(x)

  split_args$prop <- split_args$prop[1] / sum(split_args$prop)
  split_args$times <- 1
  split_inner <- try(
    {
      split_inner <- rlang::inject(
        group_mc_splits(training_set, !!!split_args)
      )
      split_inner <- split_inner$splits[[1]]
    },
    silent = TRUE
  )

  if (inherits(split_inner, "try-error")) {
    cli::cli_warn(
      "Cannot create calibration split; creating an empty calibration set."
    )
    split_inner <- mock_internal_calibration_split(training_set)
  }

  class_inner <- "group_initial_validation_split_inner"
  class(split_inner) <- c(class_inner, class(split_inner))
  split_inner
}

#' @rdname inner_split
#' @export
inner_split.initial_validation_time_split <- function(x, split_args, ...) {
  check_dots_empty()

  training_set <- training(x)

  if (nrow(training_set) < 2) {
    cli::cli_warn(
      "This set cannot be split into a training and a calibration set as there 
      is only one row; creating an empty calibration set."
    )
    split_inner <- mock_internal_calibration_split(training_set)
    class_inner <- "initial_validation_time_split_inner"
    class(split_inner) <- c(class_inner, class(split_inner))
    return(split_inner)
  }

  prop_analysis <- split_args$prop[1] / sum(split_args$prop)
  n_analysis <- floor(nrow(training_set) * prop_analysis)

  analysis_id <- seq(1, n_analysis, by = 1)
  cal_id <- seq(n_analysis + 1, nrow(training_set), by = 1)

  split_inner <- make_splits(
    list(analysis = as.integer(analysis_id), assessment = as.integer(cal_id)),
    data = training_set
  )

  class_inner <- "initial_validation_time_split_inner"
  class(split_inner) <- c(class_inner, class(split_inner))
  split_inner
}


# mock split -------------------------------------------------------------

mock_internal_calibration_split <- function(analysis_set) {
  calibration_set <- analysis_set[0, , drop = FALSE]
  mock_split <- make_splits(analysis_set, calibration_set)
  mock_split
}
