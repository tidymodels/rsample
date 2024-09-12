#' Create a Validation Split for Tuning
#'
#' `validation_set()` creates a the validation split for model tuning.
#'
#' @param split An object of class `initial_validation_split`, such as resulting
#' from [initial_validation_split()] or [group_initial_validation_split()].
#' @param x An `rsplit` object produced by `validation_set()`.
#' @inheritParams rlang::args_dots_empty
#'
#' @return An tibble with classes `validation_set`, `rset`, `tbl_df`, `tbl`, and
#'  `data.frame`. The results include a column for the data split object and a
#'  column called `id` that has a character string with the resample identifier.
#' @export
#'
#' @examples
#' set.seed(1353)
#' car_split <- initial_validation_split(mtcars)
#' car_set <- validation_set(car_split)
validation_set <- function(split, ...) {
  rlang::check_dots_empty()

  train_and_val <- dplyr::bind_rows(
    training(split),
    validation(split)
  )
  n_train <- length(split$train_id)

  val_split <- rsplit(
    data = train_and_val,
    in_id = seq_len(n_train),
    out_id = NA
  )

  split_type <- switch(
    class(split)[1],
    "group_initial_validation_split" = "group",
    "initial_validation_time_split" = "time",
    "initial_validation_split" = NULL
  )

  # "val_split" is same class as via the alternative `validation_split()`
  if (is.null(split_type)) {
    class(val_split) <- c("val_split", "rsplit")
  } else {
    class(val_split) <- c(paste0(split_type, "_val_split"), "val_split", "rsplit")
  }

  val_att <- attr(split, "val_att")
  val_att[["origin_3way"]] <- TRUE

  if (is.null(split_type)) {
    rset_classes <- c("validation_set", "rset")
  } else {
    rset_classes <- c(paste0(split_type, "_validation_set"), "validation_set", "rset")
  }

  new_rset(
    splits = list(val_split),
    ids = "validation",
    attrib = val_att,
    subclass = rset_classes
  )
}

# accessor methods for the rsplit -----------------------------------------

#' @rdname validation_set
#' @export
analysis.val_split <- function(x, ...) {
  as.data.frame(x, data = "analysis", ...)
}

#' @rdname validation_set
#' @export
assessment.val_split <- function(x, ...) {
  as.data.frame(x, data = "assessment", ...)
}

#' @rdname validation_set
#' @export
training.val_split <- function(x, ...) {
  analysis(x, ...)
}

#' @rdname validation_set
#' @export
validation.val_split <- function(x, ...) {
  assessment(x, ...)
}

#' @rdname validation_set
#' @export
testing.val_split <- function(x, ...) {
 cli_abort(c(
  "The testing data is not part of the validation set object.",
  "i" = "It is part of the result of the initial 3-way split, e.g., with {.fun initial_validation_split}."
))
}
