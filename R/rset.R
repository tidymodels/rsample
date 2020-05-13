#' Constructor for new rset objects
#' @param splits A list column of `rsplits` or a tibble with a single column
#'  called "splits" with a list column of `rsplits`.
#' @param ids A character vector or a tibble with one or more columns that
#' begin with "id".
#' @param attrib An optional named list of attributes to add to the object.
#' @param subclass A character vector of subclasses to add.
#' @return An `rset` object.
#' @keywords internal
#' @export
new_rset <-  function(splits, ids, attrib = NULL,
                      subclass = character()) {
  stopifnot(is.list(splits))
  if (!is_tibble(ids)) {
    ids <- tibble(id = ids)
  } else {
    if (!all(grepl("^id", names(ids)))) {
      rlang::abort("The `ids` tibble column names should start with 'id'.")
    }
  }
  either_type <- function(x)
    is.character(x) | is.factor(x)
  ch_check <- vapply(ids, either_type, c(logical = TRUE))
  if (!all(ch_check)) {
    rlang::abort("All ID columns should be character or factor vectors.")
  }

  if (!is_tibble(splits)) {
    splits <- tibble(splits = splits)
  } else {
    if (ncol(splits) > 1 | names(splits)[1] != "splits") {
      rlang::abort(
        "The `splits` tibble should have a single column named `splits`."
      )
    }
  }

  if (nrow(ids) != nrow(splits)) {
    rlang::abort("Split and ID vectors have different lengths.")
  }

  # Create another element to the splits that is a tibble containing
  # an identifier for each id column so that, in isolation, the resample
  # id can be known just based on the `rsplit` object. This can then be
  # accessed using the `labels` method for `rsplits`

  splits$splits <- map2(splits$splits, split_unnamed(ids, 1:nrow(ids)), add_id)

  res <- bind_cols(splits, ids)

  if (!is.null(attrib)) {
    if (any(names(attrib) == "")) {
      rlang::abort("`attrib` should be a fully named list.")
    }
    for (i in names(attrib)) {
      attr(res, i) <- attrib[[i]]
    }
  }

  if (length(subclass) > 0) {
    res <- add_class(res, cls = subclass, at_end = FALSE)
  }

  res
}

add_id <- function(split, id) {
  split$id <- id
  split
}

# ------------------------------------------------------------------------------

#' @export
`[.rset` <- function(x, i, j, drop = FALSE, ...) {
  out <- NextMethod()
  rset_maybe_reconstruct(out, x)
}

# ------------------------------------------------------------------------------

#' @export
`names<-.rset` <- function(x, value) {
  out <- NextMethod()
  rset_maybe_reconstruct(out, x)
}

# ------------------------------------------------------------------------------

rset_strip <- function(x) {
  attribs <- attributes(x)

  # Strip attributes specific to this rset subclass,
  # but keep any user-defined attributes.
  rset_attrib_names <- rset_attributes(x)
  attribs[rset_attrib_names] <- NULL

  # Let `new_tibble()` add the class
  attribs["class"] <- NULL

  # Don't try and add row names
  attribs["row.names"] <- NULL

  size <- df_size(x)

  # Strip all attributes except names to construct
  # a bare list to build the tibble back up from.
  attributes(x) <- list(names = attribs[["names"]])

  tibble::new_tibble(x, !!!attribs, nrow = size)
}

df_size <- function(x) {
  if (!is.list(x)) {
    rlang::abort("Cannot get the df size of a non-list.")
  }

  if (length(x) == 0L) {
    return(0L)
  }

  col <- x[[1L]]

  vec_size(col)
}

# ------------------------------------------------------------------------------

# Two data frames are considered identical by `rset_identical()` if the rset
# sub-data-frames are identical. This means that if we select out the rset
# specific columns, they should be exactly the same (modulo reordering).

# It is expected that `y` is an rset object already, but `x` can be a
# bare data frame, or even a named list.

rset_identical <- function(x, y) {
  x_names <- names(x)
  y_names <- names(y)

  x_rset_indicator <- col_equals_splits(x_names) | col_starts_with_id(x_names)
  y_rset_indicator <- col_equals_splits(y_names) | col_starts_with_id(y_names)

  # Special casing of `nested_cv` to also look for `inner_resamples`
  if (inherits(y, "nested_cv")) {
    x_rset_indicator <- x_rset_indicator | col_equals_inner_resamples(x_names)
    y_rset_indicator <- y_rset_indicator | col_equals_inner_resamples(y_names)
  }

  x_rset_names <- x_names[x_rset_indicator]
  y_rset_names <- y_names[y_rset_indicator]

  # Ignore ordering
  x_rset_names <- sort(x_rset_names)
  y_rset_names <- sort(y_rset_names)

  # Early return if names aren't identical
  if (!identical(x_rset_names, y_rset_names)) {
    return(FALSE)
  }

  # Avoid all S3 dispatch and don't look at outer level attributes,
  # just looking at underlying structure now
  attributes(x) <- list(names = x_names)
  attributes(y) <- list(names = y_names)

  x_rset_cols <- x[x_rset_names]
  y_rset_cols <- y[y_rset_names]

  # Check identical structures
  identical(x_rset_cols, y_rset_cols)
}

# ------------------------------------------------------------------------------

test_data <- function() {
  data.frame(x = 1:50, y = rep(c(1, 2), each = 25))
}

# Keep this list up to date with known rset subclasses for testing.
# Delay assignment because we are creating this directly in the R script
# and not all of the required helpers might have been sourced yet.
delayedAssign("rset_subclasses", {
  list(
    bootstraps       = bootstraps(test_data()),
    vfold_cv         = vfold_cv(test_data(), v = 10, repeats = 2),
    group_vfold_cv   = group_vfold_cv(test_data(), y),
    loo_cv           = loo_cv(test_data()),
    mc_cv            = mc_cv(test_data()),
    nested_cv        = nested_cv(test_data(), outside = vfold_cv(v = 3), inside = bootstraps(times = 5)),
    validation_split = validation_split(test_data()),
    rolling_origin   = rolling_origin(test_data()),
    apparent         = apparent(test_data())
  )
})

# Keep this dictionary up to date with any changes to the rset subclasses.
# These are the attributes that this specific subclass knows about.
rset_attribute_dictionary <- list(
  bootstraps       = c("times", "apparent", "strata"),
  vfold_cv         = c("v", "repeats", "strata"),
  group_vfold_cv   = c("v", "group"),
  loo_cv           = character(),
  mc_cv            = c("prop", "times", "strata"),
  nested_cv        = c("outside", "inside"),
  validation_split = c("prop", "strata"),
  rolling_origin   = c("initial", "assess", "cumulative", "skip", "lag"),
  apparent         = character()
)

rset_attributes <- function(x) {
  cls <- class(x)[[1]]

  attributes <- rset_attribute_dictionary[[cls]]

  if (is.null(attributes)) {
    rlang::abort("Unrecognized class in `rset_attributes()`.")
  }

  # Special case `nested_cv`, which appends a class onto an existing
  # rset subclass. We need to strip the `nested_cv` specific attributes
  # and the ones for the existing subclass.
  if (identical(cls, "nested_cv")) {
    class(x) <- class(x)[-1]
    extra_attributes <- rset_attributes(x)
    attributes <- c(attributes, extra_attributes)
  }

  attributes
}

# ------------------------------------------------------------------------------

# This is `dplyr_reconstruct.data.frame()`
rset_reconstruct <- function(data, template) {
  attrs <- attributes(template)
  attrs$names <- names(data)
  attrs$row.names <- .row_names_info(data, type = 0L)
  attributes(data) <- attrs
  data
}

rset_maybe_reconstruct <- function(data, template) {
  if (rset_identical(data, template)) {
    rset_reconstruct(data, template)
  } else {
    rset_strip(data)
  }
}

# ------------------------------------------------------------------------------

col_equals_splits <- function(x) {
  vec_equal(x, "splits")
}

col_starts_with_id <- function(x) {
  grepl("^id", x)
}

col_equals_inner_resamples <- function(x) {
  vec_equal(x, "inner_resamples")
}
