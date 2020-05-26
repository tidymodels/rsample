
rset_reconstruct <- function(data, template) {
  if (rset_reconstructable(data, template)) {
    df_reconstruct(data, template)
  } else {
    tib_upcast(data)
  }
}

# ------------------------------------------------------------------------------

# Two data frames are considered identical by `rset_reconstructable()` if the rset
# sub-data-frames are identical. This means that if we select out the rset
# specific columns, they should be exactly the same (modulo reordering).

# It is expected that `y` is an rset object already, but `x` can be a
# bare data frame, or even a named list.

rset_reconstructable <- function(x, y) {
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

  # Avoid all non-bare-data-frame S3 dispatch and
  # don't compare outer data frame attributes.
  # Only look at column names and actual column data.
  x <- new_data_frame(x)
  y <- new_data_frame(y)

  # Early return if number of rows doesn't match
  if (!identical(vec_size(x), vec_size(y))) {
    return(FALSE)
  }

  x_rset_cols <- x[x_rset_names]
  y_rset_cols <- y[y_rset_names]

  # Row order doesn't matter
  x_rset_cols <- vec_sort(x_rset_cols)
  y_rset_cols <- vec_sort(y_rset_cols)

  # Check identical structures
  identical(x_rset_cols, y_rset_cols)
}

# ------------------------------------------------------------------------------

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
  apparent         = character(),
  tbl_df           = character()
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

# ------------------------------------------------------------------------------

col_equals_splits <- function(x) {
  vec_equal(x, "splits")
}

col_starts_with_id <- function(x) {
  grepl("(^id$)|(^id[1-9]$)", x)
}

col_equals_inner_resamples <- function(x) {
  vec_equal(x, "inner_resamples")
}

# ------------------------------------------------------------------------------

# Maybe this should live in vctrs?
# Fallback to a tibble from the current data frame subclass.
# Removes subclass specific attributes and additional ones added by the user.
tib_upcast <- function(x) {
  size <- df_size(x)

  # Strip all attributes except names to construct
  # a bare list to build the tibble back up from.
  attributes(x) <- list(names = names(x))

  tibble::new_tibble(x, nrow = size)
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

# Maybe this should live in vctrs?
df_reconstruct <- function(x, to) {
  attrs <- attributes(to)
  attrs$names <- names(x)
  attrs$row.names <- .row_names_info(x, type = 0L)
  attributes(x) <- attrs
  x
}
