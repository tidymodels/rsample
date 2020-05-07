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

  splits$splits <- map2(splits$splits, split(ids, 1:nrow(ids)), add_id)

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

# Internally setting `i` or `j` to `NULL` is a way to signal that
# it wasn't provided. If `NULL` is provided by the user, this is
# replaced with `integer()`, which has the same typical meaning of
# subsetting nothing.

# The first chunk of code is focused on standardizing `i` and `j` based
# on the various ways that they can be provided, and is based on tibble's
# `[.tbl_df`. For example, it standardizes `x[i]` to `i = NULL, j = i` to
# more clearly indicate that this is a column subsetting operation.

#' @export
`[.rset` <- function(x, i, j, drop = FALSE, ...) {
  out <- NextMethod()

  if (missing(i)) {
    i <- NULL
  } else if (is.null(i)) {
    i <- integer()
  }

  if (missing(j)) {
    j <- NULL
  } else if (is.null(j)) {
    j <- integer()
  }

  n_real_args <- nargs() - !missing(drop)
  if (n_real_args <= 2L) {
    j <- i
    i <- NULL
  }

  # Row subset of some kind.
  # rset structure must be lost here.
  if (!is.null(i)) {
    out <- rset_strip(out)
    return(out)
  }

  # Both `i` and `j` are `NULL` (i.e. both are missing)
  # This happens with `x[]` and it means we select everything.
  if (is.null(j)) {
    return(out)
  }

  # Otherwise, only `j` is used, so we are column subsetting.
  # Check that `splits` and each `id` column still
  # exist in `out` exactly once.
  if (col_subset_requires_fallback(x, out)) {
    out <- rset_strip(out)
  }

  out
}

col_subset_requires_fallback <- function(old, new) {
  old_names <- names(old)
  new_names <- names(new)

  rset_names_indicator <-
    col_equals_splits(old_names) |
    col_starts_with_id(old_names)

  rset_names <- old_names[rset_names_indicator]

  # Each `rset_name` must be in `new_names` exactly once
  # otherwise we have to fall back.
  for (rset_name in rset_names) {
    times <- sum(vec_in(new_names, rset_name))

    if (times != 1L) {
      return(TRUE)
    }
  }

  FALSE
}

# ------------------------------------------------------------------------------

#' @export
`names<-.rset` <- function(x, value) {
  out <- NextMethod()

  old_names <- names(x)
  new_names <- names(out)

  old_rset_splits_indicator <- col_equals_splits(old_names)
  new_rset_splits_indicator <- col_equals_splits(new_names)

  # Ensure that the single `splits` column is in the same place
  if (!identical(old_rset_splits_indicator, new_rset_splits_indicator)) {
    out <- rset_strip(out)
    return(out)
  }

  old_rset_id_indicator <- col_starts_with_id(old_names)
  new_rset_id_indicator <- col_starts_with_id(new_names)

  # Ensure that we have renamed `id` columns in such a way that they are all
  # still in the same place. We cannot add or remove `id` columns, but we
  # can rename them.
  if (!identical(old_rset_id_indicator, new_rset_id_indicator)) {
    out <- rset_strip(out)
    return(out)
  }

  new_id_cols <- new_names[new_rset_id_indicator]

  # Ensure that the renamed `id` columns don't have any duplicates.
  if (any(duplicated(new_id_cols))) {
    out <- rset_strip(out)
    return(out)
  }

  out
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
