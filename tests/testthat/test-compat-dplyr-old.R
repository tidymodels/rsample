# These tests should pass on all supported versions of dplyr. Both pre and
# post dplyr 1.0.0 should work.
# When `compat-dplyr-old.R` is removed and support for dplyr < 1.0.0 is
# deprecated, these tests should move to `test-compat-dplyr.R` instead.
# Do not just delete them, as they are important tests and are not repeated in
# `compat-dplyr.R`.

library(dplyr)

# ------------------------------------------------------------------------------
# mutate()

test_that("mutate() can keep rset class", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(mutate(x, x = 1))
    expect_identical(mutate(x, x = 1)$x, rep(1, vec_size(x)))
  }
})

test_that("mutate() drops rset class if any rset columns are touched", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(mutate(x, splits = 1))
    expect_s3_class_bare_tibble(mutate(x, id = 1))

    expect_identical(mutate(x, splits = 1)$splits, rep(1, vec_size(x)))
    expect_identical(mutate(x, id = 1)$id, rep(1, vec_size(x)))
  }
})

test_that("mutate() keeps rset class if replacement rset column is same as original", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(mutate(x, splits = splits))
    expect_s3_class_rset(mutate(x, id = id))
  }
})

test_that("adding a column that looks like an `id` drops the class", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(mutate(x, id9 = 1))
  }
})

# ------------------------------------------------------------------------------
# arrange()

test_that("arrange() drops rset class when row order is modified", {
  # These only have 1 row
  subclasses <- rset_subclasses
  subclasses$apparent <- NULL
  subclasses$validation_split <- NULL

  for (x in subclasses) {
    x <- mutate(x, rn = row_number())
    expect_s3_class_bare_tibble(arrange(x, desc(rn)))
  }
})

test_that("arrange() keeps rset class when row order is untouched", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(arrange(x))

    x <- mutate(x, rn = row_number())
    expect_s3_class_rset(arrange(x, rn))
  }
})

# ------------------------------------------------------------------------------
# filter()

test_that("filter() drops rset class when rows are modified", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(filter(x, 0 == 1))
    expect_s3_class_bare_tibble(filter(x, is.numeric(id)))
  }
})

test_that("filter() keeps rset class if row order is untouched", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(filter(x))
    expect_s3_class_rset(filter(x, is.character(id)))
  }
})

# ------------------------------------------------------------------------------
# rename()

test_that("renaming can keep the rset class", {
  for (x in rset_subclasses) {
    x <- mutate(x, a = 1)
    x <- rename(x, b = a)
    expect_s3_class_rset(x)
  }
})

test_that("renaming `id` at all drops the rset class", {
  for (x in rset_subclasses) {
    x <- rename(x, id9 = id)
    expect_s3_class_bare_tibble(x)
  }
})

test_that("renaming `id` to a non-id name drops the rset class", {
  for (x in rset_subclasses) {
    x <- rename(x, stuff = id)
    expect_s3_class_bare_tibble(x)
  }
})

test_that("for nested_cv, renaming `inner_resamples` drops the rset class", {
  x <- rset_subclasses$nested_cv
  x <- rename(x, inner_stuff = inner_resamples)
  expect_s3_class_bare_tibble(x)
})

# ------------------------------------------------------------------------------
# select()

test_that("select() can keep rset class", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(select(x, everything()))
  }
})

test_that("select() drops rset class if any rset columns aren't selected", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(select(x, id))
    expect_s3_class_bare_tibble(select(x, splits))
  }
})

# ------------------------------------------------------------------------------
# slice()

test_that("slice() drops rset class when rows are modified", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(slice(x, 0))
  }
})

test_that("slice() keeps rset class when rows are untouched", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(slice(x))
    expect_s3_class_rset(slice(x, seq_len(nrow(x))))
  }
})
