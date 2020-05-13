# ------------------------------------------------------------------------------
# `[]`

test_that("subsetting with nothing returns rset", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(x[])
  }
})

# ------------------------------------------------------------------------------
# `[i]`

test_that("can subset with just `i` and return rset", {
  for (x in rset_subclasses) {
    loc <- seq_len(ncol(x))
    expect_s3_class_rset(x[loc])
  }
})

test_that("removing any rset specific columns falls back", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(x[1])
  }
})

test_that("duplicating an rset column falls back", {
  for (x in rset_subclasses) {
    loc <- c(1, seq_len(ncol(x)))
    expect_s3_class_bare_tibble(x[loc])
  }
})

test_that("can reorder columns and keep rset class", {
  for (x in rset_subclasses) {
    loc <- rev(seq_len(ncol(x)))
    expect_s3_class_rset(x[loc])
  }
})

# ------------------------------------------------------------------------------
# `[i,]`

test_that("can row subset and drop to a tibble", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(x[0,])
  }
})

test_that("can row subset and keep rset class", {
  for (x in rset_subclasses) {
    loc <- seq_len(nrow(x))
    expect_s3_class_rset(x[loc,])
  }
})

# ------------------------------------------------------------------------------
# `[,j]`

# Most of these tests should be the same as `[i]`.

test_that("can subset with just `j` and keep rset class", {
  for (x in rset_subclasses) {
    loc <- seq_len(ncol(x))
    expect_s3_class_rset(x[,loc])
  }
})

test_that("removing an rset specific class drops the rset class", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(x[,1])
  }
})

# ------------------------------------------------------------------------------
# `[i, j]`

test_that("row subsetting mixed with col subsetting can drop to tibble", {
  for (x in rset_subclasses) {
    loc <- seq_len(ncol(x))
    expect_s3_class_bare_tibble(x[0, loc])
  }
})

test_that("row subsetting mixed with col subsetting can keep rset subclass", {
  for (x in rset_subclasses) {
    row_loc <- seq_len(nrow(x))
    col_loc <- seq_len(ncol(x))
    expect_s3_class_rset(x[row_loc, col_loc])
  }
})

# ------------------------------------------------------------------------------
# Misc `[` tests

test_that("additional attributes are kept when subsetting and rset class is kept", {
  for (x in rset_subclasses) {
    attr(x, "foo") <- "bar"

    loc <- seq_len(ncol(x))
    result <- x[loc]

    expect_s3_class_rset(result)
    expect_identical(attr(result, "foo"), "bar")
  }
})

test_that("additional attributes are kept when subsetting and rset class is dropped", {
  for (x in rset_subclasses) {
    attr(x, "foo") <- "bar"

    result <- x[1]

    expect_s3_class_bare_tibble(result)
    expect_identical(attr(result, "foo"), "bar")
  }
})

# ------------------------------------------------------------------------------
# `names<-`

test_that("can modify non-rset names and keep the rset class", {
  for (x in rset_subclasses) {
    x <- mutate(x, a = 1)

    names <- names(x)
    names[names == "a"] <- "b"
    names(x) <- names

    expect_s3_class_rset(x)
  }
})

test_that("touching the `splits` name removes the class", {
  for (x in rset_subclasses) {
    names <- names(x)
    names[names == "splits"] <- "splits2"
    names(x) <- names

    expect_s3_class_bare_tibble(x)
  }
})

test_that("renaming an `id` column is not allowed", {
  for (x in rset_subclasses) {
    names <- names(x)
    names[names == "id"] <- "id9"
    names(x) <- names

    expect_s3_class_bare_tibble(x)
  }
})

test_that("renaming an `id` column to something that doesn't start with `id` drops the class", {
  for (x in rset_subclasses) {
    names <- names(x)
    names[names == "id"] <- "foo"
    names(x) <- names

    expect_s3_class_bare_tibble(x)
  }
})

test_that("`splits` and `id` columns can't just be switched", {
  for (x in rset_subclasses) {
    names <- names(x)
    new_names <- names
    new_names[names == "splits"] <- "id"
    new_names[names == "id"] <- "splits"
    names(x) <- new_names

    expect_s3_class_bare_tibble(x)
  }
})

test_that("`id` column can't just be moved", {
  for (x in rset_subclasses) {
    x <- mutate(x, a = 1)

    names <- names(x)
    new_names <- names
    new_names[names == "a"] <- "id"
    new_names[names == "id"] <- "a"
    names(x) <- new_names

    expect_s3_class_bare_tibble(x)
  }
})

test_that("`splits` column can't just be moved", {
  for (x in rset_subclasses) {
    x <- mutate(x, a = 1)

    names <- names(x)
    new_names <- names
    new_names[names == "a"] <- "splits"
    new_names[names == "splits"] <- "a"
    names(x) <- new_names

    expect_s3_class_bare_tibble(x)
  }
})

# ------------------------------------------------------------------------------
# rset_identical()

test_that("two rset subclasses can be considered identical", {
  for (x in rset_subclasses) {
    expect_true(rset_identical(x, x))
  }
})

test_that("order doesn't matter", {
  for (x in rset_subclasses) {
    y <- x[rev(names(x))]
    expect_true(rset_identical(x, y))
  }
})

test_that("no longer identical if `splits` is lost", {
  for (to in rset_subclasses) {
    locs <- col_equals_splits(names(to))
    x <- to[!locs]
    expect_false(rset_identical(x, to))
  }
})

test_that("no longer identical if any `id` columns are lost", {
  for (to in rset_subclasses) {
    locs <- col_starts_with_id(names(to))
    first_id <- which(locs)[[1]]
    x <- to[-first_id]
    expect_false(rset_identical(x, to))
  }
})

test_that("no longer identical if rows are lost", {
  # Apparent/Validation only have 1 row
  subclasses <- rset_subclasses
  subclasses$apparent <- NULL
  subclasses$validation_split <- NULL

  for (to in subclasses) {
    x <- to[1,]
    expect_false(rset_identical(x, to))
  }
})

test_that("still considered identical if rows are simply reordered", {
  for (to in rset_subclasses) {
    loc <- rev(seq_len(nrow(to)))
    x <- to[loc,]
    expect_true(rset_identical(x, to))
  }
  for (to in rset_subclasses) {
    loc <- sample(nrow(to))
    x <- to[loc,]
    expect_true(rset_identical(x, to))
  }
})

test_that("the `inner_resamples` column of `nested_cv` is handled specially", {
  to <- rset_subclasses$nested_cv
  x <- to[c("splits", "id")]

  expect_false(rset_identical(x, to))
})

