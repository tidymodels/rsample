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
    expect_s3_class_rset(x[c(1, 2)])
  }
})

test_that("removing any rset specific columns falls back", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(x[1])
  }
})

test_that("duplicating an rset column falls back", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(x[c(1, 1, 2)])
  }
})

test_that("can reorder columns and keep rset class", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(x[c(2, 1)])
  }
})

# ------------------------------------------------------------------------------
# `[i,]`

test_that("can row subset and drop to a tibble", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(x[1,])
  }
})

# ------------------------------------------------------------------------------
# `[,j]`

# Most of these tests should be the same as `[i]`.

test_that("can subset with just `j` and keep rset class", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(x[,c(1,2)])
  }
})

test_that("removing an rset specific class drops the rset class", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(x[,1])
  }
})

# ------------------------------------------------------------------------------
# `[i, j]`

test_that("row subsetting mixed with col subsetting drops to tibble", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(x[1, c(1, 2)])
  }
})

# ------------------------------------------------------------------------------
# Misc `[` tests

test_that("additional attributes are kept when subsetting and rset class is kept", {
  for (x in rset_subclasses) {
    attr(x, "foo") <- "bar"

    result <- x[c(1, 2)]

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

test_that("renaming an `id` column is allowed if it still starts with `id`", {
  for (x in rset_subclasses) {
    names <- names(x)
    names[names == "id"] <- "id50"
    names(x) <- names

    expect_s3_class_rset(x)
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
