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
