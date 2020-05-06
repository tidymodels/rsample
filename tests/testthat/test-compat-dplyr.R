# ------------------------------------------------------------------------------
# dplyr_reconstruct()

test_that("reconstructing to an rset returns a bare tibble", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(dplyr_reconstruct(x, x))
    expect_identical(dplyr_reconstruct(x, x), rset_strip(x))
  }
})

test_that("reconstructing retains extra attributes", {
  for (x in rset_subclasses) {
    x2 <- x
    attr(x2, "foo") <- "bar"

    result <- dplyr_reconstruct(x, x2)

    expect_identical(attr(result, "foo"), "bar")
  }
})

# ------------------------------------------------------------------------------
# dplyr_col_modify()

test_that("can add columns and retain rset class", {
  for (x in rset_subclasses) {
    cols <- list(x = rep(1, vec_size(x)))

    result <- dplyr_col_modify(x, cols)

    expect_s3_class_rset(result)
    expect_identical(result$x, cols$x)
  }
})

test_that("modifying rset columns removes rset class", {
  for (x in rset_subclasses) {
    cols <- list(splits = rep(1, vec_size(x)))

    result <- dplyr_col_modify(x, cols)

    expect_s3_class_bare_tibble(result)
    expect_identical(result$splits, cols$splits)
  }

  for (x in rset_subclasses) {
    cols <- list(id = rep(1, vec_size(x)))

    result <- dplyr_col_modify(x, cols)

    expect_s3_class_bare_tibble(result)
    expect_identical(result$id, cols$id)
  }
})

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
