# ------------------------------------------------------------------------------
# dplyr_reconstruct()

test_that("reconstructing to an rset returns a bare tibble", {
  for (i in seq_along(rset_subclasses)) {
    x <- rset_subclasses[[i]]
    expect_s3_class_bare_tibble(dplyr_reconstruct(x, x))
    expect_identical(dplyr_reconstruct(x, x), rset_strip(x))
  }
})

test_that("reconstructing retains extra attributes", {
  for (i in seq_along(rset_subclasses)) {
    x1 <- rset_subclasses[[i]]
    x2 <- x1
    attr(x2, "foo") <- "bar"

    result <- dplyr_reconstruct(x1, x2)

    expect_identical(attr(result, "foo"), "bar")
  }
})

# ------------------------------------------------------------------------------
# dplyr_col_modify()

test_that("can add columns and retain rset class", {
  for (i in seq_along(rset_subclasses)) {
    x <- rset_subclasses[[i]]
    cols <- list(x = rep(1, vec_size(x)))

    result <- dplyr_col_modify(x, cols)

    expect_s3_class_rset(result)
    expect_identical(result$x, cols$x)
  }
})

test_that("modifying rset columns removes rset class", {
  for (i in seq_along(rset_subclasses)) {
    x <- rset_subclasses[[i]]
    cols <- list(splits = rep(1, vec_size(x)))

    result <- dplyr_col_modify(x, cols)

    expect_s3_class_bare_tibble(result)
    expect_identical(result$splits, cols$splits)
  }

  for (i in seq_along(rset_subclasses)) {
    x <- rset_subclasses[[i]]
    cols <- list(id = rep(1, vec_size(x)))

    result <- dplyr_col_modify(x, cols)

    expect_s3_class_bare_tibble(result)
    expect_identical(result$id, cols$id)
  }
})
