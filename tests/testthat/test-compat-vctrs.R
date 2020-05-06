# ------------------------------------------------------------------------------
# vec_restore()

test_that("vec_restore() returns a bare tibble, not a rset subclass", {
  for (x in rset_subclasses) {
    expect_identical(vec_restore(x, x), rset_strip(x))
    expect_s3_class_bare_tibble(vec_restore(x, x))
  }
})

test_that("vec_restore() retains extra attributes", {
  for (x in rset_subclasses) {
    y <- x
    attr(y, "foo") <- "bar"

    # retain extra attributes of `to`
    expect_identical(attr(vec_restore(y, x), "foo"), NULL)
    expect_identical(attr(vec_restore(x, y), "foo"), "bar")

    expect_s3_class_bare_tibble(vec_restore(x, y))
  }
})

# ------------------------------------------------------------------------------
# vec_ptype2()

test_that("vec_ptype2() is working", {
  for (x in rset_subclasses) {
    tbl <- tibble::tibble(x = 1)
    df <- data.frame(x = 1)

    # rset-rset
    expect_identical(vec_ptype2(x, x), vec_ptype2(rset_strip(x), rset_strip(x)))

    # rset-tbl_df
    expect_identical(vec_ptype2(x, tbl), vec_ptype2(rset_strip(x), tbl))
    expect_identical(vec_ptype2(tbl, x), vec_ptype2(tbl, rset_strip(x)))

    # rset-df
    expect_identical(vec_ptype2(x, df), vec_ptype2(rset_strip(x), df))
    expect_identical(vec_ptype2(df, x), vec_ptype2(df, rset_strip(x)))
  }
})

# ------------------------------------------------------------------------------
# vec_cast()

test_that("vec_cast() is working", {
  for (x in rset_subclasses) {
    tbl <- rset_strip(x)
    df <- as.data.frame(tbl)

    # rset-rset
    expect_error(vec_cast(x, x), class = "vctrs_error_incompatible_type")

    # rset-tbl_df
    expect_identical(vec_cast(x, tbl), tbl)
    expect_error(vec_cast(tbl, x), class = "vctrs_error_incompatible_type")

    # rset-df
    expect_identical(vec_cast(x, df), df)
    expect_error(vec_cast(df, x), class = "vctrs_error_incompatible_type")
  }
})

# ------------------------------------------------------------------------------
# vctrs methods

test_that("vec_slice() returns a bare tibble", {
  for (x in rset_subclasses) {
    expect_identical(vec_slice(x, 1), vec_slice(rset_strip(x), 1))
    expect_s3_class_bare_tibble(vec_slice(x, 1))
  }
})

test_that("vec_c() returns a bare tibble", {
  for (x in rset_subclasses) {
    tbl <- rset_strip(x)

    expect_identical(vec_c(x), vec_c(tbl))
    expect_identical(vec_c(x, x), vec_c(tbl, tbl))
    expect_identical(vec_c(x, tbl), vec_c(tbl, tbl))

    expect_s3_class_bare_tibble(vec_c(x))
    expect_s3_class_bare_tibble(vec_c(x, x))
  }
})

test_that("vec_rbind() returns a bare tibble", {
  for (x in rset_subclasses) {
    tbl <- rset_strip(x)

    expect_identical(vec_rbind(x), vec_rbind(tbl))
    expect_identical(vec_rbind(x, x), vec_rbind(tbl, tbl))
    expect_identical(vec_rbind(x, tbl), vec_rbind(tbl, tbl))

    expect_s3_class_bare_tibble(vec_rbind(x))
    expect_s3_class_bare_tibble(vec_cbind(x, x))
  }
})

test_that("vec_cbind() returns a bare tibble", {
  for (x in rset_subclasses) {
    tbl <- rset_strip(x)

    expect_identical(vec_cbind(x), vec_cbind(tbl))
    expect_identical(vec_cbind(x, x), vec_cbind(tbl, tbl))
    expect_identical(vec_cbind(x, tbl), vec_cbind(tbl, tbl))

    expect_s3_class_bare_tibble(vec_cbind(x))
    expect_s3_class_bare_tibble(vec_cbind(x, x))
  }
})
