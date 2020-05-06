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
