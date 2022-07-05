skip_if_not(rlang::is_installed("withr"))
# ------------------------------------------------------------------------------
# vec_restore()

test_that("vec_restore() returns an rset subclass if `x` retains rset structure", {
  for (x in rset_subclasses) {
    expect_identical(vec_restore(x, x), x)
    expect_s3_class_rset(vec_restore(x, x))
  }
})

test_that("vec_restore() returns bare tibble if `x` loses rset structure", {
  for (x in rset_subclasses) {
    col <- x[1]
    row <- x[0, ]

    expect_s3_class_bare_tibble(vec_restore(col, x))
    expect_s3_class_bare_tibble(vec_restore(row, x))
  }
})

test_that("vec_restore() retains extra attributes of `to` when not falling back", {
  for (x in rset_subclasses) {
    to <- x
    attr(to, "foo") <- "bar"

    x_tbl <- x[1]

    expect_identical(attr(vec_restore(x, to), "foo"), "bar")
    expect_identical(attr(vec_restore(x_tbl, to), "foo"), NULL)

    expect_s3_class_rset(vec_restore(x, to))
    expect_s3_class_bare_tibble(vec_restore(x_tbl, to))
  }
})

# ------------------------------------------------------------------------------
# vec_ptype2()

test_that("vec_ptype2() is working", {
  for (x in rset_subclasses) {
    tbl <- tibble::tibble(x = 1)
    df <- data.frame(x = 1)

    # rset-rset
    expect_identical(vec_ptype2(x, x), vec_ptype2(tib_upcast(x), tib_upcast(x)))

    # rset-tbl_df
    expect_identical(vec_ptype2(x, tbl), vec_ptype2(tib_upcast(x), tbl))
    expect_identical(vec_ptype2(tbl, x), vec_ptype2(tbl, tib_upcast(x)))

    # rset-df
    expect_identical(vec_ptype2(x, df), vec_ptype2(tib_upcast(x), df))
    expect_identical(vec_ptype2(df, x), vec_ptype2(df, tib_upcast(x)))
  }
})

# ------------------------------------------------------------------------------
# vec_cast()

test_that("vec_cast() is working", {
  for (x in rset_subclasses) {
    tbl <- tib_upcast(x)
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

test_that("vec_ptype() returns a bare tibble", {
  for (x in rset_subclasses) {
    expect_identical(vec_ptype(x), vec_ptype(tib_upcast(x)))
    expect_s3_class_bare_tibble(vec_ptype(x))
  }
})

test_that("vec_slice() generally returns a bare tibble", {
  for (x in rset_subclasses) {
    expect_identical(vec_slice(x, 0), vec_slice(tib_upcast(x), 0))
    expect_s3_class_bare_tibble(vec_slice(x, 0))
  }
})

test_that("vec_slice() can return an rset if all rows are selected", {
  for (x in rset_subclasses) {
    expect_identical(vec_slice(x, TRUE), x)
    expect_s3_class_rset(vec_slice(x, TRUE))
  }
})

test_that("vec_c() returns a bare tibble", {
  for (x in rset_subclasses) {
    tbl <- tib_upcast(x)

    expect_identical(vec_c(x), vec_c(tbl))
    expect_identical(vec_c(x, x), vec_c(tbl, tbl))
    expect_identical(vec_c(x, tbl), vec_c(tbl, tbl))

    expect_s3_class_bare_tibble(vec_c(x))
    expect_s3_class_bare_tibble(vec_c(x, x))
  }
})

test_that("vec_rbind() returns a bare tibble", {
  for (x in rset_subclasses) {
    tbl <- tib_upcast(x)

    expect_identical(vec_rbind(x), vec_rbind(tbl))
    expect_identical(vec_rbind(x, x), vec_rbind(tbl, tbl))
    expect_identical(vec_rbind(x, tbl), vec_rbind(tbl, tbl))

    expect_s3_class_bare_tibble(vec_rbind(x))
    expect_snapshot(expect_s3_class_bare_tibble(vec_cbind(x, x)))
  }
})

test_that("vec_cbind() returns a bare tibble", {
  for (x in rset_subclasses) {
    tbl <- tib_upcast(x)

    expect_identical(vec_cbind(x), vec_cbind(tbl))
    expect_snapshot(expect_identical(vec_cbind(x, x), vec_cbind(tbl, tbl)))
    expect_snapshot(expect_identical(vec_cbind(x, tbl), vec_cbind(tbl, tbl)))

    expect_s3_class_bare_tibble(vec_cbind(x))
    expect_snapshot(expect_s3_class_bare_tibble(vec_cbind(x, x)))
  }
})
