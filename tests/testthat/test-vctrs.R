# ------------------------------------------------------------------------------
# vctrs ptype2

test_that("can compute the common type of an rset + rset of the same subclass", {
  x <- bootstraps(mtcars, 2)

  expect_s3_class(vec_ptype2(x, x), "bootstraps")
  expect_equal_data_frames(vec_ptype2(x, x), vctrs::vec_ptype(x))
})

test_that("union of columns is considered in the common type", {
  x <- bootstraps(mtcars, 2)

  y <- x
  y[["col"]] <- 1

  expect_s3_class(vec_ptype2(x, y), "bootstraps")
  expect_equal(vec_ptype2(x, y)$col, numeric())
})

test_that("common type of 2 rsets with different attributes is an error", {
  x <- bootstraps(mtcars, 2)
  y <- bootstraps(mtcars, 2, apparent = TRUE)

  expect_error(vec_ptype2(x, y), "must have the same rset attributes")
})

test_that("common type of 2 rsets with different subclasses is an error", {
  x <- bootstraps(mtcars, 2)
  y <- mc_cv(mtcars)

  expect_error(vec_ptype2(x, y), class = "vctrs_error_incompatible_type")
})

test_that("can compute the common type of an rset + data.frame", {
  x <- bootstraps(mtcars, 2)
  y <- data.frame(x = 1, y = "a", stringsAsFactors = FALSE)

  ptype <- vctrs::vec_ptype(x)
  ptype[["x"]] <- numeric()
  ptype[["y"]] <- character()

  expect_s3_class(vec_ptype2(x, y), "bootstraps")
  expect_equal_data_frames(vec_ptype2(x, y), ptype)

  expect_s3_class(vec_ptype2(y, x), "bootstraps")
  expect_equal_data_frames(vec_ptype2(y, x), ptype[c("x", "y", "splits", "id")])
})

test_that("can compute the common type of an rset + tbl_df", {
  x <- bootstraps(mtcars, 2)
  y <- tibble::tibble(x = 1, y = "a")

  ptype <- vctrs::vec_ptype(x)
  ptype[["x"]] <- numeric()
  ptype[["y"]] <- character()

  expect_s3_class(vec_ptype2(x, y), "bootstraps")
  expect_equal_data_frames(vec_ptype2(x, y), ptype)

  expect_s3_class(vec_ptype2(y, x), "bootstraps")
  expect_equal_data_frames(vec_ptype2(y, x), ptype[c("x", "y", "splits", "id")])
})

test_that("default ptype2 case is handled with rsets", {
  expect_error(vec_ptype2(bootstraps(mtcars, 2), 1), class = "vctrs_error_incompatible_type")
})

# ------------------------------------------------------------------------------
# vctrs casting

test_that("can cast from rset to rset of the same subclass", {
  x <- bootstraps(mtcars, 2)

  expect_s3_class(vec_cast(x, x), "bootstraps")
  expect_equal_data_frames(vec_cast(x, x), x)
})

test_that("union of columns is considered in the cast", {
  x <- bootstraps(mtcars, 2)

  y <- x
  y[["col"]] <- 1

  expect_s3_class(vec_cast(x, y), "bootstraps")
  expect_equal(vec_cast(x, y)$col, rep(NA_real_, 2))
})

test_that("cannot cast between 2 rsets with different attributes", {
  x <- bootstraps(mtcars, 2)
  y <- bootstraps(mtcars, 2, apparent = TRUE)

  expect_error(vec_cast(x, y), "must have the same rset attributes")
})

test_that("cannot cast between 2 rsets with different subclasses", {
  x <- bootstraps(mtcars, 2)
  y <- mc_cv(mtcars)

  expect_error(vec_cast(x, y), class = "vctrs_error_incompatible_cast")
})

test_that("can cast from rset to data frame", {
  x <- bootstraps(mtcars, 2)

  y <- as.data.frame(x)
  y[["col"]] <- 1

  expect <- as.data.frame(x)
  expect[["col"]] <- NA_real_

  expect_identical(class(vec_cast(x, y)), "data.frame")
  expect_equal_data_frames(vec_cast(x, y), expect)
})

test_that("can cast from rset to tbl_df", {
  x <- bootstraps(mtcars, 2)

  y <- as_tibble(x)
  y[["col"]] <- 1

  expect <- as_tibble(x)
  expect[["col"]] <- NA_real_

  expect_identical(class(vec_cast(x, y)), c("tbl_df", "tbl", "data.frame"))
  expect_equal_data_frames(vec_cast(x, y), expect)
})

test_that("can cast from data frame to rset", {
  x <- bootstraps(mtcars, 2)

  expect_s3_class(vec_cast(data.frame(), x), "bootstraps")
  expect_equal_data_frames(vec_cast(data.frame(), x), vctrs::vec_ptype(x))
})

test_that("can cast from tbl_df to rset", {
  x <- bootstraps(mtcars, 2)

  expect_s3_class(vec_cast(tibble(), x), "bootstraps")
  expect_equal_data_frames(vec_cast(tibble(), x), vctrs::vec_ptype(x))
})

# ------------------------------------------------------------------------------
# vctrs + tidyr

test_that("can unnest() and retain rset class in the simplest case possible", {
  x <- bootstraps(mtcars, 2)
  x[["col"]] <- 1:2

  expect_equal_data_frames(tidyr::unnest(x, col), x)
})

test_that("can unnest() and retain rset class when something is actually unnested (tidyr/#688)", {
  x <- bootstraps(mtcars, 2)
  x[["col"]] <- list(1:2, 1:3)

  expect_s3_class(tidyr::unnest(x, col), c("bootstraps"))
})

test_that("can nest() and remove rset class when splits/id is removed", {
  x <- bootstraps(mtcars, 3)

  expect_identical(
    class(tidyr::nest(x, data = id)),
    c("tbl_df", "tbl", "data.frame")
  )

  x[["col"]] <- c(1, 1, 2)

  expect_identical(
    class(tidyr::nest(x, data = -col)),
    c("tbl_df", "tbl", "data.frame")
  )
})

test_that("can nest() and retain rset class when splits/id columns are kept", {
  x <- bootstraps(mtcars, 3)
  x[["col"]] <- 1

  expect_s3_class(
    tidyr::nest(x, data = col),
    "bootstraps"
  )
})
