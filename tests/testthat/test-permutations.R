test_that("default param", {
  set.seed(11)
  rs1 <- permutations(mtcars, 1)
  sizes1 <- dim_rset(rs1)

  expect_true(all(sizes1$analysis == nrow(mtcars)))
  same_data <-
    purrr::map_lgl(rs1$splits, function(x) {
      all.equal(x$data, mtcars)
    })
  expect_true(all(same_data))

  good_holdout <- purrr::map_lgl(
    rs1$splits,
    function(x) {
      length(intersect(x$in_ind, x$out_id)) == 0
    }
  )
  expect_true(all(good_holdout))
})

test_that("apparent", {
  rs2 <- permutations(mtcars, 1, apparent = TRUE)
  sizes2 <- dim_rset(rs2)

  expect_true(all(sizes2$analysis == nrow(mtcars)))
  expect_true(all(sizes2$assessment[nrow(sizes2)] == nrow(mtcars)))
  expect_equal(sizes2$assessment[sizes2$id == "Apparent"], nrow(mtcars))
})

test_that("no assessment set", {
  xx <- permutations(mtcars, 1)
  expect_snapshot(assessment(xx$splits[[1]]), error = TRUE)
})

test_that("bad args", {
  expect_error(permutations(mtcars)) # no columns specified
  expect_error(permutations(mtcars, foo)) # column doesn't exist
  expect_error(permutations(mtcars, start_with("z"))) # column doesn't exist
  expect_snapshot(error = TRUE, {permutations(mtcars, everything())}) # all columns
})

test_that("printing", {
  expect_snapshot(permutations(mtcars, 1))
})

test_that("rsplit labels", {
  rs <- permutations(mtcars, 1)
  all_labs <- purrr::map(rs$splits, labels) %>%
    list_rbind()
  original_id <- rs[, grepl("^id", names(rs))]
  expect_equal(all_labs, original_id)
})

test_that("filtering/slicing rows", {
  x <- permutations(mtcars, 1:3)
  xf <- dplyr::filter(x, id == "Permutations01")
  xs <- dplyr::slice(x, 1)
  expect_identical(class(xf), c("tbl_df", "tbl", "data.frame"))
  expect_identical(class(xs), c("tbl_df", "tbl", "data.frame"))
})

test_that("column binding", {
  x <- permutations(mtcars, 1:3)
  xcb1 <- bind_cols(x, y = LETTERS[1:nrow(x)])
  xcb2 <- bind_cols(x, mtcars = tidyr::nest(mtcars, data = everything()))
  xcb3 <- bind_cols(y = LETTERS[1:nrow(x)], x)
  expect_identical(class(xcb1), class(x))
  expect_identical(class(xcb2), class(x))
  expect_false(identical(class(xcb3), class(x)))
})
