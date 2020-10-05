test_that("can create a split with an empty assessment set (#188)", {
  df <- data.frame(x = c(1, 2, 3, 4))
  indices <- list(analysis = 1:4, assessment = integer())

  split <- make_splits(indices, df)

  expect_identical(split$out_id, integer())
  expect_identical(assessment(split), df[0, , drop = FALSE])
})

test_that("cannot create a split with an empty analysis set", {
  df <- data.frame(x = c(1, 2, 3, 4))
  indices <- list(analysis = integer(), assessment = 1:4)

  expect_error(make_splits(indices, df), "At least one row")
})
