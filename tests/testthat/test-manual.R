test_that("can create a manual rset", {
  df <- data.frame(x = c(1, 2, 3, 4))

  indices <- list(
    list(analysis = 1L, assessment = 2L),
    list(analysis = 3L, assessment = 4L)
  )

  splits <- lapply(indices, make_splits, data = df)

  rset <- manual_rset(splits, c("Split 1", "Split 2"))

  expect_s3_class(rset, c("manual_rset", "rset"))
  expect_identical(rset$id, c("Split 1", "Split 2"))
})

test_that("can use analysis/assessment functions", {
  df <- data.frame(x = c(1, 2, 3))
  indices <- list(analysis = c(1L, 2L), assessment = 3L)
  splits <- list(make_splits(indices, df))

  rset <- manual_rset(splits, "Split 1")

  expect_identical(analysis(rset$splits[[1]]), df[1:2, 1, drop = FALSE])
  expect_identical(
    assessment(rset$splits[[1]]),
    df[3, 1, drop = FALSE],
    ignore_attr = "row.names"
  )
})

test_that("`pretty()` works", {
  df <- data.frame(x = c(1, 2, 3))
  indices <- list(analysis = c(1L, 2L), assessment = 3L)
  splits <- list(make_splits(indices, df))

  rset <- manual_rset(splits, "Split 1")

  expect_identical(pretty(rset), "Manual resampling")
})
