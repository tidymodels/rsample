test_that("get_rsplit()", {
  skip_if_not_installed("withr")
  # for `validation_split()` and variants
  withr::local_options(lifecycle_verbosity = "quiet")

  val <- withr::with_seed(
    11,
    validation_split(warpbreaks)
  )

  expect_identical(val$splits[[1]], get_rsplit(val, 1))

  expect_snapshot(error = TRUE,{
    get_rsplit(val, 3)
  })

  expect_snapshot(error = TRUE,{
    get_rsplit(val, c(1, 2))
  })

  expect_snapshot(error = TRUE,{
    get_rsplit(val, 1.5)
  })

  expect_snapshot(error = TRUE,{
    get_rsplit(warpbreaks, 1)
  })

})
