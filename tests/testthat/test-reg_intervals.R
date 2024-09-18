test_that("regression intervals", {
  skip_if_not_installed("broom")
  skip_on_cran()

  expect_error(
    {
      set.seed(1)
      int_1 <- reg_intervals(mpg ~ disp + wt, data = mtcars)
    },
    regexp = NA
  )

  expect_equal(
    names(int_1),
    c("term", ".lower", ".estimate", ".upper", ".alpha", ".method")
  )

  expect_snapshot({
    skip_if(new_rng_snapshots)
    set.seed(123)
    int_2 <- reg_intervals(
      mpg ~ disp + wt,
      data = mtcars,
      filter = term == "wt",
      model_fn = "glm",
      keep_reps = TRUE
    )
    int_2
  })

  expect_equal(
    names(int_2),
    c("term", ".lower", ".estimate", ".upper", ".alpha", ".method", ".replicates")
  )
  expect_true(nrow(int_2) == 1)
  expect_true(all(int_2$term == "wt"))


  expect_snapshot(error = TRUE, {
    reg_intervals(mpg ~ disp + wt, data = mtcars, model_fn = "potato")
  })
  expect_snapshot(error = TRUE, {
    reg_intervals(mpg ~ disp + wt, data = mtcars, type = "random")
  })
  expect_snapshot(error = TRUE, {
    reg_intervals(mpg ~ disp + wt, data = mtcars, alpha = "a")
  })
})
