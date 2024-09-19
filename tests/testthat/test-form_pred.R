test_that("no dots", {
  expect_equal(form_pred(y ~ x + z), c("x", "z"))
  expect_equal(form_pred(terms(y ~ x + z)), c("x", "z"))

  expect_equal(form_pred(y ~ x + log(z)), c("x", "z"))
  expect_equal(form_pred(terms(y ~ x + log(z))), c("x", "z"))

  expect_equal(form_pred(log(y) ~ x + z), c("x", "z"))
  expect_equal(form_pred(terms(log(y) ~ x + z)), c("x", "z"))

  expect_equal(form_pred(y1 + y2 ~ x + z), c("x", "z"))
  expect_equal(form_pred(terms(y1 + y2 ~ x + z)), c("x", "z"))

  expect_equal(form_pred(log(y1) + y2 ~ x + z), c("x", "z"))
  expect_equal(form_pred(terms(log(y1) + y2 ~ x + z)), c("x", "z"))

  expect_equal(form_pred(~ x + z), c("x", "z"))
  expect_equal(form_pred(terms(~ x + z)), c("x", "z"))

  expect_equal(form_pred(~x), "x")
  expect_equal(form_pred(terms(~x)), "x")

  expect_equal(form_pred(y ~ x), "x")
  expect_equal(form_pred(terms(y ~ x)), "x")
})

test_that("dots", {
  expect_snapshot(error = TRUE, {
    form_pred(y ~ .)
  })
  expect_snapshot(error = TRUE, {
    form_pred(terms(y ~ .))
  })

  expect_snapshot(error = TRUE, {
    form_pred(y ~ (.)^2)
  })
  expect_snapshot(error = TRUE, {
    form_pred(terms(y ~ (.)^2))
  })

  expect_equal(
    form_pred(terms(mpg ~ (.)^2, data = mtcars)),
    names(mtcars)[2:11]
  )
  expect_equal(
    form_pred(terms(~ (.)^2, data = mtcars)),
    names(mtcars)
  )
})
