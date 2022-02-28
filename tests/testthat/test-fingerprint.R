test_that("fingerprinting", {
  set.seed(1)
  rs_1 <- vfold_cv(mtcars)
  fp_1 <- .get_fingerprint(rs_1)

  set.seed(1)
  fp_2 <- .get_fingerprint(vfold_cv(mtcars))

  set.seed(1)
  fp_3 <- .get_fingerprint(vfold_cv(mtcars, repeats = 2))

  expect_true(class(fp_1) == "character")
  expect_true(class(fp_2) == "character")
  expect_true(class(fp_3) == "character")
  expect_equal(fp_1, fp_2)
  expect_false(fp_1 == fp_3)
})
