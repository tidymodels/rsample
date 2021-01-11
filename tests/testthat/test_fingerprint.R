
test_that("fingerprinting", {
  set.seed(1)
  rs_1 <- vfold_cv(mtcars)
  fp_1 <- fingerprint(rs_1)

  set.seed(1)
  fp_2 <- fingerprint(vfold_cv(mtcars))

  set.seed(1)
  fp_3 <- fingerprint(vfold_cv(mtcars, repeats = 2))

  expect_true(class(fp_1) == "character")
  expect_true(class(fp_2) == "character")
  expect_true(class(fp_3) == "character")
  expect_equal(fp_1, fp_2)
  expect_false(fp_1 == fp_3)

  expect_error(
    fingerprint(vfold_cv(mtcars) %>% dplyr::select(-id)),
    "No ID columns were found"
  )
  expect_error(
    fingerprint(vfold_cv(mtcars) %>% dplyr::select(-splits)),
    "The 'split' column was not found"
  )

  # test cases where the rows of the rset are expaned (e.g. in tune_bayes())
  set.seed(1)
  rs_2 <- vfold_cv(mtcars)
  rs_3 <- rs_2[rep(1:10, 3), ]
  fp_4 <- fingerprint(rs_3)
  expect_equal(fp_1, fp_4)

})
