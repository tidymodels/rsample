test_that("simple boot", {
  set.seed(11)
  rs1 <- bootstraps(dat1)
  td1 <- tidy(rs1, unique_ind = FALSE)
  expect_gt(nrow(td1), nrow(dat1))

  name_vals <- names0(nrow(rs1), "Bootstrap")
  for (i in 1:nrow(rs1)) {
    expect_true(
      check_ind(
        rs1$splits[[i]],
        subset(td1, Resample == name_vals[i])
      )
    )
  }
})


test_that("vfold", {
  set.seed(11)
  rs2 <- vfold_cv(dat1)
  td2 <- tidy(rs2, unique_ind = FALSE)

  for (i in 1:nrow(rs2)) {
    expect_true(
      check_ind(
        rs2$splits[[i]],
        subset(td2, Fold == rs2$id[i])
      )
    )
  }
})

test_that("vfold with repeats", {
  set.seed(11)
  rs3 <- vfold_cv(dat1, repeats = 2)
  td3 <- tidy(rs3, unique_ind = FALSE)

  for (i in 1:nrow(rs3)) {
    expect_true(
      check_ind(
        rs3$splits[[i]],
        subset(td3, Fold == rs3$id2[i] & Repeat == rs3$id[i])
      )
    )
  }
})
