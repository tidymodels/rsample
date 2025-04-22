test_that("simple rsplit", {
  rs1 <- rsplit(dat1, 1:2, 4:5)
  expect_equal(rs1$data, dat1)
  expect_equal(rs1$in_id, 1:2)
  expect_equal(rs1$out_id, 4:5)
})

test_that("simple rsplit with matrices", {
  dat2 <- as.matrix(dat1)
  rs2 <- rsplit(dat2, 1:2, 4:5)
  expect_equal(rs2$data, dat2)
  expect_equal(rs2$in_id, 1:2)
  expect_equal(rs2$out_id, 4:5)
})

test_that("bad inputs", {
  expect_snapshot(error = TRUE, {
    rsplit(as.list(dat1), 1:2, 4:5)
  })
  expect_snapshot(error = TRUE, {
    rsplit(dat1, letters[1:2], 4:5)
  })
  expect_snapshot(error = TRUE, {
    rsplit(as.list(dat1), 1:2, letters[4:5])
  })
  expect_snapshot(error = TRUE, {
    rsplit(as.list(dat1), -1:2, 4:5)
  })
  expect_snapshot(error = TRUE, {
    rsplit(as.list(dat1), 1:2, -4:5)
  })
  expect_snapshot(error = TRUE, {
    rsplit(as.list(dat1), integer(0), 4:5)
  })
})

test_that("as.data.frame", {
  rs3 <- rsplit(dat1, 1:2, 4:5)
  expect_equal(as.data.frame(rs3), dat1[1:2, ])
  expect_equal(
    as.data.frame(rs3, data = "assessment"),
    dat1[4:5, ],
    ignore_attr = "row.names"
  )

  rs4 <- rsplit(dat1, rep(1:2, each = 3), rep(4:5, c(2, 1)))
  expect_equal(
    as.data.frame(rs4),
    dat1[c(1, 1, 1, 2, 2, 2), ],
    ignore_attr = "row.names"
  )
  expect_equal(
    as.data.frame(rs4, data = "assessment"),
    dat1[c(4, 4, 5), ],
    ignore_attr = "row.names"
  )
})


test_that("print methods", {
  expect_snapshot({
    set.seed(233)
    vfold_cv(mtcars)$splits[[1]]
  })

  skip_if_not_installed("withr")
  withr::local_options(lifecycle_verbosity = "quiet")

  expect_snapshot({
    set.seed(233)
    validation_split(mtcars)$splits[[1]]
  })
  expect_snapshot({
    set.seed(233)
    validation_split(mtcars)
  })
})

test_that("`complement()` error messages", {
  fake_rsplit <- 1
  class(fake_rsplit) <- c("not_an_rsplit")
  expect_snapshot(error = TRUE, {
    complement(fake_rsplit)
  })
  class(fake_rsplit) <- c("not_an_rsplit", "really_not_an_rsplit")
  expect_snapshot(error = TRUE, {
    complement(fake_rsplit)
  })
  expect_snapshot(error = TRUE, {
    get_stored_out_id(list(out_id = NA))
  })
})

test_that("as.data.frame() works for permutations with Surv object without the survival package loaded - issue #443", {
  srv <-
    list(
      age = c(74, 68, 56, 57, 60, 74, 76, 77, 39, 75, 66, 58),
      sex = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2),
      surv_obj = structure(
        c(
          306,
          455,
          1010,
          210,
          883,
          1022,
          116,
          188,
          191,
          105,
          174,
          177,
          1,
          1,
          0,
          1,
          1,
          0,
          1,
          0,
          0,
          0,
          0,
          0
        ),
        dim = c(12L, 2L),
        dimnames = list(NULL, c("time", "status")),
        type = "right",
        class = "Surv"
      )
    )
  surv_df <-
    structure(
      srv,
      row.names = paste(1:12),
      class = "data.frame"
    )

  set.seed(472)
  surv_permutation_df <- permutations(
    surv_df,
    permute = c(age, surv_obj),
    times = 1
  ) %>%
    get_rsplit(1)

  expect_s3_class(surv_permutation_df$data$surv_obj, "Surv")
  expect_s3_class(analysis(surv_permutation_df)$surv_obj, "Surv")
})
