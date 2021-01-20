context("Rsplit constructor")

library(testthat)
library(rsample)

dat1 <- data.frame(a = 1:100, b = 101:200)
size1 <- object.size(dat1)

dat2 <- as.matrix(dat1)

test_that('simple rsplit', {
  rs1 <- rsplit(dat1, 1:2, 4:5)
  expect_equal(rs1$data, dat1)
  expect_equal(rs1$in_id, 1:2)
  expect_equal(rs1$out_id, 4:5)
  expect_equal(complement_default(rs1), rs1$out_id)
})

test_that('simple rsplit with matrices', {
  rs2 <- rsplit(dat2, 1:2, 4:5)
  expect_equal(rs2$data, dat2)
  expect_equal(rs2$in_id, 1:2)
  expect_equal(rs2$out_id, 4:5)
})

test_that('bad inputs', {
  expect_error(rsplit(as.list(dat1), 1:2, 4:5))
  expect_error(rsplit(dat1, letters[1:2], 4:5))
  expect_error(rsplit(as.list(dat1), 1:2, letters[4:5]))
  expect_error(rsplit(as.list(dat1), -1:2, 4:5))
  expect_error(rsplit(as.list(dat1), 1:2, -4:5))
  expect_error(rsplit(as.list(dat1), integer(0), 4:5))
})

test_that('as.data.frame', {
  rs3 <- rsplit(dat1, 1:2, 4:5)
  expect_equal(as.data.frame(rs3), dat1[1:2,])
  expect_equal(as.data.frame(rs3, data = "assessment"), dat1[4:5,])

  rs4 <- rsplit(dat1, rep(1:2, each = 3), rep(4:5, c(2, 1)))
  expect_equal(as.data.frame(rs4), dat1[c(1, 1, 1, 2, 2, 2),])
  expect_equal(as.data.frame(rs4, data = "assessment"), dat1[c(4, 4, 5),])
})


test_that('print methods', {
  verify_output(test_path("print_test_output", "rsplit"), {
    set.seed(233)
    print(vfold_cv(mtcars)$splits[[1]])
  })
  verify_output(test_path("print_test_output", "val_split"), {
    set.seed(233)
    print(validation_split(mtcars)$splits[[1]])
  })
})






