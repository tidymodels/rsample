library(testthat)
library(rsample)
library(purrr)

dat1 <- data.frame(a = 1:20, b = letters[1:20])

test_that('default param', {
  rs1 <- rolling_origin(dat1)
  sizes1 <- rsample:::dim_rset(rs1)
  
  expect_true(all(sizes1$assessment == 1))
  expect_true(all(sizes1$analysis == 5:19))
  same_data <-
    map_lgl(rs1$splits, function(x)
      all.equal(x$data, dat1))
  expect_true(all(same_data))
  
  for (i in 1:nrow(rs1)) {
    expect_equal(rs1$splits[[i]]$in_id,
                 1:(i + attr(rs1, "initial") - 1))
    expect_equal(rs1$splits[[i]]$out_id,
                 i + attr(rs1, "initial"))
  }
  
})

test_that('larger holdout', {
  rs2 <- rolling_origin(dat1, assess = 3)
  sizes2 <- rsample:::dim_rset(rs2)
  
  expect_true(all(sizes2$assessment == 3))
  expect_true(all(sizes2$analysis == 5:17))

  for (i in 1:nrow(rs2)) {
    expect_equal(rs2$splits[[i]]$in_id,
                 1:(i + attr(rs2, "initial") - 1))
    expect_equal(rs2$splits[[i]]$out_id,
                 (i + attr(rs2, "initial")):
                   (i + attr(rs2, "initial") + attr(rs2, "assess") - 1))
  }
  
})

test_that('fixed analysis size', {
  rs3 <- rolling_origin(dat1, cumulative = FALSE)
  sizes3 <- rsample:::dim_rset(rs3)
  
  expect_true(all(sizes3$assessment == 1))
  expect_true(all(sizes3$analysis == 5))

  for (i in 1:nrow(rs3)) {
    expect_equal(rs3$splits[[i]]$in_id,
                 i:(i + attr(rs3, "initial") - 1))
    expect_equal(rs3$splits[[i]]$out_id,
                 i + attr(rs3, "initial"))
  }
  
})


test_that('skipping', {
  rs4 <- rolling_origin(dat1, cumulative = FALSE, skip = 2)
  sizes4 <- rsample:::dim_rset(rs4)
  
  expect_true(all(sizes4$assessment == 1))
  expect_true(all(sizes4$analysis == 5))

  for (i in 1:nrow(rs4)) {
    expect_equal(rs4$splits[[i]]$in_id,
                 (i + attr(rs4, "skip")*(i-1)):
                   (i + attr(rs4, "skip")*(i-1) + attr(rs4, "initial") -1))
    expect_equal(rs4$splits[[i]]$out_id,
                 i + attr(rs4, "skip")*(i-1) + attr(rs4, "initial"))
  }
  
})

