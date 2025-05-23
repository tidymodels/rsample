test_that("simple numerics", {
  set.seed(13333)
  x1 <- rnorm(1000)
  str1a <- make_strata(x1)
  tab1a <- table(str1a)
  expect_equal(as.vector(tab1a), rep(250, 4))

  expect_snapshot(str1b <- make_strata(x1, depth = 500))
  tab1b <- table(str1b)
  expect_equal(as.vector(tab1b), rep(500, 2))

  str1c <- make_strata(c(NA, x1[1:999]))
  tab1c <- table(str1c)
  expect_true(all(as.vector(tab1c) %in% 249:251))
})

test_that("simple character", {
  x2 <- factor(rep(LETTERS[1:12], each = 20))
  expect_snapshot({
    str2a <- make_strata(x2, pool = 0.05)
  })
  expect_equal(table(str2a, dnn = ""), table(x2, dnn = ""))

  x2[5] <- NA
  expect_snapshot({
    str2b <- make_strata(x2, pool = 0.05)
  })
  expect_true(all(as.vector(table(str2b, dnn = "")) %in% 19:21))
})

test_that("bad data", {
  x3 <- factor(rep(LETTERS[1:15], each = 50))
  expect_snapshot(s0 <- make_strata(x3))
  expect_snapshot(s1 <- make_strata(x3, pool = 0.06))
  expect_snapshot(s2 <- make_strata(mtcars$mpg))
  expect_snapshot(s3 <- make_strata(seq_len(50), breaks = -1))
})


# check_strata() ----------------------------------------------------------

test_that("don't stratify on Surv objects", {
  df <- data.frame(
    time = c(85, 79, 70, 6, 32, 8, 17, 93, 81, 76),
    event = c(0, 0, 1, 0, 0, 0, 1, 1, 1, 1)
  )
  df$surv <- structure(
    c(
      85,
      79,
      70,
      6,
      32,
      8,
      17,
      93,
      81,
      76,
      0,
      0,
      1,
      0,
      0,
      0,
      1,
      1,
      1,
      1
    ),
    .Dim = c(10L, 2L),
    .Dimnames = list(NULL, c("time", "status")),
    type = "right",
    class = "Surv"
  )

  expect_snapshot(error = TRUE, {
    check_strata("surv", df)
  })
})
