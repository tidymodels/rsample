test_that("default param", {
  set.seed(11)
  rs1 <- initial_split(dat1)
  expect_equal(class(rs1), c("initial_split", "mc_split", "rsplit"))
  tr1 <- training(rs1)
  ts1 <- testing(rs1)
  expect_equal(nrow(tr1), nrow(dat1) * 3 / 4)
  expect_equal(nrow(ts1), nrow(dat1) / 4)
})

test_that("default time param", {
  rs1 <- initial_time_split(dat1)
  expect_equal(class(rs1), c("initial_split", "rsplit"))
  tr1 <- training(rs1)
  ts1 <- testing(rs1)
  expect_equal(nrow(tr1), floor(nrow(dat1) * 3 / 4))
  expect_equal(nrow(ts1), ceiling(nrow(dat1) / 4))
  expect_equal(tr1, dplyr::slice(dat1, 1:floor(nrow(dat1) * 3 / 4)))
})

test_that("default time param with lag", {
  rs1 <- initial_time_split(dat1, lag = 5)
  expect_equal(class(rs1), c("initial_split", "rsplit"))
  tr1 <- training(rs1)
  ts1 <- testing(rs1)
  expect_equal(nrow(tr1), floor(nrow(dat1) * 3 / 4))
  expect_equal(nrow(ts1), ceiling(nrow(dat1) / 4) + 5)
  expect_equal(tr1, dplyr::slice(dat1, 1:floor(nrow(dat1) * 3 / 4)))
  expect_equal(ts1, dat1[(floor(nrow(dat1) * 3 / 4) + 1 - 5):nrow(dat1), ])

  data(drinks, package = "modeldata")

  # Whole numbers only
  expect_snapshot(
    initial_time_split(drinks, lag = 12.5),
    error = TRUE
  )
  # Lag must be less than number of training observations
  expect_snapshot(
    initial_time_split(drinks, lag = 500),
    error = TRUE
  )
})

test_that("default group param", {
  rs1 <- group_initial_split(dat1, c)
  expect_equal(class(rs1), c("group_initial_split", "initial_split", "grouped_mc_split", "mc_split", "rsplit"))
  tr1 <- training(rs1)
  ts1 <- testing(rs1)
  expect_equal(nrow(tr1), nrow(dat1) * 3 / 4)
  expect_equal(nrow(ts1), nrow(dat1) / 4)
})

test_that("`prop` computes the proportion for analysis (#217)", {
  set.seed(11)

  props <- c(.1, .9)

  for (prop in props) {
    # Not stratified
    split <- initial_split(airquality, prop = prop)
    actual <- nrow(analysis(split))
    expect <- as.integer(floor(nrow(airquality) * prop))

    expect_identical(actual, expect)

    # Stratified
    split <- initial_split(airquality, prop = prop, strata = Month)
    actual <- nrow(analysis(split))
    expect <- as.integer(sum(floor(table(airquality$Month) * prop)))

    expect_identical(actual, expect)
  }
})

test_that("`prop` computes the proportion for group analysis", {
  rs1 <- group_initial_split(dat1, c, prop = 1 / 2)
  expect_equal(class(rs1), c("group_initial_split", "initial_split", "grouped_mc_split", "mc_split", "rsplit"))
  tr1 <- training(rs1)
  ts1 <- testing(rs1)
  expect_equal(nrow(tr1), nrow(dat1) * 1 / 2)
  expect_equal(nrow(ts1), nrow(dat1) / 2)
  expect_equal(nrow(tr1), nrow(ts1))
})

test_that("printing initial split objects", {
  expect_snapshot(initial_split(mtcars))
  expect_snapshot(initial_time_split(mtcars))
})
