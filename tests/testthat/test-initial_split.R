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
  expect_equal(class(rs1), c("initial_time_split", "initial_split", "rsplit"))
  tr1 <- training(rs1)
  ts1 <- testing(rs1)
  expect_equal(nrow(tr1), floor(nrow(dat1) * 3 / 4))
  expect_equal(nrow(ts1), ceiling(nrow(dat1) / 4))
  expect_equal(tr1, dplyr::slice(dat1, 1:floor(nrow(dat1) * 3 / 4)))
})

test_that("default time param with lag", {
  rs1 <- initial_time_split(dat1, lag = 5)
  expect_equal(class(rs1), c("initial_time_split", "initial_split", "rsplit"))
  tr1 <- training(rs1)
  ts1 <- testing(rs1)
  expect_equal(nrow(tr1), floor(nrow(dat1) * 3 / 4))
  expect_equal(nrow(ts1), ceiling(nrow(dat1) / 4) + 5)
  expect_equal(tr1, dplyr::slice(dat1, 1:floor(nrow(dat1) * 3 / 4)))
  expect_equal(ts1, dat1[(floor(nrow(dat1) * 3 / 4) + 1 - 5):nrow(dat1), ], ignore_attr = "row.names")
})

test_that("`initial_time_split()` error messages", {
  skip_if_not_installed("modeldata")
  data(drinks, package = "modeldata", envir = rlang::current_env())

  expect_snapshot(error = TRUE, {
    initial_time_split(drinks, prop = 2)
  })

  expect_snapshot(error = TRUE, {
    initial_time_split(drinks, lag = 12.5)
  })

  expect_snapshot(error = TRUE, {
    initial_time_split(drinks, lag = nrow(drinks) + 1)
  })
})

test_that("default group param", {
  rs1 <- group_initial_split(dat1, c)
  expect_equal(class(rs1), c("group_initial_split", "initial_split", "group_mc_split", "mc_split", "rsplit"))
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

test_that("grouping -- strata", {
  set.seed(11)

  n_common_class <- 70
  n_rare_class <- 30

  group_table <- tibble(
    group = 1:100,
    outcome = sample(c(rep(0, n_common_class), rep(1, n_rare_class)))
  )
  observation_table <- tibble(
    group = sample(1:100, 5e4, replace = TRUE),
    observation = 1:5e4
  )
  sample_data <- dplyr::full_join(
    group_table,
    observation_table,
    by = "group",
    multiple = "all"
  )
  rs4 <- group_initial_split(sample_data, group, strata = outcome)
  expect_equal(mean(as.data.frame(rs4)$outcome == 1), 0.3, tolerance = 1e-2)

  expect_identical(length(intersect(rs4$in_ind, rs4$out_id)), 0L)

})

test_that("`prop` computes the proportion for group analysis", {
  rs1 <- group_initial_split(dat1, c, prop = 1 / 2)
  expect_equal(class(rs1), c("group_initial_split", "initial_split", "group_mc_split", "mc_split", "rsplit"))
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
