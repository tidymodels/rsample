test_that("basic split", {
  dat1 <- data.frame(a = 1:20, b = letters[1:20], c = rep(1:4, 5))

  set.seed(11)
  rs1 <- initial_validation_split(dat1, prop = c(0.6, 0.2))

  expect_equal(class(rs1), c("initial_validation_split", "three_way_split"))

  size_train <- length(rs1$train_id)
  size_val <- length(rs1$val_id)

  expect_equal(size_train, 12)
  expect_equal(size_val, 4)
  expect_equal(rs1$test_id, NA)

  expect_equal(rs1$data, dat1)

  good_holdout <- length(intersect(rs1$train_id, rs1$val_id))
  expect_equal(good_holdout, 0)
})

test_that("basic split - accessor functions", {
  dat1 <- data.frame(a = 1:20, b = letters[1:20], c = rep(1:4, 5))

  set.seed(11)
  val_split <- initial_validation_split(dat1, prop = c(0.6, 0.2))

  dat_train <- training(val_split)
  dat_val <- validation(val_split)
  dat_test <- testing(val_split)

  expect_equal(nrow(dat_train), 12)
  expect_equal(nrow(dat_val), 4)
  expect_equal(nrow(dat_test), 4)

  expect_snapshot(error = TRUE, {
    analysis(val_split)
  })
  expect_snapshot(error = TRUE, {
    assessment(val_split)
  })
})


test_that("basic split stratified", {
  dat <- data.frame(
    id = 1:100,
    f = factor(rep(letters[1:3], times = c(50, 25, 25)))
  )

  val_split <- initial_validation_split(dat, strata = f)

  # distribution of strata should be similar in all 3 data sets
  expected <- c(50, 25, 25)/100
  actual_train <- dat[val_split$train_id, "f"] %>% table() %>% prop.table()
  expect_equal(as.vector(actual_train), expected)

  actual_val <- dat[val_split$val_id, "f"] %>% table() %>% prop.table()
  expect_equal(as.vector(actual_val), expected)

  actual_test <- dat[-c(val_split$train_id, val_split$val_id), "f"] %>%
    table() %>%
    prop.table()
  expect_equal(as.vector(actual_test), expected)

  # bad args for `strata`
  expect_snapshot(error = TRUE, {
    initial_validation_split(dat, strata = does_not_exist)
  })
  expect_snapshot(error = TRUE, {
    initial_validation_split(dat, strata = c(x, f))
  })
  expect_snapshot(error = TRUE, {
    initial_validation_split(dat, strata = rep(1:3, times = c(50, 25, 25)))
  })
})


test_that("grouped split", {

  # all observations of each group should be in only one of the 3 data sets
  # = all obs in the same group and no intersection in the groups
  #   from the 3 data sets

  dat <- data.frame(
    id = 1:100,
    g = rep(1:5, each = 20)
  )

  val_split <- group_initial_validation_split(dat, group = g, prop = c(0.6, 0.2))

  dat_train <- dat[val_split$train_id, ]
  dat_val <- dat[val_split$val_id, ]
  dat_test <- dat[-c(val_split$train_id,val_split$val_id), ]

  expect_equal(nrow(dat_train), 60)
  expect_equal(nrow(dat_val), 20)
  expect_equal(nrow(dat_test), 20)

  g_train <- dat_train %>% dplyr::count(g)
  g_val <- dat_val %>% dplyr::count(g)
  g_test <- dat_test %>% dplyr::count(g)

  # all obs of the chosen groups are here
  # which also means there are none elsewhere
  expect_true(all(g_train$n == 20))
  expect_true(all(g_val$n == 20))
  expect_true(all(g_test$n == 20))

  intersect_train_val <- intersect(g_train$id, g_val$id)
  intersect_train_test <- intersect(g_train$id, g_test$id)
  intersect_val_test <- intersect(g_val$id, g_test$id)
  expect_equal(length(intersect_train_val), 0)
  expect_equal(length(intersect_train_test), 0)
  expect_equal(length(intersect_val_test), 0)

})

test_that("grouped split stratified", {
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

  val_split <- group_initial_validation_split(
    sample_data,
    group = "group",
    prop = c(0.6, 0.2),
    strata = outcome
  )

  dat_train <- sample_data[val_split$train_id, ]
  dat_val <- sample_data[val_split$val_id, ]
  dat_test <- sample_data[-c(val_split$train_id,val_split$val_id), ]

  expect_equal(mean(dat_train$outcome == 1), 0.3, tolerance = 1e-2)
  expect_equal(mean(dat_val$outcome == 1), 0.3, tolerance = 1e-2)
  expect_equal(mean(dat_test$outcome == 1), 0.3, tolerance = 1e-1)

  intersect_train_val <- intersect(val_split$train_id, val_split$val_id)
  expect_equal(length(intersect_train_val), 0)
})

test_that("grouped split - accessor functions", {
  dat <- data.frame(
    id = 1:100,
    g = rep(1:5, each = 20)
  )

  set.seed(1)
  val_split <- group_initial_validation_split(dat, group = g, prop = c(0.6, 0.2))

  dat_train <- training(val_split)
  dat_val <- validation(val_split)
  dat_test <- testing(val_split)

  expect_equal(nrow(dat_train), 60)
  expect_equal(nrow(dat_val),20)
  expect_equal(nrow(dat_test), 20)

  expect_snapshot(error = TRUE, {
    analysis(val_split)
  })
  expect_snapshot(error = TRUE, {
    assessment(val_split)
  })
})


test_that("check_prop_3() works", {
  expect_snapshot(error = TRUE, check_prop_3(0.3))
  expect_snapshot(error = TRUE, check_prop_3("zero"))
  expect_snapshot(error = TRUE, check_prop_3(NULL))
  expect_snapshot(error = TRUE, check_prop_3(NA))
  expect_snapshot(error = TRUE, check_prop_3(0))

  expect_snapshot(error = TRUE, check_prop_3(c(0.3, NA)))
  expect_snapshot(error = TRUE, check_prop_3(c(0.3, NULL)))
  expect_snapshot(error = TRUE, check_prop_3(c(0.3, 1)))
  expect_snapshot(error = TRUE, check_prop_3(c(0.3, 0.7)))
})
