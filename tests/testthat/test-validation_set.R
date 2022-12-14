test_that("basic set", {
  dat1 <- data.frame(a = 1:20, b = letters[1:20], c = rep(1:4, 5))

  set.seed(11)
  initial_val_split <- initial_validation_split(dat1, prop = c(0.6, 0.2))
  val_set <- validation_set(initial_val_split)
  train_and_val <- dplyr::bind_rows(
    training(initial_val_split),
    validation(initial_val_split)
  )

  expect_s3_class(val_set, "rset")
  expect_s3_class(val_set, "validation_set")

  set_sizes <- dim_rset(val_set)
  expect_true(all(set_sizes$analysis == 12))
  expect_true(all(set_sizes$assessment == 4))

  expect_equal(length(val_set$splits), 1)
  expect_equal(
    val_set$splits[[1]]$data,
    train_and_val,
    ignore_attr = "row.names"
  )
})

test_that("accessor functions for `val_split`", {
  dat1 <- data.frame(a = 1:20, b = letters[1:20], c = rep(1:4, 5))

  set.seed(11)
  initial_val_split <- initial_validation_split(dat1, prop = c(0.6, 0.2))
  val_split <- validation_set(initial_val_split) %>% get_rsplit(1)

  expect_equal(analysis(val_split), training(initial_val_split))
  expect_equal(training(val_split), training(initial_val_split))
  expect_equal(assessment(val_split), validation(initial_val_split))
  expect_equal(validation(val_split), validation(initial_val_split))
  expect_snapshot(error = TRUE, testing(val_split))
})
