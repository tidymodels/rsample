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


test_that("working with Surv objects - issue #443", {

  check_surv <- function(x) inherits(x$surv_obj, "Surv")

  srv <-
    list(
      age = c(74, 68, 56, 57, 60, 74, 76, 77, 39, 75, 66, 58),
      sex = c(1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 2),
      surv_obj = structure(
        c(306, 455, 1010, 210, 883, 1022, 1, 1, 0, 1, 1, 0,
          116, 188,  191, 105, 174,  177, 1, 0, 0, 0, 0, 0),
        dim = c(12L, 2L),
        dimnames = list(NULL, c("time", "status")),
        type = "right",
        class = "Surv"))
  surv_df <-
    structure(
      srv,
      row.names = paste(1:12),
      class = "data.frame")

  surv_tbl <- dplyr::as_tibble(surv_df)

  # ----------------------------------------------------------------------------
  # data frame input

  set.seed(472)
  surv_split_df <- initial_validation_split(surv_df, prop = c(.3, .3))

  expect_true(check_surv(surv_split_df$data))

  expect_true(check_surv(training(surv_split_df)))
  expect_true(check_surv(testing(surv_split_df)))
  expect_true(check_surv(validation(surv_split_df)))

  surv_rs_df <- validation_set(surv_split_df)
  expect_true(check_surv(surv_rs_df$splits[[1]]$data))

  expect_true(check_surv(training(surv_rs_df$splits[[1]])))
  expect_true(check_surv(validation(surv_rs_df$splits[[1]])))
  expect_true(check_surv(analysis(surv_rs_df$splits[[1]])))

  # ----------------------------------------------------------------------------
  # tibble input

  set.seed(472)
  surv_split_tbl <- initial_validation_split(surv_tbl, prop = c(.3, .3))

  expect_true(check_surv(surv_split_tbl$data))

  expect_true(check_surv(training(surv_split_tbl)))
  expect_true(check_surv(testing(surv_split_tbl)))
  expect_true(check_surv(validation(surv_split_tbl)))
  expect_true(check_surv(validation(surv_split_tbl)))

  surv_rs_tbl <- validation_set(surv_split_tbl)
  expect_true(check_surv(surv_rs_tbl$splits[[1]]$data))

  expect_true(check_surv(training(surv_rs_tbl$splits[[1]])))
  expect_true(check_surv(validation(surv_rs_tbl$splits[[1]])))
  expect_true(check_surv(analysis(surv_rs_tbl$splits[[1]])))
  expect_true(check_surv(assessment(surv_rs_tbl$splits[[1]])))

})
