test_that("classification simulation", {
  set.seed(1)
  dat_1 <- sim_classification(500, num_linear = 0)
  dat_2 <- sim_classification(10, num_linear = 11)
  dat_3 <- sim_classification(1000, num_linear = 1, intercept = 50)

  expect_equal(
    names(dat_1),
    c(
      "class", "two_factor_1", "two_factor_2", "non_linear_1", "non_linear_2",
      "non_linear_3"
    )
  )
  expect_equal(
    names(dat_2),
    c(
      "class", "two_factor_1", "two_factor_2", "non_linear_1", "non_linear_2",
      "non_linear_3", rsample:::names0(11, "linear_")
    )
  )
  expect_equal(
    names(dat_3),
    c(
      "class", "two_factor_1", "two_factor_2", "non_linear_1", "non_linear_2",
      "non_linear_3", "linear_1"
    )
  )
  expect_equal(nrow(dat_1), 500)
  expect_equal(nrow(dat_2), 10)
  expect_equal(nrow(dat_3), 1000)
  expect_true(all(vapply(dat_1[, -1], is.numeric, logical(1))))

  expect_equal(sum(dat_3 == "class_2"), 0)
  expect_equal(levels(dat_3$class), paste0("class_", 1:2))
  expect_error(
    sim_classification(5, method = "potato"),
    "must be one of"
  )
})

test_that("sapp_2014_1 simulation", {
  set.seed(1)
  dat_1 <- sim_regression(10, method = "sapp_2014_1")
  expect_equal(names(dat_1), c("outcome", rsample:::names0(20, "predictor_")))
  expect_equal(nrow(dat_1), 10)
  expect_true(all(vapply(dat_1, is.numeric, logical(1))))
  expect_error(
    sim_regression(5, method = "potato"),
    "must be one of"
  )
})

test_that("sapp_2014_2 simulation", {
  set.seed(1)
  dat_1 <- sim_regression(10, method = "sapp_2014_2")
  expect_equal(names(dat_1), c("outcome", rsample:::names0(200, "predictor_")))
  expect_equal(nrow(dat_1), 10)
  expect_true(all(vapply(dat_1, is.numeric, logical(1))))
})

test_that("van_der_laan_2007_1 simulation", {
  set.seed(1)
  dat_1 <- sim_regression(10, method = "van_der_laan_2007_1")
  dat_2 <- sim_regression(10, method = "van_der_laan_2007_1", factors = TRUE)
  expect_equal(names(dat_1), c("outcome", rsample:::names0(10, "predictor_")))
  expect_equal(nrow(dat_1), 10)
  expect_true(all(vapply(dat_1, is.numeric, logical(1))))
  expect_true(all(vapply(dat_1[, -1], is.integer, logical(1))))
  expect_true(all(vapply(dat_2[, -1], is.factor, logical(1))))
  expect_equal(levels(dat_2$predictor_01), c("yes", "no"))
})

test_that("van_der_laan_2007_2 simulation", {
  set.seed(1)
  dat_1 <- sim_regression(10, method = "van_der_laan_2007_2")
  expect_equal(names(dat_1), c("outcome", rsample:::names0(20, "predictor_")))
  expect_equal(nrow(dat_1), 10)
  expect_true(all(vapply(dat_1, is.numeric, logical(1))))
})


test_that("noise simulation", {
  set.seed(1)
  dat_1 <- sim_noise(1000, num_vars = 10)
  dat_2 <- sim_noise(1000, num_vars = 3, cov_param = .5)
  dat_3 <- sim_noise(1000, num_vars = 3, cov_type = "toeplitz", cov_param = .99)
  dat_4 <- sim_noise(10, num_vars = 3, outcome = "classification")
  dat_5 <- sim_noise(10, num_vars = 3, outcome = "classification", num_classes = 10)
  dat_6 <- sim_noise(10, num_vars = 3, outcome = "regression")

  expect_equal(names(dat_1), rsample:::names0(10, "noise_"))
  expect_equal(names(dat_2), rsample:::names0(3, "noise_"))
  expect_equal(nrow(dat_1), 1000)
  expect_equal(nrow(dat_4), 10)

  expect_true(all(vapply(dat_1, is.numeric, logical(1))))
  expect_true(all(vapply(dat_1[, -1], is.numeric, logical(1))))
  expect_true(is.factor(dat_5$class))
  expect_true(all(vapply(dat_6, is.numeric, logical(1))))

  cor_1 <- cor(dat_1)[upper.tri(cor(dat_1))]
  expect_true(all(cor_1 <= 0.1 & cor_1 >= -0.1))

  cor_2 <- cor(dat_2)[upper.tri(cor(dat_2))]
  expect_true(all(cor_2 <= 0.6 & cor_2 >= 0.4))

  cor_3 <- cor(dat_3)[upper.tri(cor(dat_3))]
  expect_true(all(cor_3 >= 0.95))

  expect_equal(levels(dat_4$class), paste0("class_", 1:2))
  expect_equal(levels(dat_5$class), rsample:::names0(10, "class_"))
})
