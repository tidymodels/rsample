#' Various simulations functions
#'
#' These functions can be used to simulate classification and regression
#' equations.
#'
#' @param num_samples Number of data points to simulate.
#' @param method A character string for the simulation method. For
#' classification, the current value is "caret". For classification, values can
#' be "sapp_2014_1", "sapp_2014_2", "van_der_laan_2007_1", or
#' "van_der_laan_2007_2". See Details below.
#' @param intercept The intercept for the linear predictor.
#' @param num_linear Number of diminishing linear effects.
#' @param std_dev Gaussian distribution standard deviation for residuals.
#' Default values are shown below in Details.
#' @param num_vars Number of noise predictors to create.
#' @param cov_type The multivariate normal correlation structure of the
#' predictors. Possible values are "exchangeable" and "toeplitz".
#' @param cov_param A single numeric value for the exchangeable correlation
#' value or the base of the toeplitz structure. See Details below.
#' @param factors A single logical for whether the binary indicators should be
#' encoded as factors or not.
#' @param outcome A single character string for what type of independent outcome should be
#' simulated (if any). The default value of "none" produces no extra columns.
#' Using "classification" will generate a `class` column with `num_classes`
#' values, equally distributed. A value of "regression" results in a `outcome`
#' column that contains independent standard normal values.
#' @param num_classes When `outcome = "classification"`, the number of classes
#' to simulate.
#'
#' @details
#' There are several supervised simulation methods (and one unsupervised). The
#' details are shown below by value of `method`.
#'
#' ## `caret`
#'
#' This is a simulated two-class problem, originally implemented in
#'  [caret::twoClassSim()] with all numeric predictors. The predictors are
#'  simulated in different sets. First, two multivariate normal predictors
#'  (denoted here as `two_factor_1` and `two_factor_2`) are created with a
#'  correlation of about 0.65. They change the log-odds using main effects and
#'  an interaction:
#'
#' \preformatted{ intercept - 4 * two_factor_1 + 4 * two_factor_2 + 2 * two_factor_1 * two_factor_2 }
#'
#' The intercept is a parameter for the simulation and can be used to control
#' the amount of class imbalance.
#'
#' The second set of effects are linear with coefficients that alternate signs
#' and have a sequence of values between 2.5 and 0.025. For example, if there
#' were four predictors in this set, their contribution to the log-odds would
#' be
#'
#' \preformatted{ -2.5 * linear_1 + 1.75 * linear_2 -1.00 * linear_3 + 0.25 * linear_4}
#'
#' (Note that these column names may change based on the value of `num_linear`).
#'
#' The third set is a nonlinear function of a single predictor ranging between
#' `[0, 1]` called `non_linear_1` here:
#'
#' \preformatted{ (non_linear_1^3) + 2 * exp(-6 * (non_linear_1 - 0.3)^2) }
#'
#' The fourth set of informative predictors are copied from one of Friedman's
#' systems and use two more predictors (`non_linear_2` and `non_linear_3`):
#'
#' \preformatted{ 2 * sin(non_linear_2 * non_linear_3) }
#'
#' All of these effects are added up to model the log-odds.
#'
#' ## `sapp_2014_1`
#'
#' This regression simulation from Sapp et al. (2014). There are 20 independent
#' Gaussian random predictors with mean zero and a variance of 9. The prediction
#' equation is:
#'
#' \preformatted{
#' predictor_01 + sin(predictor_02) + log(abs(predictor_03)) +
#'  predictor_04^2 + predictor_05 * predictor_06 +
#'  ifelse(predictor_07 * predictor_08 * predictor_09 < 0, 1, 0) +
#'  ifelse(predictor_10 > 0, 1, 0) + predictor_11 * ifelse(predictor_11 > 0, 1, 0) +
#'  sqrt(abs(predictor_12)) + cos(predictor_13) + 2 * predictor_14 + abs(predictor_15) +
#'  ifelse(predictor_16 < -1, 1, 0) + predictor_17 * ifelse(predictor_17 < -1, 1, 0) -
#'  2 * predictor_18 - predictor_19 * predictor_20
#' }
#'
#' The error is Gaussian with mean zero and variance 9.
#'
#' ## `sapp_2014_2`
#'
#' This regression simulation is also from Sapp et al. (2014). There are 200
#' independent Gaussian predictors with mean zero and variance 16. The
#' prediction equation has an intercept of one and identical linear effects of
#' `log(abs(predictor))`.
#'
#' The error is Gaussian with mean zero and variance 25.
#'
#' ## `van_der_laan_2007_1`
#'
#' This is a regression simulation from van der Laan et al. (2007) with ten
#' random Bernoulli variables that have a 40% probability of being a value of
#' one. The true regression equation is:
#'
#' \preformatted{
#' 2 * predictor_01 * predictor_10 + 4 * predictor_02 * predictor_07 +
#'   3 * predictor_04 * predictor_05 - 5 * predictor_06 * predictor_10 +
#'   3 * predictor_08 * predictor_09 + predictor_01 * predictor_02 * predictor_04 -
#'   2 * predictor_07 * (1 - predictor_06) * predictor_02 * predictor_09 -
#'   4 * (1 - predictor_10) * predictor_01 * (1 - predictor_04)
#' }
#'
#' The error term is standard normal.
#'
#' ## `van_der_laan_2007_2`
#'
#' Another regression simulation from van der Laan et al. (2007)  with twenty
#' Gaussians with mean zero and variance 16. The prediction equation is
#'
#' \preformatted{
#' predictor_01 * predictor_02 + predictor_10^2 - predictor_03 * predictor_17 -
#'  predictor_15 * predictor_04 + predictor_09 * predictor_05 + predictor_19 -
#'  predictor_20^2 + predictor_09 * predictor_08
#' }
#'
#' The error term is also Gaussian with mean zero and variance 16.
#'
#' ## `sim_noise`
#'
#' This simulates a number of random normal variables with mean zero. The
#' values can be independent if `cov_param = 0`. Otherwise the values are
#' multivariate normal with non-diagonal covariance matrices. For
#' `cov_type = "exchangeable"`, the structure has unit variances and covariances
#' of `cov_param`. `cov_type = "toeplitz"`, the covariances are an exponential
#' pattern (see example below).
#' @references
#' van der Laan, Mark J., Polley, Eric C and Hubbard, Alan E.. "Super Learner"
#' _Statistical Applications in Genetics and Molecular Biology_, vol. 6, no. 1,
#' 2007. DOI: 10.2202/1544-6115.1309.
#'
#' Stephanie Sapp, Mark J. van der Laan & John Canny (2014) Subsemble: an
#' ensemble method for combining subset-specific algorithm fits, _Journal of
#' Applied Statistics_, 41:6, 1247-1259, DOI: 10.1080/02664763.2013.864263
#' @examples
#' set.seed(1)
#' classfication_data <- sim_classification(100)
#'
#' # toeplitz covariance structure for four variables
#'
#' cov_value <- 1/2
#' toeplitz(cov_value^(seq(0, 3, by = 1)))
#'
#' set.seed(2)
#' dat <- sim_noise(1000, num_vars = 4, cov_type = "toeplitz", cov_param = 1/2)
#' round(cor(dat), 2)
#' @export
sim_classification <- function(num_samples = 100, method = "caret",
                               intercept = -5, num_linear = 10) {
  method <- rlang::arg_match0(method, "caret", arg_nm = "method")

  if (method == "caret") {
    # Simulate two correlated normal variates
    var_cov <- matrix(c(2, 1.3, 1.3, 2), 2, 2)
    dat <- MASS::mvrnorm(n = num_samples, c(0, 0), var_cov)

    # Simulate a uniform for the first nonlinear term
    dat <- cbind(dat, stats::runif(num_samples, min = -1))
    # Simulate second two nonlinear terms
    dat <- cbind(dat, matrix(stats::runif(num_samples * 2), ncol = 2))

    # Assign names
    colnames(dat) <- c(paste0("two_factor_", 1:2), paste0("non_linear_", 1:3))

    linear_pred <-
      rlang::expr(
        !!intercept - 4 * two_factor_1 + 4 * two_factor_2 +
          2 * two_factor_1 * two_factor_2 +
          (non_linear_1 ^ 3) + 2 * exp(-6 * (non_linear_1 - 0.3)^2) +
          2 * sin(pi * non_linear_2 * non_linear_3)
      )

    # Simulate a series of linear coefficients
    if(num_linear > 0) {
      dat_linear <- matrix(stats::rnorm(num_samples * num_linear), ncol = num_linear)
      lin_names <- names0(num_linear, "linear_")
      colnames(dat_linear) <- lin_names
      lin_symbols <- rlang::syms(lin_names)
      lin_coefs <-
        seq(10, 1, length = num_linear)/4 *
        rep_len(c(-1, 1), length.out = num_linear)
      lin_expr <-
        purrr::map2(lin_coefs, lin_symbols, ~ rlang::expr(!!.x * !!.y)) %>%
        purrr::reduce(function(l, r) rlang::expr(!!l + !!r))
      linear_pred <- rlang::expr(!!linear_pred + !!lin_expr)
      dat <- cbind(dat, dat_linear)
    }
  }

  dat <-
    tibble::as_tibble(dat) %>%
    dplyr::mutate(
      linear_pred = rlang::eval_tidy(linear_pred, data = .),
      true_prob = stats::binomial()$linkinv(linear_pred),
      rand = stats::runif(num_samples),
      class = ifelse(rand <= true_prob, "class_1", "class_2"),
      class = factor(class, levels = c("class_1", "class_2"))
    ) %>%
    dplyr::select(-linear_pred, -true_prob, -rand) %>%
    dplyr::relocate(class)

  dat
}

#' @export
#' @rdname sim_classification
sim_regression <-
  function(num_samples = 100, method = "sapp_2014_1", std_dev = NULL, factors = FALSE) {
    reg_methods <- c("sapp_2014_1", "sapp_2014_2", "van_der_laan_2007_1", "van_der_laan_2007_2")
    method <- rlang::arg_match0(method, reg_methods, arg_nm = "method")

    switch(
      method,
      sapp_2014_1 = sapp_2014_1(num_samples, std_dev),
      sapp_2014_2 = sapp_2014_2(num_samples, std_dev),
      van_der_laan_2007_1 = van_der_laan_2007_1(num_samples, std_dev, factors = factors),
      van_der_laan_2007_2 = van_der_laan_2007_2(num_samples, std_dev)
    )
  }


sapp_2014_1 <- function(num_samples = 100, std_dev = NULL) {
  if (is.null(std_dev)) {
    std_dev <- 3
  }
  dat <- matrix(stats::rnorm(num_samples * 20, sd = 3), ncol = 20)
  colnames(dat) <- names0(20, "predictor_")
  dat <- tibble::as_tibble(dat)

  slc_14 <- rlang::expr(
    predictor_01 + sin(predictor_02) + log(abs(predictor_03)) +
      predictor_04^2 + predictor_05 * predictor_06 +
      ifelse(predictor_07 * predictor_08 * predictor_09 < 0, 1, 0) +
      ifelse(predictor_10 > 0, 1, 0) + predictor_11 * ifelse(predictor_11 > 0, 1, 0) +
      sqrt(abs(predictor_12)) + cos(predictor_13) + 2 * predictor_14 + abs(predictor_15) +
      ifelse(predictor_16 < -1, 1, 0) + predictor_17 * ifelse(predictor_17 < -1, 1, 0) -
      2 * predictor_18 - predictor_19 * predictor_20
  )

  dat <-
    tibble::as_tibble(dat) %>%
    dplyr::mutate(
      linear_pred = rlang::eval_tidy(slc_14, data = .),
      outcome = linear_pred + stats::rnorm(num_samples, sd = std_dev)
    ) %>%
    dplyr::select(-linear_pred) %>%
    dplyr::relocate(outcome)

  dat
}

sapp_2014_2 <- function(num_samples = 100, std_dev = 4) {
  if (is.null(std_dev)) {
    std_dev <- 5
  }
  dat <- matrix(stats::rnorm(num_samples * 200, sd = 4), ncol = 200)
  colnames(dat) <- names0(200, "predictor_")

  slc_14 <- function(x) sum(log(abs(x)))

  y <- apply(dat, 1, slc_14) + stats::rnorm(num_samples, sd = std_dev)  - 1
  dat <- tibble::as_tibble(dat)
  dat$outcome <- y
  dplyr::relocate(dat, outcome)
}

van_der_laan_2007_1 <- function(num_samples = 100, std_dev = NULL, factors = FALSE) {
  if (is.null(std_dev)) {
    std_dev <- 1
  }
  dat <- matrix(stats::rbinom(num_samples * 10, size = 1, prob = .4), ncol = 10)
  colnames(dat) <- names0(10, "predictor_")
  dat <- tibble::as_tibble(dat)

  lph_07 <- rlang::expr(
    2 * predictor_01 * predictor_10 + 4 * predictor_02 * predictor_07 + 3 * predictor_04 *
      predictor_05 - 5 * predictor_06 * predictor_10 + 3 * predictor_08 * predictor_09 +
      predictor_01 *predictor_02 * predictor_04 -
      2 * predictor_07 * (1 - predictor_06) * predictor_02 *
      predictor_09 - 4 * (1 - predictor_10) * predictor_01 * (1 - predictor_04)
  )

  dat <-
    tibble::as_tibble(dat) %>%
    dplyr::mutate(
      linear_pred = rlang::eval_tidy(lph_07, data = .),
      outcome = linear_pred + stats::rnorm(num_samples, sd = std_dev)
    ) %>%
    dplyr::select(-linear_pred) %>%
    dplyr::relocate(outcome)

  if (factors) {
    dat <-
      dat %>%
      dplyr::mutate(
        dplyr::across(2:11, ~ ifelse(.x == 1, "yes", "no")),
        dplyr::across(2:11, ~ factor(.x, levels = c("yes", "no")))
      )
  }

  dat
}

van_der_laan_2007_2 <- function(num_samples = 100, std_dev = NULL) {
  if (is.null(std_dev)) {
    std_dev <- 4
  }
  dat <- matrix(stats::rnorm(num_samples * 20, sd = 4), ncol = 20)
  colnames(dat) <- names0(20, "predictor_")
  dat <- tibble::as_tibble(dat)

  lph_07 <- rlang::expr(
    predictor_01 * predictor_02 + predictor_10^2 - predictor_03 * predictor_17 -
      predictor_15 * predictor_04 + predictor_09 * predictor_05 + predictor_19 -
      predictor_20^2 + predictor_09 * predictor_08
  )

  dat <-
    tibble::as_tibble(dat) %>%
    dplyr::mutate(
      linear_pred = rlang::eval_tidy(lph_07, data = .),
      outcome = linear_pred + stats::rnorm(num_samples, sd = std_dev)
    ) %>%
    dplyr::select(-linear_pred) %>%
    dplyr::relocate(outcome)

  dat
}

#' @export
#' @rdname sim_classification
sim_noise <- function(num_samples, num_vars, cov_type = "exchangeable",
                      outcome = "none", num_classes = 2, cov_param = 0) {
  cov_type <- rlang::arg_match0(cov_type, c("exchangeable", "toeplitz"),
                                arg_nm = "cov_type")
  outcome <- rlang::arg_match0(outcome, c("none", "classification", "regression"),
                               arg_nm = "outcome")
  if(cov_type == "exchangeable") {
    var_cov <- matrix(cov_param, ncol = num_vars,  nrow = num_vars)
    diag(var_cov) <- 1
  } else {
    var_cov_values <- cov_param^(seq(0, num_vars - 1, by = 1))
    var_cov <- stats::toeplitz(var_cov_values)
  }
  dat <- MASS::mvrnorm(num_samples, mu = rep(0, num_vars), Sigma = var_cov)
  colnames(dat) <- names0(num_vars, "noise_")
  dat <- tibble::as_tibble(dat)

  if (outcome == "classification") {
    if (num_classes <= 0) {
      rlang::abort("'num_classes' should be a positive integer.")
    }
    cls <- names0(num_classes, "class_")
    dat <-
      dat %>%
      dplyr::mutate(
        class = sample(cls, num_samples, replace = TRUE),
        class = factor(class, levels = cls)
        ) %>%
      dplyr::relocate(class)
  } else if (outcome == "regression") {
    dat <-
      dat %>%
      dplyr::mutate(outcome = stats::rnorm(num_samples)
      ) %>%
      dplyr::relocate(outcome)
  }
  dat
}
