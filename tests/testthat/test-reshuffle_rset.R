test_that("reshuffle_rset is working", {
  skip_if_not_installed("withr")
  # for `validation_split()` and variants
  withr::local_options(lifecycle_verbosity = "quiet")

  supported_subclasses <- rset_subclasses[
    setdiff(names(rset_subclasses), c("manual_rset"))
  ]

  # Reshuffling with the same seed, in the same order,
  # should recreate the same objects
  out <- withr::with_seed(
    123,
    lapply(
      supported_subclasses,
      function(x) suppressWarnings(reshuffle_rset(x))
    )
  )

  for (i in seq_along(supported_subclasses)) {
    expect_identical(
      out[[i]],
      supported_subclasses[[i]]
    )
  }

  # Check to make sure that stratification,
  # with non-default arguments,
  # is supported by reshuffled_resample

  # Select any non-grouped function in rset_subclasses with a strata argument:
  supports_strata <- purrr::map_lgl(
    names(supported_subclasses),
    ~ any(names(formals(.x)) == "strata") && !any(names(formals(.x)) == "group")
  )
  supports_strata <- names(supported_subclasses)[supports_strata]

  for (i in seq_along(supports_strata)) {
    # Fit those functions with non-default arguments:
    set.seed(123)
    resample <- do.call(
      supports_strata[i],
      list(
        data = test_data(),
        strata = "y",
        breaks = 2,
        pool = 0.2
      )
    )
    # Reshuffle them under the same seed to ensure they're identical
    set.seed(123)
    reshuffled_resample <- reshuffle_rset(resample)
    expect_identical(resample, reshuffled_resample)
  }

  # Select any grouped function in rset_subclasses with a strata argument:
  grouped_strata <- purrr::map_lgl(
    names(supported_subclasses),
    ~ any(names(formals(.x)) == "strata") && any(names(formals(.x)) == "group")
  )
  grouped_strata <- names(supported_subclasses)[grouped_strata]

  set.seed(11)

  group_table <- tibble::tibble(
    group = 1:100,
    outcome = sample(c(rep(0, 89), rep(1, 11)))
  )
  observation_table <- tibble::tibble(
    group = sample(1:100, 5e4, replace = TRUE),
    observation = 1:5e4
  )
  sample_data <- dplyr::full_join(
    group_table,
    observation_table,
    by = "group",
    multiple = "all"
  )

  for (i in seq_along(grouped_strata)) {
    # Fit those functions with non-default arguments:
    set.seed(123)
    resample <- suppressWarnings(
      do.call(
        grouped_strata[i],
        list(
          data = sample_data,
          group = "group",
          strata = "outcome",
          pool = 0.2
        )
      )
    )
    # Reshuffle them under the same seed to ensure they're identical
    set.seed(123)
    reshuffled_resample <- reshuffle_rset(resample)
    expect_identical(resample, reshuffled_resample)
  }

  for (i in seq_along(non_random_classes)) {
    expect_snapshot(
      reshuffle_rset(rset_subclasses[[non_random_classes[[i]]]])
    )
  }

  resample <- vfold_cv(mtcars, strata = cyl)
  attr(resample, "strata") <- TRUE

  expect_snapshot_error(reshuffle_rset(resample))

  expect_snapshot_error(reshuffle_rset(rset_subclasses[["manual_rset"]]))

  expect_snapshot_error(reshuffle_rset(rset_subclasses[["manual_rset"]]$splits[[1]]))

})
