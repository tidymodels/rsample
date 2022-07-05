test_that("reverse_splits is working", {
  skip_if_not(rlang::is_installed("withr"))

  reversable_subclasses <- setdiff(names(rset_subclasses), "permutations")
  reversable_subclasses <- rset_subclasses[reversable_subclasses]
  for (x in reversable_subclasses) {

    set.seed(123)
    rev_x <- reverse_splits(x)
    expect_identical(analysis(x$splits[[1]]), assessment(rev_x$splits[[1]]))
    expect_identical(assessment(x$splits[[1]]), analysis(rev_x$splits[[1]]))
    expect_identical(class(x), class(rev_x))
    expect_identical(class(x$splits[[1]]), class(rev_x$splits[[1]]))

  }

  expect_snapshot(
    reverse_splits(1),
    error = TRUE
  )

  permutes <- permutations(mtcars, cyl)

  expect_snapshot(
    reverse_splits(permutes),
    error = TRUE
  )

  expect_snapshot(
    reverse_splits(permutes$splits[[1]]),
    error = TRUE
  )

})

test_that("reshuffle_rset is working", {

  skip_if_not(rlang::is_installed("withr"))
  supported_subclasses <- rset_subclasses[
    setdiff(names(rset_subclasses), "manual_rset")
  ]

  # Reshuffling with the same seed, in the same order,
  # should recreate the same objects
  out <- withr::with_seed(
    123,
    lapply(
      supported_subclasses,
      reshuffle_rset
    )
  )

  for (i in seq_along(supported_subclasses)) {
    expect_identical(
      out[[i]],
      supported_subclasses[[i]]
    )
  }

  supports_strata <- purrr::map_lgl(
    names(supported_subclasses),
    ~ any(names(formals(.x)) == "strata")
  )
  supports_strata <- names(supported_subclasses)[supports_strata]
  supports_strata <- supported_subclasses[supports_strata]
  for (i in seq_along(supports_strata)) {
    set.seed(123)
    resample <- do.call(
      names(supports_strata)[i],
      list(
        data = test_data(),
        strata = "y",
        breaks = 2,
        pool = 0.2
      )
    )
    set.seed(123)
    reshuffled_resample <- reshuffle_rset(resample)
    expect_identical(resample, reshuffled_resample)
  }
})
