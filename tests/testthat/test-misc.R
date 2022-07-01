test_that("reverse_splits is working", {
  for (x in rset_subclasses) {

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

})

