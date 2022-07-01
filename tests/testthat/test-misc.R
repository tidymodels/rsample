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
  for (x in rset_subclasses) {
    if (inherits(x, "manual_rset")) next
    withr::with_seed(123, out <- reshuffle_rset(x))
    expect_snapshot(out)
  }
})

