test_that("reverse_splits is working", {
  for (x in rset_subclasses) {

    rev_x <- reverse_splits(x)
    expect_identical(analysis(x$splits[[1]]), assessment(rev_x$splits[[1]]))
    expect_identical(assessment(x$splits[[1]]), analysis(rev_x$splits[[1]]))

  }
})
