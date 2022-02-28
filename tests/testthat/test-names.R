test_that("basic naming sequences", {
  expect_equal(names0(2), c("x1", "x2"))
  expect_equal(names0(2, "y"), c("y1", "y2"))
  expect_equal(
    names0(10),
    c(paste0("x0", 1:9), "x10")
  )
})
