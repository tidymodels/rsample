expect_equal_data_frames <- function(object, expected) {
  expect_equal(as.data.frame(object), as.data.frame(expected))
}
