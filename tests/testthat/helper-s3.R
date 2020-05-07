expect_s3_class_rset <- function(x) {
  expect_s3_class(x, "rset")
}

expect_s3_class_bare_tibble <- function(x) {
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
}
