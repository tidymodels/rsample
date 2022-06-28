dat1 <- data.frame(a = 1:20, b = letters[1:20], c = rep(1:4, 5))
car_folds <- vfold_cv(mtcars)

new_rng_snapshots <- utils::compareVersion("3.6.0", as.character(getRversion())) > 0

expect_s3_class_rset <- function(x) {
  expect_s3_class(x, "rset")
}

expect_s3_class_bare_tibble <- function(x) {
  expect_s3_class(x, c("tbl_df", "tbl", "data.frame"), exact = TRUE)
}

get_stats <- function(split, ...) {
  dat <- analysis(split)
  x <- dat[[1]]
  tibble(
    term = "mean",
    estimate = mean(x, na.rm = TRUE),
    std.error = sqrt(var(x, na.rm = TRUE) / sum(!is.na(x)))
  )
}

check_ind <- function(x, tdat) {
  in_dat <- subset(tdat, Data == "Analysis")
  in_check <- all(sort(in_dat$Row) == x$in_ind)
  out_dat <- subset(tdat, Data == "Analysis")
  out_check <- all(sort(out_dat$Row) == x$out_ind)
  in_check & out_check
}

get_id_left_out <- function(x) {
  unique(as.character(assessment(x)$tension))
}
