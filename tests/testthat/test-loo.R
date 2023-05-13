test_that("Loooooo", {
  loo1 <- loo_cv(dat1)
  expect_equal(nrow(loo1), nrow(dat1))

  same_data <-
    purrr::map_lgl(loo1$splits, function(x) {
      all.equal(x$data, dat1)
    })
  expect_true(all(same_data))

  holdouts <-
    purrr::map_lgl(loo1$splits, function(x) {
      length(x$out_id) == 1
    })
  expect_true(all(holdouts))

  retained <-
    purrr::map_lgl(loo1$splits, function(x) {
      length(x$in_id) == (nrow(dat1) - 1)
    })
  expect_true(all(retained))
})

test_that("printing", {
  expect_snapshot(loo_cv(dat1))
})

test_that("rsplit labels", {
  rs <- loo_cv(mtcars)
  all_labs <- purrr::map(rs$splits, labels) %>%
    list_rbind()
  original_id <- rs[, grepl("^id", names(rs))]
  expect_equal(all_labs, original_id)
})
