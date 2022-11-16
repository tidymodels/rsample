test_that("default param", {
  set.seed(11)
  rs1 <- clustering_cv(dat1, c, v = 2)

  same_data <-
    purrr::map_lgl(rs1$splits, function(x) {
      all.equal(x$data, dat1)
    })
  expect_true(all(same_data))

  good_holdout <- purrr::map_lgl(
    rs1$splits,
    function(x) {
      length(intersect(x$in_ind, x$out_id)) == 0
    }
  )
  expect_true(all(good_holdout))
})

test_that("repeated", {
  set.seed(11)
  rs2 <- clustering_cv(dat1, c, v = 2, repeats = 4)
  sizes2 <- dim_rset(rs2)

  same_data <-
    purrr::map_lgl(rs2$splits, function(x) {
      all.equal(x$data, dat1)
    })
  expect_true(all(same_data))

  good_holdout <- purrr::map_lgl(
    rs2$splits,
    function(x) {
      length(intersect(x$in_ind, x$out_id)) == 0
    }
  )
  expect_true(all(good_holdout))
})

test_that("bad args", {
  expect_snapshot_error(clustering_cv(dat1))
  expect_snapshot_error(clustering_cv(iris, Sepal.Length, v = -500))
  expect_snapshot_error(clustering_cv(iris, Sepal.Length, v = 500))
  expect_snapshot_error(clustering_cv(iris, Sepal.Length, cluster_function = "not an option"))
  expect_snapshot_error(clustering_cv(Orange, repeats = 0))
  expect_snapshot_error(clustering_cv(Orange, repeats = NULL))
})

test_that("printing", {
  set.seed(11)
  expect_snapshot(clustering_cv(dat1, c, v = 2))
})

test_that("rsplit labels", {
  rs <- clustering_cv(dat1, c, v = 2)
  all_labs <- purrr::map_df(rs$splits, labels)
  original_id <- rs[, grepl("^id", names(rs))]
  expect_equal(all_labs, original_id)
})
