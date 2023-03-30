test_that("default param", {
  set.seed(11)
  rs1 <- nested_cv(mtcars[1:30, ],
    outside = vfold_cv(v = 10),
    inside = vfold_cv(v = 3)
  )
  sizes1 <- dim_rset(rs1)
  expect_true(all(sizes1$analysis == 27))
  expect_true(all(sizes1$assessment == 3))
  subsizes1 <- purrr::map(rs1$inner_resamples, dim_rset)
  subsizes1 <- do.call("rbind", subsizes1)
  expect_true(all(subsizes1$analysis == 18))
  expect_true(all(subsizes1$assessment == 9))

  set.seed(11)
  rs2 <- nested_cv(mtcars[1:30, ],
    outside = vfold_cv(v = 10),
    inside = bootstraps(times = 3)
  )
  sizes2 <- dim_rset(rs2)
  expect_true(all(sizes2$analysis == 27))
  expect_true(all(sizes2$assessment == 3))
  subsizes2 <- purrr::map(rs2$inner_resamples, dim_rset)
  subsizes2 <- do.call("rbind", subsizes2)
  expect_true(all(subsizes2$analysis == 27))

  set.seed(11)
  rs3 <- nested_cv(mtcars[1:30, ],
    outside = vfold_cv(v = 10),
    inside = mc_cv(prop = 2 / 3, times = 3)
  )
  sizes3 <- dim_rset(rs3)
  expect_true(all(sizes3$analysis == 27))
  expect_true(all(sizes3$assessment == 3))
  subsizes3 <- purrr::map(rs3$inner_resamples, dim_rset)
  subsizes3 <- do.call("rbind", subsizes3)
  expect_true(all(subsizes3$analysis == 18))
  expect_true(all(subsizes3$assessment == 9))
})

test_that("bad args", {
  expect_snapshot({
    skip_if(new_rng_snapshots)
    set.seed(123)
    nested_cv(mtcars,
      outside = bootstraps(times = 5),
      inside = vfold_cv(V = 3)
    )
  })
  folds <- vfold_cv(mtcars)
  expect_snapshot(
    nested_cv(mtcars,
      outside = vfold_cv(),
      inside = folds
    ),
    error = TRUE
  )
})

test_that("can pass in variables", {
  make_folds <- function(df) {
    outer_cv <- 5
    inner_cv <- 4
    nested_cv(df,
      outside = vfold_cv(v = outer_cv),
      inside = vfold_cv(v = inner_cv)
    )
  }

  rs1 <- make_folds(mtcars[1:30, ])
  sizes1 <- dim_rset(rs1)
  expect_true(all(sizes1$analysis == 24))
  expect_true(all(sizes1$assessment == 6))
})

test_that("printing", {
  rs1 <- nested_cv(mtcars[1:30, ],
    outside = vfold_cv(v = 10),
    inside = vfold_cv(v = 3)
  )
  expect_snapshot(rs1)
})

test_that("rsplit labels", {
  rs <- nested_cv(mtcars[1:30, ],
    outside = vfold_cv(v = 10),
    inside = vfold_cv(v = 3)
  )
  all_labs <- purrr::map(rs$splits, labels) %>%
    list_rbind()
  original_id <- rs[, grepl("^id", names(rs))]
  expect_equal(all_labs, original_id)
})

# ------------------------------------------------------------------------------
# `[`
skip_if_not_installed("withr")
test_that("can keep the rset class", {
  x <- rset_subclasses$nested_cv
  loc <- seq_len(ncol(x))
  expect_s3_class_rset(x[loc])
})

test_that("drops the rset class if missing `inner_resamples`", {
  x <- rset_subclasses$nested_cv

  names <- names(x)
  names <- names[names != "inner_resamples"]

  expect_s3_class_bare_tibble(x[names])
})

test_that("drops the rset class if duplicating `inner_resamples`", {
  x <- rset_subclasses$nested_cv

  names <- names(x)
  names <- c(names, "inner_resamples")

  expect_s3_class_bare_tibble(x[names])
})

# ------------------------------------------------------------------------------
# `names<-`

test_that("can keep the rset subclass when renaming doesn't touch rset columns", {
  x <- rset_subclasses$nested_cv
  x <- mutate(x, a = 1)

  names <- names(x)
  names[names == "a"] <- "b"

  names(x) <- names

  expect_s3_class_rset(x)
})

test_that("drops the rset class if `inner_resamples` is renamed", {
  x <- rset_subclasses$nested_cv

  names <- names(x)
  names[names == "inner_resamples"] <- "inner_things"

  names(x) <- names

  expect_s3_class_bare_tibble(x)
})

test_that("drops the rset class if `inner_resamples` is moved", {
  x <- rset_subclasses$nested_cv
  x <- mutate(x, a = 1)

  names <- names(x)
  new_names <- names
  new_names[names == "inner_resamples"] <- "a"
  new_names[names == "a"] <- "inner_resamples"

  names(x) <- new_names

  expect_s3_class_bare_tibble(x)
})
