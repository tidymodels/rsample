# mc ---------------------------------------------------------------------

test_that("mc_split", {
  set.seed(11)
  r_set <- mc_cv(warpbreaks)
  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_identical(
    isplit$data,
    analysis(r_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, ],
    ignore_attr = "row.names"
  )
})

test_that("group_mc_split", {
  skip_if_not_installed("modeldata")

  data(ames, package = "modeldata", envir = rlang::current_env())

  set.seed(11)
  r_set <- group_mc_cv(ames, "MS_SubClass")
  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_identical(
    isplit$data,
    analysis(r_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, ],
    ignore_attr = "row.names"
  )
})


# vfold ------------------------------------------------------------------

test_that("vfold_split", {
  set.seed(11)
  r_set <- vfold_cv(warpbreaks, v = 5)
  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_identical(
    isplit$data,
    analysis(r_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, ],
    ignore_attr = "row.names"
  )
})

test_that("group_vfold_split", {
  skip_if_not_installed("modeldata")

  data(ames, package = "modeldata", envir = rlang::current_env())

  set.seed(11)
  r_set <- group_vfold_cv(ames, "MS_SubClass")
  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_identical(
    isplit$data,
    analysis(r_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, ],
    ignore_attr = "row.names"
  )
})

# bootstrap --------------------------------------------------------------

test_that("boot_split", {
  set.seed(11)
  r_set <- bootstraps(warpbreaks, times = 2)
  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_lte(
    nrow(isplit$data),
    analysis(r_split) |> nrow()
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[complement(isplit), ],
    ignore_attr = "row.names"
  )
})

test_that("group_boot_split", {
  skip_if_not_installed("modeldata")

  data(ames, package = "modeldata", envir = rlang::current_env())

  set.seed(11)
  r_set <- group_bootstraps(ames, group = "MS_SubClass", times = 2)
  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_lte(
    nrow(isplit$data),
    analysis(r_split) |> nrow()
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[complement(isplit), ],
    ignore_attr = "row.names"
  )
})


# validation set ---------------------------------------------------------

test_that("initial_validation_split", {
  set.seed(11)
  initial_vsplit <- initial_validation_split(
    warpbreaks,
    prop = c(0.6, 0.2)
  )
  r_set <- validation_set(initial_vsplit)
  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_identical(
    isplit$data,
    analysis(r_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, ],
    ignore_attr = "row.names"
  )
})

test_that("group_initial_validation_split", {
  skip_if_not_installed("modeldata")

  data(ames, package = "modeldata", envir = rlang::current_env())

  set.seed(11)
  initial_vsplit <- group_initial_validation_split(
    ames,
    group = "MS_SubClass",
    prop = c(0.7, 0.2)
  )
  r_set <- validation_set(initial_vsplit)
  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_identical(
    isplit$data,
    analysis(r_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, ],
    ignore_attr = "row.names"
  )
})

test_that("initial_validation_time_split", {
  set.seed(11)
  initial_vsplit <- initial_validation_time_split(
    warpbreaks,
    prop = c(0.6, 0.2)
  )
  r_set <- validation_set(initial_vsplit)
  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_identical(
    isplit$data,
    analysis(r_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[isplit$out_id, ],
    ignore_attr = "row.names"
  )
})


# clustering -------------------------------------------------------------

test_that("clustering_split", {
  set.seed(11)
  r_set <- clustering_cv(warpbreaks, vars = breaks, v = 5)
  split_args <- .get_split_args(r_set)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split, split_args)

  expect_identical(
    isplit$data,
    analysis(r_split)
  )

  expect_identical(
    analysis(isplit),
    isplit$data[isplit$in_id, ],
    ignore_attr = "row.names"
  )
  expect_identical(
    assessment(isplit),
    isplit$data[-isplit$in_id, ],
    ignore_attr = "row.names"
  )
})

# apparent ---------------------------------------------------------------

test_that("apparent_split", {
  set.seed(11)
  r_set <- apparent(warpbreaks)
  r_split <- get_rsplit(r_set, 1)

  isplit <- inner_split(r_split)

  expect_identical(
    isplit$data,
    analysis(r_split)
  )

  expect_identical(
    analysis(isplit),
    analysis(r_split)
  )
  expect_identical(
    assessment(isplit),
    analysis(r_split)
  )
})
