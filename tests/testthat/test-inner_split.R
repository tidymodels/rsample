
# mc ---------------------------------------------------------------------

test_that("mc_split", {
  set.seed(11)
  r_set <- mc_cv(warpbreaks)
  split_args <- get_split_args(r_set)
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
  split_args <- get_split_args(r_set)
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
  split_args <- get_split_args(r_set)
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
  split_args <- get_split_args(r_set)
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
  split_args <- get_split_args(r_set)
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
