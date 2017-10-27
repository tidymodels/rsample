library(testthat)
library(rsample)
library(purrr)

test_that('default param', {
  set.seed(11)
  rs1 <- nested_cv(mtcars[1:30,],
                   outside = vfold_cv(v = 10),
                   inside = vfold_cv(v = 3))
  sizes1 <- rsample:::dim_rset(rs1)
  expect_true(all(sizes1$analysis == 27))
  expect_true(all(sizes1$assessment == 3))  
  subsizes1 <- map(rs1$inner_resamples, rsample:::dim_rset)
  subsizes1 <- do.call("rbind", subsizes1)
  expect_true(all(subsizes1$analysis == 18))
  expect_true(all(subsizes1$assessment == 9)) 
  
  set.seed(11)
  rs2 <- nested_cv(mtcars[1:30,],
                   outside = vfold_cv(v = 10),
                   inside = bootstraps(times = 3))
  sizes2 <- rsample:::dim_rset(rs2)
  expect_true(all(sizes2$analysis == 27))
  expect_true(all(sizes2$assessment == 3))  
  subsizes2 <- map(rs2$inner_resamples, rsample:::dim_rset)
  subsizes2 <- do.call("rbind", subsizes2)
  expect_true(all(subsizes2$analysis == 27))
  
  set.seed(11)
  rs3 <- nested_cv(mtcars[1:30,],
                   outside = vfold_cv(v = 10),
                   inside = mc_cv(prop = 2/3, times = 3))
  sizes3 <- rsample:::dim_rset(rs3)
  expect_true(all(sizes3$analysis == 27))
  expect_true(all(sizes3$assessment == 3))  
  subsizes3 <- map(rs3$inner_resamples, rsample:::dim_rset)
  subsizes3 <- do.call("rbind", subsizes3)
  expect_true(all(subsizes3$analysis == 18))
  expect_true(all(subsizes3$assessment == 9))  
})

test_that('bad args', {
  expect_warning(
    nested_cv(mtcars,
              outside = bootstraps(times = 5),
              inside = vfold_cv(V = 3))
  )
  folds <- vfold_cv(mtcars)
  expect_error(
    nested_cv(mtcars,
              outside = vfold_cv(),
              inside = folds)
  )  
})

test_that('printing', {
  rs1 <- nested_cv(mtcars[1:30,],
                   outside = vfold_cv(v = 10),
                   inside = vfold_cv(v = 3))
  expect_output(print(rs1))
})

test_that('rsplit labels', {
  rs <- nested_cv(mtcars[1:30,],
                  outside = vfold_cv(v = 10),
                  inside = vfold_cv(v = 3))
  all_labs <- map_df(rs$splits, labels)
  original_id <- rs[, grepl("^id", names(rs))]
  expect_equal(all_labs, original_id)
})
