library(testthat)
library(rsample)

cars_10fold <- vfold_cv(mtcars)

test_that('bad args', {
  expect_error(
    rsample:::new_rset(cars_10fold$splits[1:2], cars_10fold$id)
  )
  expect_error(
    rsample:::new_rset(cars_10fold$splits, cars_10fold[ "splits"])
  )  
  expect_error(
    rsample:::new_rset(cars_10fold$splits, cars_10fold$splits)
  )  
  args <- list(a = 1, b = 2, 3)
  expect_error(
    rsample:::new_rset(
      cars_10fold$splits, 
      cars_10fold$id, 
      attrib = args
      )
  )
})

test_that('simple rset', {
  res1 <- rsample:::new_rset(
    cars_10fold$splits, 
    cars_10fold$id
  )
  expect_equal(names(res1), c("splits", "id"))
  expect_equal(class(res1), c("tbl_df", "tbl", "data.frame"))
  expect_equal(sort(names(attributes(res1))), 
               c("class", "names", "row.names"))
  
  res2 <- rsample:::new_rset(
    cars_10fold[, "splits"], 
    cars_10fold[, "id"]
  )
  expect_equal(names(res2), c("splits", "id"))
  expect_equal(class(res2), c("tbl_df", "tbl", "data.frame"))
  expect_equal(sort(names(attributes(res2))), 
               c("class", "names", "row.names"))  
})

test_that('rset with attributes', {
  args <- list(value = "potato")
  res3 <- rsample:::new_rset(
    cars_10fold$splits, 
    cars_10fold$id,
    attrib = args
  )
  expect_equal(sort(names(attributes(res3))), 
               c("class", "names", "row.names", "value"))  
  expect_equal(attr(res3, "value"), "potato")    
})

test_that('rset with additional classes', {
  res4 <- rsample:::new_rset(
    cars_10fold$splits, 
    cars_10fold$id,
    subclass = "potato"
  )
  expect_equal(class(res4), 
               c("potato", "tbl_df", "tbl", "data.frame"))   
})




