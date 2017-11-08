library(rsample)
library(testthat)
library(dplyr)

###################################################################

obj_1 <- vfold_cv(mtcars)
obj_2 <- bootstraps(mtcars)
obj_3 <- rolling_origin(mtcars)
obj_4 <- nested_cv(mtcars, obj_1, inside = bootstraps(times = 5))
obj_5 <- mc_cv(mtcars)
obj_6 <- loo_cv(mtcars)
obj_7 <- group_vfold_cv(mtcars, group = "am")

###################################################################

check_att <- function(x, y)
  length(setdiff(names(attributes(x)), names(attributes(x)))) == 0

###################################################################

test_that('object types', {
  expect_true(rsample:::is_rset(obj_1))
  expect_false(rsample:::is_rset(obj_1[, -1]))  
})

###################################################################

test_that('dplyr ops', {
  expect_true(
    rsample:::is_rset(
      obj_2 %>% filter(id == "Bootstrap02")
    )
  )
  expect_true(  
    rsample:::is_rset(
      obj_3 %>% mutate(blah = substr(id, 1, 3))
    )    
  )
  expect_true(  
    rsample:::is_rset(
      obj_4 %>% select(splits, id)
    )    
  ) 
  expect_true(  
    rsample:::is_rset(
      obj_1 %>% arrange(id)
    )    
  )   
  expect_true(  
    rsample:::is_rset(
      obj_1 %>% mutate(blah = substr(id, 1, 3)) %>% rename(newer = blah)
    )    
  ) 
  expect_true(  
    check_att(
      obj_1 %>% mutate(blah = substr(id, 1, 3)) %>% rename(newer = blah), 
      obj_1
    )    
  )   
  expect_true(  
    rsample:::is_rset(
      obj_2 %>% mutate(blah = substr(id, 1, 3)) %>% rename(newer = blah)
    )    
  )  
  expect_true(  
    check_att(
      obj_2 %>% mutate(blah = substr(id, 1, 3)) %>% rename(newer = blah), 
      obj_2
    )
  )  
  expect_true(  
    rsample:::is_rset(
      obj_3 %>% mutate(blah = substr(id, 1, 3)) %>% rename(newer = blah)
    )    
  )  
  expect_true(  
    check_att(
      obj_3 %>% mutate(blah = substr(id, 1, 3)) %>% rename(newer = blah), 
      obj_3
    )
  )  
  expect_true(  
    rsample:::is_rset(
      obj_4 %>% mutate(blah = substr(id, 1, 3)) %>% rename(newer = blah)
    )    
  )  
  expect_true(  
    check_att(
      obj_4 %>% mutate(blah = substr(id, 1, 3)) %>% rename(newer = blah), 
      obj_4
    )
  )  
  expect_true(  
    rsample:::is_rset(
      obj_5 %>% mutate(blah = substr(id, 1, 3)) %>% rename(newer = blah)
    )    
  )  
  expect_true(  
    check_att(
      obj_5 %>% mutate(blah = substr(id, 1, 3)) %>% rename(newer = blah), 
      obj_5
    )
  )  
  expect_true(  
    rsample:::is_rset(
      obj_6 %>% mutate(blah = substr(id, 1, 3)) %>% rename(newer = blah)
    )    
  )  
  expect_true(  
    check_att(
      obj_6 %>% mutate(blah = substr(id, 1, 3)) %>% rename(newer = blah), 
      obj_6
    )
  )  
  expect_true(  
    rsample:::is_rset(
      obj_7 %>% mutate(blah = substr(id, 1, 3)) %>% rename(newer = blah)
    )    
  )  
  expect_true(  
    check_att(
      obj_7 %>% mutate(blah = substr(id, 1, 3)) %>% rename(newer = blah), 
      obj_7
    )
  )  
  expect_true(  
    rsample:::is_rset(
      obj_3 %>% slice(1L)
    )    
  )    
})
