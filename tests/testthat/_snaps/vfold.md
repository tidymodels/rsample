# strata

    Code
      rs4 <- vfold_cv(mlc_churn, strata = state, pool = 0.01)
    Condition
      Warning:
      Stratifying groups that make up 1% of the data may be statistically risky.
      * Consider increasing `pool` to at least 0.1

# bad args

    Code
      vfold_cv(iris, strata = iris$Species)
    Condition
      Error in `vfold_cv()`:
      ! Can't select columns that don't exist.
      x Columns `setosa`, `setosa`, `setosa`, `setosa`, `setosa`, etc. don't exist.

---

    Code
      vfold_cv(iris, strata = c("Species", "Sepal.Width"))
    Condition
      Error in `strata_check()`:
      ! `strata` should be a single name or character value.

---

    Code
      vfold_cv(iris, v = -500)
    Condition
      Error in `vfold_cv()`:
      ! `v` must be a single positive integer greater than 1.

---

    Code
      vfold_cv(iris, v = 1)
    Condition
      Error in `vfold_cv()`:
      ! `v` must be a single positive integer greater than 1.

---

    Code
      vfold_cv(iris, v = NULL)
    Condition
      Error in `vfold_cv()`:
      ! `v` must be a single positive integer greater than 1.

---

    Code
      vfold_cv(iris, v = 500)
    Condition
      Error in `vfold_cv()`:
      ! The number of rows is less than `v` = 500.

---

    Code
      vfold_cv(iris, v = 150, repeats = 2)
    Condition
      Error in `vfold_cv()`:
      ! Repeated resampling when `v` is 150 would create identical resamples.

---

    Code
      vfold_cv(Orange, repeats = 0)
    Condition
      Error in `vfold_cv()`:
      ! `repeats` must be a single positive integer.

---

    Code
      vfold_cv(Orange, repeats = NULL)
    Condition
      Error in `vfold_cv()`:
      ! `repeats` must be a single positive integer.

---

    Code
      vfold_cv(mtcars, v = nrow(mtcars))
    Condition
      Error in `vfold_cv()`:
      ! Leave-one-out cross-validation is not supported by this function.
      x You set `v` to `nrow(data)`, which would result in a leave-one-out cross-validation.
      i Use `loo_cv()` in this case.

# printing

    Code
      vfold_cv(mtcars)
    Output
      #  10-fold cross-validation 
      # A tibble: 10 x 2
         splits         id    
         <list>         <chr> 
       1 <split [28/4]> Fold01
       2 <split [28/4]> Fold02
       3 <split [29/3]> Fold03
       4 <split [29/3]> Fold04
       5 <split [29/3]> Fold05
       6 <split [29/3]> Fold06
       7 <split [29/3]> Fold07
       8 <split [29/3]> Fold08
       9 <split [29/3]> Fold09
      10 <split [29/3]> Fold10

# grouping -- bad args

    Code
      group_vfold_cv(warpbreaks, group = warpbreaks$tension)
    Condition
      Error in `validate_group()`:
      ! Can't select columns that don't exist.
      x Columns `L`, `L`, `L`, `L`, `L`, etc. don't exist.

---

    Code
      group_vfold_cv(warpbreaks, group = c("tension", "wool"))
    Condition
      Error in `group_vfold_cv()`:
      ! `group` should be a single character value for the column that will be used for splitting.

---

    Code
      group_vfold_cv(warpbreaks, group = "tensio")
    Condition
      Error in `validate_group()`:
      ! Can't select columns that don't exist.
      x Column `tensio` doesn't exist.

---

    Code
      group_vfold_cv(warpbreaks)
    Condition
      Error in `group_vfold_cv()`:
      ! `group` should be a single character value for the column that will be used for splitting.

---

    Code
      group_vfold_cv(warpbreaks, group = "tension", v = 10)
    Condition
      Error in `group_vfold_cv()`:
      ! The number of groups is less than `v` = 10.

---

    Code
      group_vfold_cv(dat1, c, v = 4, repeats = 4)
    Condition
      Error in `group_vfold_cv()`:
      ! Repeated resampling when `v` is 4 would create identical resamples.

---

    Code
      group_vfold_cv(dat1, c, repeats = 4)
    Condition
      Error in `group_vfold_cv()`:
      ! Repeated resampling when `v` is "NULL" would create identical resamples.

---

    Code
      group_vfold_cv(Orange, v = 1, group = "Tree")
    Condition
      Error in `group_vfold_cv()`:
      ! `v` must be a single positive integer greater than 1.

# grouping -- other balance methods

    Code
      rs1
    Output
      # Group 5-fold cross-validation 
      # A tibble: 5 x 2
        splits             id       
        <list>             <chr>    
      1 <split [2364/566]> Resample1
      2 <split [2371/559]> Resample2
      3 <split [2360/570]> Resample3
      4 <split [2278/652]> Resample4
      5 <split [2347/583]> Resample5

# grouping -- strata

    Code
      sizes4
    Output
      # A tibble: 5 x 5
        analysis assessment      n     p id       
           <int>      <int>  <int> <int> <chr>    
      1    80004      19996 100000     3 Resample1
      2    79850      20150 100000     3 Resample2
      3    79912      20088 100000     3 Resample3
      4    80131      19869 100000     3 Resample4
      5    80103      19897 100000     3 Resample5

---

    Code
      group_vfold_cv(sample_data, group, strata = outcome)
    Condition
      Warning in `group_vfold_cv()`:
      Leaving `v = NULL` while using stratification will set `v` to the number of groups present in the least common stratum.
      i Set `v` explicitly to override this warning.
    Output
      # Group 30-fold cross-validation 
      # A tibble: 30 x 2
         splits               id        
         <list>               <chr>     
       1 <split [96070/3930]> Resample01
       2 <split [95898/4102]> Resample02
       3 <split [96079/3921]> Resample03
       4 <split [96008/3992]> Resample04
       5 <split [95982/4018]> Resample05
       6 <split [95955/4045]> Resample06
       7 <split [96025/3975]> Resample07
       8 <split [96053/3947]> Resample08
       9 <split [96030/3970]> Resample09
      10 <split [96069/3931]> Resample10
      # i 20 more rows

---

    Code
      sizes5
    Output
      # A tibble: 5 x 5
        analysis assessment      n     p id       
           <int>      <int>  <int> <int> <chr>    
      1    80096      19904 100000     3 Resample1
      2    79962      20038 100000     3 Resample2
      3    79928      20072 100000     3 Resample3
      4    80058      19942 100000     3 Resample4
      5    79956      20044 100000     3 Resample5

---

    Code
      group_vfold_cv(sample_data, group, strata = outcome)
    Condition
      Warning in `group_vfold_cv()`:
      Leaving `v = NULL` while using stratification will set `v` to the number of groups present in the least common stratum.
      i Set `v` explicitly to override this warning.
    Output
      # Group 30-fold cross-validation 
      # A tibble: 30 x 2
         splits               id        
         <list>               <chr>     
       1 <split [95985/4015]> Resample01
       2 <split [95983/4017]> Resample02
       3 <split [96052/3948]> Resample03
       4 <split [95867/4133]> Resample04
       5 <split [96056/3944]> Resample05
       6 <split [95956/4044]> Resample06
       7 <split [95975/4025]> Resample07
       8 <split [96062/3938]> Resample08
       9 <split [95932/4068]> Resample09
      10 <split [96051/3949]> Resample10
      # i 20 more rows

# grouping -- printing

    Code
      group_vfold_cv(warpbreaks, "tension")
    Output
      # Group 3-fold cross-validation 
      # A tibble: 3 x 2
        splits          id       
        <list>          <chr>    
      1 <split [36/18]> Resample1
      2 <split [36/18]> Resample2
      3 <split [36/18]> Resample3

# grouping -- printing with ...

    Code
      print(group_vfold_cv(warpbreaks, "tension"), n = 2)
    Output
      # Group 3-fold cross-validation 
      # A tibble: 3 x 2
        splits          id       
        <list>          <chr>    
      1 <split [36/18]> Resample1
      2 <split [36/18]> Resample2
      # i 1 more row

