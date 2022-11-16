# strata

    Code
      rs4 <- vfold_cv(mlc_churn, strata = state, pool = 0.01)
    Condition
      Warning:
      Stratifying groups that make up 1% of the data may be statistically risky.
      * Consider increasing `pool` to at least 0.1

# bad args

    `v` must be a single positive integer

---

    The number of rows is less than `v = 500`

---

    Repeated resampling when `v` is 150 would create identical resamples

---

    `repeats` must be a single positive integer

---

    `repeats` must be a single positive integer

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

    Repeated resampling when `v` is 4 would create identical resamples

---

    Repeated resampling when `v` is `NULL` would create identical resamples

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

    Leaving `v = NULL` while using stratification will set `v` to the number of groups present in the least common stratum.
    i Set `v` explicitly to override this warning.

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

    Leaving `v = NULL` while using stratification will set `v` to the number of groups present in the least common stratum.
    i Set `v` explicitly to override this warning.

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
      # ... with 1 more row

