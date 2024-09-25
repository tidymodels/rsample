# bad args

    Code
      mc_cv(mtcars, prop = 1)
    Condition
      Error in `mc_splits()`:
      ! `prop` must be a number on (0, 1).

---

    Code
      mc_cv(warpbreaks, strata = warpbreaks$tension)
    Condition
      Error in `mc_cv()`:
      ! Can't select columns that don't exist.
      x Columns `L`, `L`, `L`, `L`, `L`, etc. don't exist.

---

    Code
      mc_cv(warpbreaks, strata = c("tension", "wool"))
    Condition
      Error in `mc_cv()`:
      ! `strata` must be a single string or `NULL`, not a character vector.

# printing

    Code
      mc_cv(warpbreaks)
    Output
      # Monte Carlo cross-validation (0.75/0.25) with 25 resamples 
      # A tibble: 25 x 2
         splits          id        
         <list>          <chr>     
       1 <split [40/14]> Resample01
       2 <split [40/14]> Resample02
       3 <split [40/14]> Resample03
       4 <split [40/14]> Resample04
       5 <split [40/14]> Resample05
       6 <split [40/14]> Resample06
       7 <split [40/14]> Resample07
       8 <split [40/14]> Resample08
       9 <split [40/14]> Resample09
      10 <split [40/14]> Resample10
      # i 15 more rows

# grouping - bad args

    Code
      group_mc_cv(warpbreaks, group = warpbreaks$tension)
    Condition
      Error in `validate_group()`:
      ! Can't select columns that don't exist.
      x Columns `L`, `L`, `L`, `L`, `L`, etc. don't exist.

---

    Code
      group_mc_cv(warpbreaks, group = c("tension", "wool"))
    Condition
      Error in `group_mc_cv()`:
      ! `group` must be a single string, not a character vector.

---

    Code
      group_mc_cv(warpbreaks, group = "tensio")
    Condition
      Error in `validate_group()`:
      ! Can't select columns that don't exist.
      x Column `tensio` doesn't exist.

---

    Code
      group_mc_cv(warpbreaks)
    Condition
      Error in `group_mc_cv()`:
      ! `group` must be a single string, not `NULL`.

---

    Code
      group_mc_cv(mtcars, group = "cyl", prop = 1)
    Condition
      Error in `balance_prop()`:
      ! `prop` must be a number between 0 and 1.

---

    Code
      group_mc_cv(warpbreaks, group = "tension", balance = "groups")
    Condition
      Error in `group_mc_cv()`:
      ! `...` must be empty.
      x Problematic argument:
      * balance = "groups"

---

    Code
      group_mc_cv(warpbreaks, group = "tension", prop = 0.99)
    Condition
      Error in `group_mc_cv()`:
      ! Some assessment sets contained zero rows
      i Consider using a non-grouped resampling method

# grouping -- strata

    Code
      sizes4
    Output
      # A tibble: 5 x 5
        analysis assessment     n     p id       
           <int>      <int> <int> <int> <chr>    
      1    37939      12061 50000     3 Resample1
      2    37063      12937 50000     3 Resample2
      3    37178      12822 50000     3 Resample3
      4    37950      12050 50000     3 Resample4
      5    37585      12415 50000     3 Resample5

# grouping - printing

    Code
      group_mc_cv(warpbreaks, "tension")
    Output
      # Group Monte Carlo cross-validation (0.75/0.25) with 25 resamples  
      # A tibble: 25 x 2
         splits          id        
         <list>          <chr>     
       1 <split [36/18]> Resample01
       2 <split [36/18]> Resample02
       3 <split [36/18]> Resample03
       4 <split [36/18]> Resample04
       5 <split [36/18]> Resample05
       6 <split [36/18]> Resample06
       7 <split [36/18]> Resample07
       8 <split [36/18]> Resample08
       9 <split [36/18]> Resample09
      10 <split [36/18]> Resample10
      # i 15 more rows

# grouping - printing with ...

    Code
      print(group_mc_cv(warpbreaks, "tension"), n = 2)
    Output
      # Group Monte Carlo cross-validation (0.75/0.25) with 25 resamples  
      # A tibble: 25 x 2
        splits          id        
        <list>          <chr>     
      1 <split [36/18]> Resample01
      2 <split [36/18]> Resample02
      # i 23 more rows

