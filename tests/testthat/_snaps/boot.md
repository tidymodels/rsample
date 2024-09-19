# grouping -- strata

    Code
      sizes4
    Output
      # A tibble: 5 x 5
        analysis assessment     n     p id        
           <int>      <int> <int> <int> <chr>     
      1    49669      18552 50000     3 Bootstrap1
      2    49943      17429 50000     3 Bootstrap2
      3    50133      16350 50000     3 Bootstrap3
      4    50314      18071 50000     3 Bootstrap4
      5    50253      17830 50000     3 Bootstrap5

# bad args

    Code
      bootstraps(warpbreaks, strata = warpbreaks$tension)
    Condition
      Error in `bootstraps()`:
      ! Can't select columns that don't exist.
      x Columns `L`, `L`, `L`, `L`, `L`, etc. don't exist.

---

    Code
      bootstraps(warpbreaks, strata = c("tension", "wool"))
    Condition
      Error in `strata_check()`:
      ! `strata` should be a single name or character value.

---

    Code
      group_bootstraps(warpbreaks, tension)
    Condition
      Warning in `group_bootstraps()`:
      Some assessment sets contained zero rows.
      i Consider using a non-grouped resampling method.
    Output
      # Group bootstrap sampling 
      # A tibble: 25 x 2
         splits          id         
         <list>          <chr>      
       1 <split [54/18]> Bootstrap01
       2 <split [54/36]> Bootstrap02
       3 <split [54/18]> Bootstrap03
       4 <split [54/18]> Bootstrap04
       5 <split [54/36]> Bootstrap05
       6 <split [54/18]> Bootstrap06
       7 <split [54/18]> Bootstrap07
       8 <split [54/18]> Bootstrap08
       9 <split [54/18]> Bootstrap09
      10 <split [54/18]> Bootstrap10
      # i 15 more rows

---

    Code
      bootstraps(mtcars[2, ])
    Condition
      Warning in `bootstraps()`:
      Some assessment sets contained zero rows.
    Output
      # Bootstrap sampling 
      # A tibble: 25 x 2
         splits        id         
         <list>        <chr>      
       1 <split [1/0]> Bootstrap01
       2 <split [1/0]> Bootstrap02
       3 <split [1/0]> Bootstrap03
       4 <split [1/0]> Bootstrap04
       5 <split [1/0]> Bootstrap05
       6 <split [1/0]> Bootstrap06
       7 <split [1/0]> Bootstrap07
       8 <split [1/0]> Bootstrap08
       9 <split [1/0]> Bootstrap09
      10 <split [1/0]> Bootstrap10
      # i 15 more rows

# printing

    Code
      set.seed(11)
      bootstraps(warpbreaks)
    Output
      # Bootstrap sampling 
      # A tibble: 25 x 2
         splits          id         
         <list>          <chr>      
       1 <split [54/20]> Bootstrap01
       2 <split [54/22]> Bootstrap02
       3 <split [54/17]> Bootstrap03
       4 <split [54/22]> Bootstrap04
       5 <split [54/17]> Bootstrap05
       6 <split [54/19]> Bootstrap06
       7 <split [54/18]> Bootstrap07
       8 <split [54/17]> Bootstrap08
       9 <split [54/23]> Bootstrap09
      10 <split [54/22]> Bootstrap10
      # i 15 more rows

