# bad args

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
      # ... with 15 more rows
      # i Use `print(n = ...)` to see more rows

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
      # ... with 15 more rows
      # i Use `print(n = ...)` to see more rows

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
      # ... with 15 more rows
      # i Use `print(n = ...)` to see more rows

