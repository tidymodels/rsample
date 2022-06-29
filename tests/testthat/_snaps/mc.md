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
      # ... with 15 more rows

# grouping - bad args

    Code
      group_mc_cv(warpbreaks, group = "tension", prop = 0.99)
    Condition
      Error in `group_mc_cv()`:
      ! Some assessment sets contained 0 data
      i Consider using a non-grouped resampling method

# grouping - printing

    Code
      group_mc_cv(warpbreaks, "tension")
    Output
      # Grouped Monte Carlo cross-validation (0.75/0.25) with 25 resamples  
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
      # ... with 15 more rows

# grouping - printing with ...

    Code
      print(group_mc_cv(warpbreaks, "tension"), n = 2)
    Output
      # Grouped Monte Carlo cross-validation (0.75/0.25) with 25 resamples  
      # A tibble: 25 x 2
        splits          id        
        <list>          <chr>     
      1 <split [36/18]> Resample01
      2 <split [36/18]> Resample02
      # ... with 23 more rows

