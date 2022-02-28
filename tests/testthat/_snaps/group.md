# printing

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

# printing with ...

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

