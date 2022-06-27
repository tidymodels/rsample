# other balance methods

    Code
      rs1
    Output
      # Group 5-fold cross-validation 
      # A tibble: 5 x 2
        splits             id       
        <list>             <chr>    
      1 <split [2345/585]> Resample1
      2 <split [2290/640]> Resample2
      3 <split [2360/570]> Resample3
      4 <split [2344/586]> Resample4
      5 <split [2381/549]> Resample5

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

