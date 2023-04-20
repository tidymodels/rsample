# bad args

    Code
      skip_if(new_rng_snapshots)
      set.seed(123)
      nested_cv(mtcars, outside = bootstraps(times = 5), inside = vfold_cv(v = 3))
    Condition
      Warning:
      Using bootstrapping as the outer resample is dangerous since the inner resample might have the same data point in both the analysis and assessment set.
    Output
      # Nested resampling:
      #  outer: Bootstrap sampling
      #  inner: 3-fold cross-validation
      # A tibble: 5 x 3
        splits          id         inner_resamples
        <list>          <chr>      <list>         
      1 <split [32/11]> Bootstrap1 <vfold [3 x 2]>
      2 <split [32/9]>  Bootstrap2 <vfold [3 x 2]>
      3 <split [32/10]> Bootstrap3 <vfold [3 x 2]>
      4 <split [32/14]> Bootstrap4 <vfold [3 x 2]>
      5 <split [32/11]> Bootstrap5 <vfold [3 x 2]>

---

    Code
      nested_cv(mtcars, outside = vfold_cv(), inside = folds)
    Condition
      Error in `list2()`:
      ! Argument 3 can't be empty.

# printing

    Code
      rs1
    Output
      # Nested resampling:
      #  outer: 10-fold cross-validation
      #  inner: 3-fold cross-validation
      # A tibble: 10 x 3
         splits         id     inner_resamples
         <list>         <chr>  <list>         
       1 <split [27/3]> Fold01 <vfold [3 x 2]>
       2 <split [27/3]> Fold02 <vfold [3 x 2]>
       3 <split [27/3]> Fold03 <vfold [3 x 2]>
       4 <split [27/3]> Fold04 <vfold [3 x 2]>
       5 <split [27/3]> Fold05 <vfold [3 x 2]>
       6 <split [27/3]> Fold06 <vfold [3 x 2]>
       7 <split [27/3]> Fold07 <vfold [3 x 2]>
       8 <split [27/3]> Fold08 <vfold [3 x 2]>
       9 <split [27/3]> Fold09 <vfold [3 x 2]>
      10 <split [27/3]> Fold10 <vfold [3 x 2]>

