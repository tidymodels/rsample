# strata

    Code
      rs4 <- vfold_cv(mlc_churn, strata = state, pool = 0.01)
    Warning <rlang_warning>
      Stratifying groups that make up 1% of the data may be statistically risky.
      * Consider increasing `pool` to at least 0.1

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

