# no assessment set

    Code
      assessment(xx$splits[[1]])
    Condition
      Error in `as.data.frame()`:
      ! There is no assessment data set for an `rsplit` object with class `perm_split`.

# printing

    Code
      permutations(mtcars, 1)
    Output
      # Permutation sampling 
      # Permuted columns: [mpg] 
      # A tibble: 25 x 2
         splits         id            
         <list>         <chr>         
       1 <split [32/0]> Permutations01
       2 <split [32/0]> Permutations02
       3 <split [32/0]> Permutations03
       4 <split [32/0]> Permutations04
       5 <split [32/0]> Permutations05
       6 <split [32/0]> Permutations06
       7 <split [32/0]> Permutations07
       8 <split [32/0]> Permutations08
       9 <split [32/0]> Permutations09
      10 <split [32/0]> Permutations10
      # ... with 15 more rows

