# reverse_splits is working

    Code
      reverse_splits(1)
    Condition
      Error in `reverse_splits()`:
      ! `x` must be either an `rsplit` or an `rset` object

---

    Code
      reverse_splits(permutes)
    Condition
      Error in `reverse_splits()`:
      ! Permutations cannot have their splits reversed

---

    Code
      reverse_splits(permutes$splits[[1]])
    Condition
      Error in `reverse_splits()`:
      ! Permutations cannot have their splits reversed

# reshuffle_rset is working

    Code
      out
    Output
      # Bootstrap sampling 
      # A tibble: 25 x 2
         splits          id         
         <list>          <chr>      
       1 <split [50/20]> Bootstrap01
       2 <split [50/14]> Bootstrap02
       3 <split [50/15]> Bootstrap03
       4 <split [50/14]> Bootstrap04
       5 <split [50/22]> Bootstrap05
       6 <split [50/20]> Bootstrap06
       7 <split [50/18]> Bootstrap07
       8 <split [50/19]> Bootstrap08
       9 <split [50/17]> Bootstrap09
      10 <split [50/17]> Bootstrap10
      # ... with 15 more rows

---

    Code
      out
    Output
      # Bootstrap sampling 
      # A tibble: 25 x 2
         splits          id         
         <list>          <chr>      
       1 <split [50/15]> Bootstrap01
       2 <split [50/20]> Bootstrap02
       3 <split [50/15]> Bootstrap03
       4 <split [50/25]> Bootstrap04
       5 <split [50/20]> Bootstrap05
       6 <split [50/25]> Bootstrap06
       7 <split [50/10]> Bootstrap07
       8 <split [50/15]> Bootstrap08
       9 <split [50/25]> Bootstrap09
      10 <split [50/20]> Bootstrap10
      # ... with 15 more rows

---

    Code
      out
    Output
      #  10-fold cross-validation repeated 2 times 
      # A tibble: 20 x 3
         splits         id      id2   
         <list>         <chr>   <chr> 
       1 <split [45/5]> Repeat1 Fold01
       2 <split [45/5]> Repeat1 Fold02
       3 <split [45/5]> Repeat1 Fold03
       4 <split [45/5]> Repeat1 Fold04
       5 <split [45/5]> Repeat1 Fold05
       6 <split [45/5]> Repeat1 Fold06
       7 <split [45/5]> Repeat1 Fold07
       8 <split [45/5]> Repeat1 Fold08
       9 <split [45/5]> Repeat1 Fold09
      10 <split [45/5]> Repeat1 Fold10
      11 <split [45/5]> Repeat2 Fold01
      12 <split [45/5]> Repeat2 Fold02
      13 <split [45/5]> Repeat2 Fold03
      14 <split [45/5]> Repeat2 Fold04
      15 <split [45/5]> Repeat2 Fold05
      16 <split [45/5]> Repeat2 Fold06
      17 <split [45/5]> Repeat2 Fold07
      18 <split [45/5]> Repeat2 Fold08
      19 <split [45/5]> Repeat2 Fold09
      20 <split [45/5]> Repeat2 Fold10

---

    Code
      out
    Output
      # Group 10-fold cross-validation 
      # A tibble: 10 x 2
         splits         id        
         <list>         <chr>     
       1 <split [45/5]> Resample01
       2 <split [45/5]> Resample02
       3 <split [45/5]> Resample03
       4 <split [45/5]> Resample04
       5 <split [45/5]> Resample05
       6 <split [45/5]> Resample06
       7 <split [45/5]> Resample07
       8 <split [45/5]> Resample08
       9 <split [45/5]> Resample09
      10 <split [45/5]> Resample10

---

    Code
      out
    Output
      # Leave-one-out cross-validation 
      # A tibble: 50 x 2
         splits         id        
         <list>         <chr>     
       1 <split [49/1]> Resample1 
       2 <split [49/1]> Resample2 
       3 <split [49/1]> Resample3 
       4 <split [49/1]> Resample4 
       5 <split [49/1]> Resample5 
       6 <split [49/1]> Resample6 
       7 <split [49/1]> Resample7 
       8 <split [49/1]> Resample8 
       9 <split [49/1]> Resample9 
      10 <split [49/1]> Resample10
      # ... with 40 more rows

---

    Code
      out
    Output
      # Monte Carlo cross-validation (0.75/0.25) with 25 resamples  
      # A tibble: 25 x 2
         splits          id        
         <list>          <chr>     
       1 <split [37/13]> Resample01
       2 <split [37/13]> Resample02
       3 <split [37/13]> Resample03
       4 <split [37/13]> Resample04
       5 <split [37/13]> Resample05
       6 <split [37/13]> Resample06
       7 <split [37/13]> Resample07
       8 <split [37/13]> Resample08
       9 <split [37/13]> Resample09
      10 <split [37/13]> Resample10
      # ... with 15 more rows

---

    Code
      out
    Output
      # Grouped Monte Carlo cross-validation (0.75/0.25) with 25 resamples  
      # A tibble: 25 x 2
         splits          id        
         <list>          <chr>     
       1 <split [40/10]> Resample01
       2 <split [40/10]> Resample02
       3 <split [40/10]> Resample03
       4 <split [40/10]> Resample04
       5 <split [40/10]> Resample05
       6 <split [40/10]> Resample06
       7 <split [40/10]> Resample07
       8 <split [40/10]> Resample08
       9 <split [40/10]> Resample09
      10 <split [40/10]> Resample10
      # ... with 15 more rows

---

    Code
      out
    Output
      # Nested resampling:
      #  outer: 3-fold cross-validation
      #  inner: Bootstrap sampling
      # A tibble: 3 x 3
        splits          id    inner_resamples     
        <list>          <chr> <list>              
      1 <split [33/17]> Fold1 <bootstraps [5 x 2]>
      2 <split [33/17]> Fold2 <bootstraps [5 x 2]>
      3 <split [34/16]> Fold3 <bootstraps [5 x 2]>

---

    Code
      out
    Output
      # Validation Set Split (0.75/0.25)  
      # A tibble: 1 x 2
        splits          id        
        <list>          <chr>     
      1 <split [37/13]> validation

---

    Code
      out
    Output
      # Rolling origin forecast resampling 
      # A tibble: 45 x 2
         splits         id     
         <list>         <chr>  
       1 <split [5/1]>  Slice01
       2 <split [6/1]>  Slice02
       3 <split [7/1]>  Slice03
       4 <split [8/1]>  Slice04
       5 <split [9/1]>  Slice05
       6 <split [10/1]> Slice06
       7 <split [11/1]> Slice07
       8 <split [12/1]> Slice08
       9 <split [13/1]> Slice09
      10 <split [14/1]> Slice10
      # ... with 35 more rows

---

    Code
      out
    Output
      # Sliding window resampling 
      # A tibble: 49 x 2
         splits        id     
         <list>        <chr>  
       1 <split [1/1]> Slice01
       2 <split [1/1]> Slice02
       3 <split [1/1]> Slice03
       4 <split [1/1]> Slice04
       5 <split [1/1]> Slice05
       6 <split [1/1]> Slice06
       7 <split [1/1]> Slice07
       8 <split [1/1]> Slice08
       9 <split [1/1]> Slice09
      10 <split [1/1]> Slice10
      # ... with 39 more rows

---

    Code
      out
    Output
      # Sliding index resampling 
      # A tibble: 49 x 2
         splits        id     
         <list>        <chr>  
       1 <split [1/1]> Slice01
       2 <split [1/1]> Slice02
       3 <split [1/1]> Slice03
       4 <split [1/1]> Slice04
       5 <split [1/1]> Slice05
       6 <split [1/1]> Slice06
       7 <split [1/1]> Slice07
       8 <split [1/1]> Slice08
       9 <split [1/1]> Slice09
      10 <split [1/1]> Slice10
      # ... with 39 more rows

---

    Code
      out
    Output
      # Sliding period resampling 
      # A tibble: 7 x 2
        splits        id    
        <list>        <chr> 
      1 <split [7/7]> Slice1
      2 <split [7/7]> Slice2
      3 <split [7/7]> Slice3
      4 <split [7/7]> Slice4
      5 <split [7/7]> Slice5
      6 <split [7/7]> Slice6
      7 <split [7/1]> Slice7

---

    Code
      out
    Output
      # Apparent sampling 
      # A tibble: 1 x 2
        splits          id      
        <list>          <chr>   
      1 <split [50/50]> Apparent

