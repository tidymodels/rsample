# no assessment set

    Code
      assessment(xx$splits[[1]])
    Condition
      Error in `as.data.frame()`:
      ! There is no assessment data set for an `rsplit` object with class <perm_split>.

# bad args

    Code
      permutations(mtcars)
    Condition
      Error in `permutations()`:
      ! You must specify at least one column to permute.

---

    Code
      permutations(mtcars, foo)
    Condition
      Error in `permutations()`:
      ! Can't select columns that don't exist.
      x Column `foo` doesn't exist.

---

    Code
      permutations(mtcars, start_with("z"))
    Condition
      Error in `permutations()`:
      i In argument: `start_with("z")`.
      Caused by error in `start_with()`:
      ! could not find function "start_with"

---

    Code
      permutations(mtcars, everything())
    Condition
      Error in `permutations()`:
      ! You have selected all columns to permute.
      i This effectively reorders the rows in the original data without changing the data structure.
      > Please select fewer columns to permute.

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
      # i 15 more rows

