# print methods

    Code
      set.seed(233)
      vfold_cv(mtcars)$splits[[1]]
    Output
      <Analysis/Assess/Total>
      <28/4/32>

---

    Code
      set.seed(233)
      validation_split(mtcars)$splits[[1]]
    Output
      <Training/Validation/Total>
      <24/8/32>

---

    Code
      set.seed(233)
      validation_split(mtcars)
    Output
      # Validation Set Split (0.75/0.25)  
      # A tibble: 1 x 2
        splits         id        
        <list>         <chr>     
      1 <split [24/8]> validation

# default complement method errors

    Code
      complement("a string")
    Condition
      Error in `complement()`:
      ! No `complement()` method for this class(es) 'character'

