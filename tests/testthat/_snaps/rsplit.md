# bad inputs

    Code
      rsplit(as.list(dat1), 1:2, 4:5)
    Condition
      Error in `rsplit()`:
      ! `data` must be a data frame.

---

    Code
      rsplit(dat1, letters[1:2], 4:5)
    Condition
      Error in `rsplit()`:
      ! `in_id` must be a positive integer vector.

---

    Code
      rsplit(as.list(dat1), 1:2, letters[4:5])
    Condition
      Error in `rsplit()`:
      ! `data` must be a data frame.

---

    Code
      rsplit(as.list(dat1), -1:2, 4:5)
    Condition
      Error in `rsplit()`:
      ! `data` must be a data frame.

---

    Code
      rsplit(as.list(dat1), 1:2, -4:5)
    Condition
      Error in `rsplit()`:
      ! `data` must be a data frame.

---

    Code
      rsplit(as.list(dat1), integer(0), 4:5)
    Condition
      Error in `rsplit()`:
      ! `data` must be a data frame.

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

# `complement()` error messages

    Code
      complement(fake_rsplit)
    Condition
      Error in `complement()`:
      ! No `complement()` method for objects of class: <not_an_rsplit>

---

    Code
      complement(fake_rsplit)
    Condition
      Error in `complement()`:
      ! No `complement()` method for objects of classes: <not_an_rsplit/really_not_an_rsplit>

---

    Code
      get_stored_out_id(list(out_id = NA))
    Condition
      Error in `get_stored_out_id()`:
      ! Cannot derive the assessment set for this type of resampling with class: <list>.

