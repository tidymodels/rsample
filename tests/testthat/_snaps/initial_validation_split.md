# basic split - accessor functions

    Code
      analysis(val_split)
    Condition
      Error in `analysis()`:
      ! The initial validation split does not contain an analysis set.

---

    Code
      assessment(val_split)
    Condition
      Error in `assessment()`:
      ! The initial validation split does not contain an assessment set.

# basic split stratified

    Code
      initial_validation_split(dat, strata = does_not_exist)
    Condition
      Error in `initial_validation_split()`:
      ! Can't subset columns that don't exist.
      x Column `does_not_exist` doesn't exist.

---

    Code
      initial_validation_split(dat, strata = c(x, f))
    Condition
      Error in `initial_validation_split()`:
      ! Can't subset columns that don't exist.
      x Column `x` doesn't exist.

---

    Code
      initial_validation_split(dat, strata = rep(1:3, times = c(50, 25, 25)))
    Condition
      Error in `initial_validation_split()`:
      ! Can't subset columns past the end.
      i Location 3 doesn't exist.
      i There are only 2 columns.

# grouped split - accessor functions

    Code
      analysis(val_split)
    Condition
      Error in `analysis()`:
      ! The initial validation split does not contain an analysis set.

---

    Code
      assessment(val_split)
    Condition
      Error in `assessment()`:
      ! The initial validation split does not contain an assessment set.

# check_prop_3() works

    Code
      check_prop_3(0.3)
    Condition
      Error:
      ! `prop` needs to contain the proportions for training and validation.

---

    Code
      check_prop_3("zero")
    Condition
      Error:
      ! `prop` needs to be numeric.

---

    Code
      check_prop_3(NULL)
    Condition
      Error:
      ! `prop` needs to be numeric.

---

    Code
      check_prop_3(NA)
    Condition
      Error:
      ! `prop` needs to be numeric.

---

    Code
      check_prop_3(0)
    Condition
      Error:
      ! `prop` needs to contain the proportions for training and validation.

---

    Code
      check_prop_3(c(0.3, NA))
    Condition
      Error:
      ! `prop` cannot contain `NA`.

---

    Code
      check_prop_3(c(0.3, NULL))
    Condition
      Error:
      ! `prop` needs to contain the proportions for training and validation.

---

    Code
      check_prop_3(c(0.3, 1))
    Condition
      Error:
      ! Elements of `prop` need to be in (0, 1).

---

    Code
      check_prop_3(c(0.3, 0.7))
    Condition
      Error:
      ! The sum of the proportions in `prop` needs to be in (0, 1).

