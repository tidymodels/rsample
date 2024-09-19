# bad args

    Code
      new_rset(car_folds$splits[1:2], car_folds$id)
    Condition
      Error in `new_rset()`:
      ! Split and ID vectors have different lengths.

---

    Code
      new_rset(car_folds$splits, car_folds["splits"])
    Condition
      Error in `new_rset()`:
      ! The `id` tibble column names should start with 'id'.

---

    Code
      new_rset(car_folds$splits, car_folds$splits)
    Condition
      Error in `new_rset()`:
      ! All ID columns should be character or factor vectors.

---

    Code
      new_rset(list(1), "x")
    Condition
      Error in `new_rset()`:
      ! Each element of `splits` must be an <rsplit> object.

---

    Code
      new_rset(car_folds$splits, car_folds$id, attrib = args)
    Condition
      Error in `new_rset()`:
      ! `attrib` should be a fully named list.

# not an rsplit

    Code
      analysis(folds$splits[1])
    Condition
      Error in `analysis()`:
      ! No method for objects of class: list

---

    Code
      assessment(folds$splits[1])
    Condition
      Error in `assessment()`:
      ! No method for objects of class: list

