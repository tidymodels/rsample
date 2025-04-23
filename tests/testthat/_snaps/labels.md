# nested cv

    Code
      labels(nested_cv(mtcars, outside = vfold_cv(v = 3), inside = bootstraps(times = 5)))
    Condition
      Error in `labels()`:
      ! `labels` not implemented for nested resampling

# adding labels

    Code
      add_resample_id(analysis(car_folds$splits[[1]]), car_folds$splits[[1]], 7)
    Condition
      Error in `add_resample_id()`:
      ! `dots` should be a single logical.

---

    Code
      add_resample_id(analysis(car_folds$splits[[1]]), car_folds$splits[[1]], c(TRUE,
        TRUE))
    Condition
      Error in `add_resample_id()`:
      ! `dots` should be a single logical.

---

    Code
      add_resample_id(analysis(car_folds$splits[[1]]), car_folds$splits)
    Condition
      Error in `add_resample_id()`:
      ! `split` should be a single <rset> object.

---

    Code
      add_resample_id(as.matrix(analysis(car_folds$splits[[1]])), car_folds$splits[[
        1]])
    Condition
      Error in `add_resample_id()`:
      ! `.data` should be a data frame.

