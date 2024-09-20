# bad args

    Code
      clustering_cv(dat1)
    Condition
      Error in `clustering_cv()`:
      ! `vars` is required and must contain at least one variable in `data`.

---

    Code
      clustering_cv(iris, Sepal.Length, v = -500)
    Condition
      Error in `clustering_cv()`:
      ! `v` must be a single positive integer greater than 1.

---

    Code
      clustering_cv(iris, Sepal.Length, v = 500)
    Condition
      Error in `clustering_cv()`:
      ! The number of rows is less than `v` = 500.

---

    Code
      clustering_cv(iris, Sepal.Length, cluster_function = "not an option")
    Condition
      Error in `clustering_cv()`:
      ! `cluster_function` must be one of "kmeans" or "hclust", not "not an option".

---

    Code
      clustering_cv(Orange, v = 1, vars = "Tree")
    Condition
      Error in `clustering_cv()`:
      ! `v` must be a single positive integer greater than 1.

---

    Code
      clustering_cv(Orange, repeats = 0)
    Condition
      Error in `clustering_cv()`:
      ! `repeats` must be a whole number larger than or equal to 1, not the number 0.

---

    Code
      clustering_cv(Orange, repeats = NULL)
    Condition
      Error in `clustering_cv()`:
      ! `repeats` must be a whole number, not `NULL`.

---

    Code
      clustering_cv(mtcars, mpg, v = nrow(mtcars))
    Condition
      Error in `clustering_cv()`:
      ! Leave-one-out cross-validation is not supported by this function.
      x You set `v` to `nrow(data)`, which would result in a leave-one-out cross-validation.
      i Use `loo_cv()` in this case.

# printing

    Code
      clustering_cv(dat1, c, v = 2)
    Output
      # 2-cluster cross-validation 
      # A tibble: 2 x 2
        splits         id   
        <list>         <chr>
      1 <split [5/15]> Fold1
      2 <split [15/5]> Fold2

