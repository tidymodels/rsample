# bad args

    `vars` are required and must be variables in `data`.

---

    `v` must be a single positive integer

---

    The number of rows is less than `v = 500`

---

    `cluster_function` must be one of "kmeans" or "hclust", not "not an option".

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

