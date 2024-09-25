# Upper & lower confidence interval does not contain NA

    Code
      int_pctl(bt_resamples, res)
    Condition
      Warning in `int_pctl()`:
      Recommend at least 1000 non-missing bootstrap resamples for term `mean`.
      Error in `pctl_single()`:
      ! All statistics have missing values.

---

    Code
      int_t(bt_resamples, res)
    Condition
      Warning in `int_t()`:
      Recommend at least 500 non-missing bootstrap resamples for term `mean`.
      Error in `t_single()`:
      ! All statistics have missing values.

---

    Code
      int_bca(bt_resamples, res, .fn = bad_stats)
    Condition
      Warning in `int_bca()`:
      Recommend at least 1000 non-missing bootstrap resamples for term `mean`.
      Error in `bca_calc()`:
      ! All statistics have missing values.

# Sufficient replications needed to sufficiently reduce Monte Carlo sampling Error for BCa method

    Code
      int_pctl(bt_small, stats)
    Condition
      Warning in `int_pctl()`:
      Recommend at least 1000 non-missing bootstrap resamples for term `mean`.
    Output
      # A tibble: 1 x 6
        term  .lower .estimate .upper .alpha .method   
        <chr>  <dbl>     <dbl>  <dbl>  <dbl> <chr>     
      1 mean    9.97      10.0   10.1   0.05 percentile

---

    Code
      int_t(bt_small, stats)
    Condition
      Warning in `int_t()`:
      Recommend at least 500 non-missing bootstrap resamples for term `mean`.
    Output
      # A tibble: 1 x 6
        term  .lower .estimate .upper .alpha .method  
        <chr>  <dbl>     <dbl>  <dbl>  <dbl> <chr>    
      1 mean    9.96      10.0   10.1   0.05 student-t

# bad input

    Code
      int_pctl(bt_small, id)
    Condition
      Error in `int_pctl()`:
      ! `statistics` should select a list column of tidy results.

---

    Code
      int_pctl(bt_small, junk)
    Condition
      Error in `int_pctl()`:
      ! `statistics` should select a list column of tidy results.

---

    Code
      int_pctl(bt_small, stats, alpha = c(0.05, 0.2))
    Condition
      Error in `int_pctl()`:
      ! `alpha` must be a single numeric value.

---

    Code
      int_t(bt_small, stats, alpha = "potato")
    Condition
      Error in `int_t()`:
      ! `alpha` must be a single numeric value.

---

    Code
      int_bca(bt_small, stats, alpha = 1:2, .fn = get_stats)
    Condition
      Error in `int_bca()`:
      ! `alpha` must be a single numeric value.

---

    Code
      int_pctl(vfold_cv(mtcars))
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'int_pctl' applied to an object of class "c('vfold_cv', 'rset', 'tbl_df', 'tbl', 'data.frame')"

---

    Code
      int_t(vfold_cv(mtcars))
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'int_t' applied to an object of class "c('vfold_cv', 'rset', 'tbl_df', 'tbl', 'data.frame')"

---

    Code
      int_bca(vfold_cv(mtcars))
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'int_bca' applied to an object of class "c('vfold_cv', 'rset', 'tbl_df', 'tbl', 'data.frame')"

---

    Code
      int_t(bad_bt_norm, stats)
    Condition
      Error in `int_t()`:
      ! `statistics` should select a single column for the standard error.

---

    Code
      int_bca(bt_norm, stats, .fn = no_dots)
    Condition
      Error in `int_bca()`:
      ! `.fn` must have an argument `...`.

---

    Code
      int_pctl(as.data.frame(bt_norm), stats)
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'int_pctl' applied to an object of class "data.frame"

---

    Code
      int_t(as.data.frame(bt_norm), stats)
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'int_t' applied to an object of class "data.frame"

---

    Code
      int_bca(as.data.frame(bt_norm), stats, .fn = get_stats)
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'int_bca' applied to an object of class "data.frame"

---

    Code
      int_t(bt_norm %>% dplyr::filter(id != "Apparent"), stats)
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'int_t' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"

---

    Code
      int_bca(bt_norm %>% dplyr::filter(id != "Apparent"), stats, .fn = get_stats)
    Condition
      Error in `UseMethod()`:
      ! no applicable method for 'int_bca' applied to an object of class "c('tbl_df', 'tbl', 'data.frame')"

---

    Code
      int_pctl(badder_bt_norm, bad_term)
    Condition
      Error in `int_pctl()`:
      ! The tibble in `statistics` must have a column for 'term'.

---

    Code
      int_t(badder_bt_norm, bad_err)
    Condition
      Error in `int_t()`:
      ! `statistics` should select a single column for the standard error.

---

    Code
      int_bca(badder_bt_norm, bad_est, .fn = get_stats)
    Condition
      Error in `int_bca()`:
      ! The tibble in `statistics` must have a column for 'estimate'.

---

    Code
      int_pctl(badder_bt_norm, bad_num)
    Condition
      Error in `pctl_single()`:
      ! `stats` must be a numeric vector.

# checks for apparent bootstrap

    Code
      int_t(rs_boot)
    Condition
      Error in `int_t()`:
      ! The bootstrap resamples must include an apparent sample.
      i Please set `apparent = TRUE` in the `bootstraps()` function.

---

    Code
      int_bca(rs_boot)
    Condition
      Error in `int_bca()`:
      ! The bootstrap resamples must include an apparent sample.
      i Please set `apparent = TRUE` in the `bootstraps()` function.

# checks input for statistics

    Code
      int_t(rs_boot_missing_term, stats)
    Condition
      Error in `int_t()`:
      ! The tibble in `statistics` must have a column for 'term'.

---

    Code
      int_t(rs_boot_missing_estimate, stats)
    Condition
      Error in `int_t()`:
      ! The tibble in `statistics` must have a column for 'estimate'.

---

    Code
      int_t(rs_boot_missing_std_err, stats)
    Condition
      Error in `int_t()`:
      ! `statistics` should select a single column for the standard error.

