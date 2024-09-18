# Upper & lower confidence interval does not contain NA

    Code
      int_pctl(bt_resamples, res)
    Condition
      Warning:
      Recommend at least 1000 non-missing bootstrap resamples for term `mean`.
      Error in `pctl_single()`:
      ! All statistics have missing values.

---

    Code
      int_t(bt_resamples, res)
    Condition
      Warning:
      Recommend at least 500 non-missing bootstrap resamples for term `mean`.
      Error in `t_single()`:
      ! All statistics have missing values.

---

    Code
      int_bca(bt_resamples, res, .fn = bad_stats)
    Condition
      Warning:
      Recommend at least 1000 non-missing bootstrap resamples for term `mean`.
      Error in `bca_calc()`:
      ! All statistics have missing values.

