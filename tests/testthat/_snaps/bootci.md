# Upper & lower confidence interval does not contain NA

    Code
      int_pctl(bt_resamples, res)
    Warning <simpleWarning>
      Recommend at least 1000 non-missing bootstrap resamples for term `mean`.
    Error <simpleError>
      All statistics have missing values..

---

    Code
      int_t(bt_resamples, res)
    Warning <simpleWarning>
      Recommend at least 500 non-missing bootstrap resamples for term `mean`.
    Error <simpleError>
      All statistics have missing values.

---

    Code
      int_bca(bt_resamples, res, .fn = bad_stats)
    Warning <simpleWarning>
      Recommend at least 1000 non-missing bootstrap resamples for term `mean`.
    Error <simpleError>
      All statistics have missing values.

# regression intervals

    Code
      set.seed(1)
      int_2 <- reg_intervals(mpg ~ disp + wt, data = mtcars, filter = term == "wt",
      model_fn = "glm", keep_reps = TRUE)
      int_2
    Output
      # A tibble: 1 x 7
        term  .lower .estimate .upper .alpha .method          .replicates
        <chr>  <dbl>     <dbl>  <dbl>  <dbl> <chr>     <list<tibble[,2]>>
      1 wt     -5.49     -3.45 -0.797   0.05 student-t        [1,001 x 2]

