# regression intervals

    Code
      skip_if(new_rng_snapshots)
      set.seed(123)
      int_2 <- reg_intervals(mpg ~ disp + wt, data = mtcars, filter = term == "wt",
      model_fn = "glm", keep_reps = TRUE)
      int_2
    Output
      # A tibble: 1 x 7
        term  .lower .estimate .upper .alpha .method          .replicates
        <chr>  <dbl>     <dbl>  <dbl>  <dbl> <chr>     <list<tibble[,2]>>
      1 wt     -5.62     -3.46 -0.955   0.05 student-t        [1,001 x 2]

---

    Code
      reg_intervals(mpg ~ disp + wt, data = mtcars, model_fn = "potato")
    Condition
      Error in `reg_intervals()`:
      ! `model_fn` must be one of "lm", "glm", "survreg", or "coxph", not "potato".

---

    Code
      reg_intervals(mpg ~ disp + wt, data = mtcars, type = "random")
    Condition
      Error in `reg_intervals()`:
      ! `type` must be one of "student-t" or "percentile", not "random".

---

    Code
      reg_intervals(mpg ~ disp + wt, data = mtcars, alpha = "a")
    Condition
      Error in `reg_intervals()`:
      ! `alpha` must be a single numeric value.

