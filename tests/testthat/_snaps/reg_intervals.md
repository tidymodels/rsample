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

