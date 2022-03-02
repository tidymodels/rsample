# bad data

    Code
      s1 <- make_strata(x3, pool = 0.06)
    Warning <rlang_warning>
      Stratifying groups that make up 6% of the data may be statistically risky.
      * Consider increasing `pool` to at least 0.1

---

    Code
      s2 <- make_strata(mtcars$mpg)
    Warning <rlang_warning>
      The number of observations in each quantile is below the recommended threshold of 20.
      * Stratification will use 1 breaks instead.
      Too little data to stratify.
      * Resampling will be unstratified.

---

    Code
      s3 <- make_strata(seq_len(50), breaks = -1)
    Warning <rlang_warning>
      The bins specified by `breaks` must be >=2.
      * Resampling will be unstratified.

