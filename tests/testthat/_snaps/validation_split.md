# `validation_split()` is deprecated

    Code
      set.seed(11)
      rs1 <- validation_split(dat1)
    Condition
      Warning:
      `validation_split()` was deprecated in rsample 1.2.0.
      i Please use `initial_validation_split()` instead.
    Code
      sizes1 <- dim_rset(rs1)
      expect_true(all(sizes1$analysis == 15))
      expect_true(all(sizes1$assessment == 5))

# `validation_time_split()` is deprecated

    Code
      set.seed(11)
      rs1 <- validation_time_split(dat1)
    Condition
      Warning:
      `validation_time_split()` was deprecated in rsample 1.2.0.
      i Please use `initial_validation_time_split()` instead.
    Code
      sizes1 <- dim_rset(rs1)
      expect_true(all(sizes1$analysis == 15))
      expect_true(all(sizes1$assessment == 5))

# default time param with lag

    Code
      validation_time_split(drinks)
    Output
      # Validation Set Split (0.75/0.25)  
      # A tibble: 1 x 2
        splits           id        
        <list>           <chr>     
      1 <split [231/78]> validation

---

    Code
      validation_time_split(drinks, lag = 12.5)
    Condition
      Error in `validation_time_split()`:
      ! `lag` must be a whole number.

---

    Code
      validation_time_split(drinks, lag = 500)
    Condition
      Error in `validation_time_split()`:
      ! `lag` must be less than or equal to the number of training observations.

# `group_validation_split()` is deprecated

    Code
      set.seed(11)
      rs1 <- group_validation_split(dat1, c)
    Condition
      Warning:
      `group_validation_split()` was deprecated in rsample 1.2.0.
      i Please use `group_initial_validation_split()` instead.
    Code
      sizes1 <- dim_rset(rs1)
      expect_true(all(sizes1$analysis == 15))
      expect_true(all(sizes1$assessment == 5))

# grouping -- strata

    Code
      sizes4
    Output
      # A tibble: 1 x 5
        analysis assessment     n     p id        
           <int>      <int> <int> <int> <chr>     
      1    37074      12926 50000     3 validation

# bad args

    Code
      validation_split(warpbreaks, strata = warpbreaks$tension)
    Condition
      Error in `validation_split()`:
      ! Can't select columns that don't exist.
      x Columns `L`, `L`, `L`, `L`, `L`, etc. don't exist.

---

    Code
      validation_split(warpbreaks, strata = c("tension", "wool"))
    Condition
      Error in `strata_check()`:
      ! `strata` should be a single name or character value.

# printing

    Code
      validation_split(warpbreaks)
    Output
      # Validation Set Split (0.75/0.25)  
      # A tibble: 1 x 2
        splits          id        
        <list>          <chr>     
      1 <split [40/14]> validation

---

    Code
      validation_split(warpbreaks)$splits[[1]]
    Output
      <Training/Validation/Total>
      <40/14/54>

