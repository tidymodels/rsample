# `initial_time_split()` error messages

    Code
      initial_time_split(drinks, prop = 2)
    Condition
      Error in `initial_time_split()`:
      ! `prop` must be a number on (0, 1).

---

    Code
      initial_time_split(drinks, lag = 12.5)
    Condition
      Error in `initial_time_split()`:
      ! `lag` must be a whole number.

---

    Code
      initial_time_split(drinks, lag = nrow(drinks) + 1)
    Condition
      Error in `initial_time_split()`:
      ! `lag` must be less than or equal to the number of training observations.

# printing initial split objects

    Code
      initial_split(mtcars)
    Output
      <Training/Testing/Total>
      <24/8/32>

---

    Code
      initial_time_split(mtcars)
    Output
      <Training/Testing/Total>
      <24/8/32>

