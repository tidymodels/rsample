# `initial_time_split()` error messages

    Code
      initial_time_split(drinks, prop = 2)
    Condition
      Error in `initial_time_split()`:
      ! `prop` must be less than 1.

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

# prop is checked

    Code
      initial_split(mtcars, prop = 1)
    Condition
      Error in `initial_split()`:
      ! `prop` must be less than 1.

---

    Code
      initial_time_split(mtcars, prop = 1)
    Condition
      Error in `initial_time_split()`:
      ! `prop` must be less than 1.

---

    Code
      group_initial_split(mtcars, group = "cyl", prop = 1)
    Condition
      Error in `group_initial_split()`:
      ! `prop` must be less than 1.

