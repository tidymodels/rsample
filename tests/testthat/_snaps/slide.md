# `data` is validated

    Code
      sliding_window(1)
    Condition
      Error in `sliding_window()`:
      ! `data` must be a data frame.

---

    Code
      sliding_index(1)
    Condition
      Error in `sliding_index()`:
      ! `data` must be a data frame.

---

    Code
      sliding_period(1)
    Condition
      Error in `sliding_period()`:
      ! `data` must be a data frame.

# `lookback` is validated

    Code
      sliding_window(data.frame(), lookback = -1)
    Condition
      Error in `check_lookback()`:
      ! `lookback` must be positive, or zero.

---

    Code
      sliding_window(data.frame(), lookback = "a")
    Condition
      Error in `check_lookback()`:
      ! `lookback` must be an integer of size 1, or `Inf`.

---

    Code
      sliding_window(data.frame(), lookback = c(1, 2))
    Condition
      Error in `check_lookback()`:
      ! `lookback` must have size 1.

---

    Code
      sliding_window(data.frame(), lookback = NA)
    Condition
      Error in `check_lookback()`:
      ! `lookback` must be an integer of size 1, or `Inf`.

# `assess_start` is validated

    Code
      sliding_window(data.frame(), assess_start = -1)
    Condition
      Error in `check_assess()`:
      ! `assess_start` must be positive.

---

    Code
      sliding_window(data.frame(), assess_start = "a")
    Condition
      Error in `check_assess()`:
      ! `assess_start` must be an integer of size 1, or `Inf`.

---

    Code
      sliding_window(data.frame(), assess_start = c(1, 2))
    Condition
      Error in `check_assess()`:
      ! `assess_start` must have size 1.

---

    Code
      sliding_window(data.frame(), assess_start = NA)
    Condition
      Error in `check_assess()`:
      ! `assess_start` must be an integer of size 1, or `Inf`.

# `assess_stop` is validated

    Code
      sliding_window(data.frame(), assess_stop = -1)
    Condition
      Error in `check_assess()`:
      ! `assess_stop` must be positive.

---

    Code
      sliding_window(data.frame(), assess_stop = "a")
    Condition
      Error in `check_assess()`:
      ! `assess_stop` must be an integer of size 1, or `Inf`.

---

    Code
      sliding_window(data.frame(), assess_stop = c(1, 2))
    Condition
      Error in `check_assess()`:
      ! `assess_stop` must have size 1.

---

    Code
      sliding_window(data.frame(), assess_stop = NA)
    Condition
      Error in `check_assess()`:
      ! `assess_stop` must be an integer of size 1, or `Inf`.

# `assess_start` must be before or equal to `assess_stop`

    Code
      sliding_window(data.frame(), assess_start = 2, assess_stop = 1)
    Condition
      Error in `sliding_window()`:
      ! `assess_start` must be less than or equal to `assess_stop`.

# `index` is validated

    Code
      sliding_index(df, y)
    Condition
      Error in `sliding_index()`:
      ! Can't select columns that don't exist.
      x Column `y` doesn't exist.

---

    Code
      sliding_period(df, y)
    Condition
      Error in `sliding_period()`:
      ! Can't select columns that don't exist.
      x Column `y` doesn't exist.

