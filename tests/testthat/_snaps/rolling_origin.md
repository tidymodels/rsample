# lag

    Code
      rolling_origin(drinks, initial = 5, lag = 6)
    Condition
      Error in `rolling_origin()`:
      ! `lag` must be less than or equal to the number of training observations.

---

    Code
      rolling_origin(drinks, lag = 2.1)
    Condition
      Error in `rolling_origin()`:
      ! `lag` must be a whole number.

