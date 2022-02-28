# default time param with lag

    Code
      initial_time_split(drinks, lag = 12.5)
    Error <simpleError>
      `lag` must be a whole number.

---

    Code
      initial_time_split(drinks, lag = 500)
    Error <simpleError>
      `lag` must be less than or equal to the number of training observations.

