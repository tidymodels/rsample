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
    Error <simpleError>
      `lag` must be a whole number.

---

    Code
      validation_time_split(drinks, lag = 500)
    Error <simpleError>
      `lag` must be less than or equal to the number of training observations.

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

