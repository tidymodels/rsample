# nested cv

    Code
      labels(nested_cv(mtcars, outside = vfold_cv(v = 3), inside = bootstraps(times = 5)))
    Condition
      Error in `labels()`:
      ! `labels` not implemented for nested resampling

