# nested cv

    Code
      labels(nested_cv(mtcars, outside = vfold_cv(v = 3), inside = bootstraps(times = 5)))
    Error <rlang_error>
      `labels` not implemented for nested resampling

