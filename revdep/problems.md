# trendeval

<details>

* Version: 0.1.0
* GitHub: https://github.com/reconverse/trendeval
* Source code: https://github.com/cran/trendeval
* Date/Publication: 2023-04-11 20:30:02 UTC
* Number of recursive dependencies: 70

Run `revdepcheck::cloud_details(, "trendeval")` for more info

</details>

## Newly broken

*   checking examples ... ERROR
    ```
    Running examples in ‘trendeval-Ex.R’ failed
    The error most likely occurred in:
    
    > ### Name: evaluate_resampling
    > ### Title: Resampling approach for model evaluation
    > ### Aliases: evaluate_resampling evaluate_resampling.default
    > ###   evaluate_resampling.trending_model evaluate_resampling.list
    > 
    > ### ** Examples
    > 
    ...
    Backtrace:
        ▆
     1. ├─trendeval::evaluate_resampling(model, dat)
     2. └─trendeval:::evaluate_resampling.trending_model(model, dat)
     3.   └─rsample::vfold_cv(data, v = v, repeats = repeats)
     4.     └─rsample:::vfold_splits(...)
     5.       └─rsample:::check_v(v, n, prevent_loo = prevent_loo, call = rlang::caller_env())
     6.         └─cli::cli_abort(...)
     7.           └─rlang::abort(...)
    Execution halted
    ```

