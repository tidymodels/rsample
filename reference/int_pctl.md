# Bootstrap confidence intervals

Calculate bootstrap confidence intervals using various methods.

## Usage

``` r
int_pctl(.data, ...)

# Default S3 method
int_pctl(.data, ...)

# S3 method for class 'bootstraps'
int_pctl(.data, statistics, alpha = 0.05, ...)

int_t(.data, ...)

# Default S3 method
int_t(.data, ...)

# S3 method for class 'bootstraps'
int_t(.data, statistics, alpha = 0.05, ...)

int_bca(.data, ...)

# Default S3 method
int_bca(.data, ...)

# S3 method for class 'bootstraps'
int_bca(.data, statistics, alpha = 0.05, .fn, ...)
```

## Arguments

- .data:

  A object containing the bootstrap resamples, created using
  [`bootstraps()`](https://rsample.tidymodels.org/reference/bootstraps.md).
  For t- and BCa-intervals, the `apparent` argument should be set to
  `TRUE`. Even if the `apparent` argument is set to `TRUE` for the
  percentile method, the apparent data is never used in calculating the
  percentile confidence interval.

- ...:

  Arguments to pass to `.fn` (`int_bca()` only).

- statistics:

  An unquoted column name or `dplyr` selector that identifies a single
  column in the data set containing the individual bootstrap estimates.
  This must be a list column of tidy tibbles (with columns `term` and
  `estimate`). Optionally, users can include columns whose names begin
  with a period and the intervals will be created for each combination
  of these variables and the `term` column. For t-intervals, a standard
  tidy column (usually called `std.error`) is required. See the examples
  below.

- alpha:

  Level of significance.

- .fn:

  A function to calculate statistic of interest. The function should
  take an `rsplit` as the first argument and the `...` are required.

## Value

Each function returns a tibble with columns `.lower`, `.estimate`,
`.upper`, `.alpha`, `.method`, and `term`. `.method` is the type of
interval (eg. "percentile", "student-t", or "BCa"). `term` is the name
of the estimate. Note the `.estimate` returned from `int_pctl()` is the
mean of the estimates from the bootstrap resamples and not the estimate
from the apparent model.

## Details

Percentile intervals are the standard method of obtaining confidence
intervals but require thousands of resamples to be accurate. T-intervals
may need fewer resamples but require a corresponding variance estimate.
Bias-corrected and accelerated intervals require the original function
that was used to create the statistics of interest and are
computationally taxing.

## References

<https://rsample.tidymodels.org/articles/Applications/Intervals.html>

Davison, A., & Hinkley, D. (1997). *Bootstrap Methods and their
Application*. Cambridge: Cambridge University Press.
doi:10.1017/CBO9780511802843

## See also

[`reg_intervals()`](https://rsample.tidymodels.org/reference/reg_intervals.md)

## Examples

``` r
# \donttest{
library(broom)
library(dplyr)
library(purrr)
library(tibble)
library(tidyr)

# ------------------------------------------------------------------------------

lm_est <- function(split, ...) {
  lm(mpg ~ disp + hp, data = analysis(split)) |>
    tidy()
}

set.seed(52156)
car_rs <-
  bootstraps(mtcars, 500, apparent = TRUE) |>
  mutate(results = map(splits, lm_est))

int_pctl(car_rs, results)
#> Warning: Recommend at least 1000 non-missing bootstrap resamples for terms
#> `(Intercept)`, `disp`, and `hp`.
#> # A tibble: 3 × 6
#>   term         .lower .estimate   .upper .alpha .method   
#>   <chr>         <dbl>     <dbl>    <dbl>  <dbl> <chr>     
#> 1 (Intercept) 27.5      30.7    33.6       0.05 percentile
#> 2 disp        -0.0440   -0.0300 -0.0162    0.05 percentile
#> 3 hp          -0.0572   -0.0260 -0.00840   0.05 percentile
int_t(car_rs, results)
#> # A tibble: 3 × 6
#>   term         .lower .estimate   .upper .alpha .method  
#>   <chr>         <dbl>     <dbl>    <dbl>  <dbl> <chr>    
#> 1 (Intercept) 28.1      30.7    34.6       0.05 student-t
#> 2 disp        -0.0446   -0.0300 -0.0170    0.05 student-t
#> 3 hp          -0.0449   -0.0260 -0.00337   0.05 student-t
int_bca(car_rs, results, .fn = lm_est)
#> Warning: Recommend at least 1000 non-missing bootstrap resamples for terms
#> `(Intercept)`, `disp`, and `hp`.
#> # A tibble: 3 × 6
#>   term         .lower .estimate   .upper .alpha .method
#>   <chr>         <dbl>     <dbl>    <dbl>  <dbl> <chr>  
#> 1 (Intercept) 27.7      30.7    33.7       0.05 BCa    
#> 2 disp        -0.0446   -0.0300 -0.0172    0.05 BCa    
#> 3 hp          -0.0576   -0.0260 -0.00843   0.05 BCa    

# ------------------------------------------------------------------------------

# putting results into a tidy format
rank_corr <- function(split) {
  dat <- analysis(split)
  tibble(
    term = "corr",
    estimate = cor(dat$sqft, dat$price, method = "spearman"),
    # don't know the analytical std.error so no t-intervals
    std.error = NA_real_
  )
}

set.seed(69325)
data(Sacramento, package = "modeldata")
bootstraps(Sacramento, 1000, apparent = TRUE) |>
  mutate(correlations = map(splits, rank_corr)) |>
  int_pctl(correlations)
#> # A tibble: 1 × 6
#>   term  .lower .estimate .upper .alpha .method   
#>   <chr>  <dbl>     <dbl>  <dbl>  <dbl> <chr>     
#> 1 corr   0.737     0.768  0.796   0.05 percentile

# ------------------------------------------------------------------------------
# An example of computing the interval for each value of a custom grouping
# factor (type of house in this example)

# Get regression estimates for each house type
lm_est <- function(split, ...) {
  analysis(split) |>
    tidyr::nest(.by = c(type)) |>
    # Compute regression estimates for each house type
    mutate(
      betas = purrr::map(data, \(.x) lm(log10(price) ~ sqft, data = .x) |> tidy())
    ) |>
    # Convert the column name to begin with a period
    rename(.type = type) |>
    select(.type, betas) |>
    unnest(cols = betas)
}

set.seed(52156)
house_rs <-
  bootstraps(Sacramento, 1000, apparent = TRUE) |>
  mutate(results = map(splits, lm_est))

int_pctl(house_rs, results)
#> # A tibble: 6 × 7
#>   term        .type           .lower .estimate   .upper .alpha .method 
#>   <chr>       <fct>            <dbl>     <dbl>    <dbl>  <dbl> <chr>   
#> 1 (Intercept) Condo         4.45     4.59      4.72       0.05 percent…
#> 2 (Intercept) Multi_Family  4.74     5.25      5.71       0.05 percent…
#> 3 (Intercept) Residential   4.93     4.96      4.99       0.05 percent…
#> 4 sqft        Condo         0.000412 0.000520  0.000659   0.05 percent…
#> 5 sqft        Multi_Family -0.000197 0.0000344 0.000277   0.05 percent…
#> 6 sqft        Residential   0.000211 0.000225  0.000240   0.05 percent…
# }
```
