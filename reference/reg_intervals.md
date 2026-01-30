# A convenience function for confidence intervals with linear-ish parametric models

A convenience function for confidence intervals with linear-ish
parametric models

## Usage

``` r
reg_intervals(
  formula,
  data,
  model_fn = "lm",
  type = "student-t",
  times = NULL,
  alpha = 0.05,
  filter = term != "(Intercept)",
  keep_reps = FALSE,
  ...
)
```

## Arguments

- formula:

  An R model formula with one outcome and at least one predictor.

- data:

  A data frame.

- model_fn:

  The model to fit. Allowable values are `"lm"`, `"glm"`, `"survreg"`,
  and `"coxph"`. The latter two require that the survival package be
  installed.

- type:

  The type of bootstrap confidence interval. Values of `"student-t"` and
  `"percentile"` are allowed.

- times:

  A single integer for the number of bootstrap samples. If left `NULL`,
  1,001 are used for t-intervals and 2,001 for percentile intervals.

- alpha:

  Level of significance.

- filter:

  A logical expression used to remove rows from the final result, or
  `NULL` to keep all rows.

- keep_reps:

  Should the individual parameter estimates for each bootstrap sample be
  retained?

- ...:

  Options to pass to the model function (such as `family` for
  [`stats::glm()`](https://rdrr.io/r/stats/glm.html)).

## Value

A tibble with columns "term", ".lower", ".estimate", ".upper", ".alpha",
and ".method". If `keep_reps = TRUE`, an additional list column called
".replicates" is also returned.

## References

Davison, A., & Hinkley, D. (1997). *Bootstrap Methods and their
Application*. Cambridge: Cambridge University Press.
doi:10.1017/CBO9780511802843

*Bootstrap Confidence Intervals*,
<https://rsample.tidymodels.org/articles/Applications/Intervals.html>

## See also

[`int_pctl()`](https://rsample.tidymodels.org/reference/int_pctl.md),
[`int_t()`](https://rsample.tidymodels.org/reference/int_pctl.md)

## Examples

``` r
# \donttest{
set.seed(1)
reg_intervals(mpg ~ I(1 / sqrt(disp)), data = mtcars)
#> # A tibble: 1 × 6
#>   term            .lower .estimate .upper .alpha .method  
#>   <chr>            <dbl>     <dbl>  <dbl>  <dbl> <chr>    
#> 1 I(1/sqrt(disp))   207.      249.   290.   0.05 student-t

set.seed(1)
reg_intervals(mpg ~ I(1 / sqrt(disp)), data = mtcars, keep_reps = TRUE)
#> # A tibble: 1 × 7
#>   term            .lower .estimate .upper .alpha .method    .replicates
#>   <chr>            <dbl>     <dbl>  <dbl>  <dbl> <chr>     <list<tibbl>
#> 1 I(1/sqrt(disp))   207.      249.   290.   0.05 student-t  [1,001 × 2]
# }
```
