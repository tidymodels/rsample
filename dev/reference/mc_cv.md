# Monte Carlo Cross-Validation

One resample of Monte Carlo cross-validation takes a random sample
(without replacement) of the original data set to be used for analysis.
All other data points are added to the assessment set.

## Usage

``` r
mc_cv(data, prop = 3/4, times = 25, strata = NULL, breaks = 4, pool = 0.1, ...)
```

## Arguments

- data:

  A data frame.

- prop:

  The proportion of data to be retained for modeling/analysis.

- times:

  The number of times to repeat the sampling.

- strata:

  A variable in `data` (single character or name) used to conduct
  stratified sampling. When not `NULL`, each resample is created within
  the stratification variable. Numeric `strata` are binned into
  quartiles.

- breaks:

  A single number giving the number of bins desired to stratify a
  numeric stratification variable.

- pool:

  A proportion of data used to determine if a particular group is too
  small and should be pooled into another group. We do not recommend
  decreasing this argument below its default of 0.1 because of the
  dangers of stratifying groups that are too small.

- ...:

  These dots are for future extensions and must be empty.

## Value

An tibble with classes `mc_cv`, `rset`, `tbl_df`, `tbl`, and
`data.frame`. The results include a column for the data split objects
and a column called `id` that has a character string with the resample
identifier.

## Details

With a `strata` argument, the random sampling is conducted *within the
stratification variable*. This can help ensure that the resamples have
equivalent proportions as the original data set. For a categorical
variable, sampling is conducted separately within each class. For a
numeric stratification variable, `strata` is binned into quartiles,
which are then used to stratify. Strata below 10% of the total are
pooled together; see
[`make_strata()`](https://rsample.tidymodels.org/dev/reference/make_strata.md)
for more details.

## Examples

``` r
mc_cv(mtcars, times = 2)
#> # Monte Carlo cross-validation (0.75/0.25) with 2 resamples 
#> # A tibble: 2 × 2
#>   splits         id       
#>   <list>         <chr>    
#> 1 <split [24/8]> Resample1
#> 2 <split [24/8]> Resample2
mc_cv(mtcars, prop = .5, times = 2)
#> # Monte Carlo cross-validation (0.5/0.5) with 2 resamples 
#> # A tibble: 2 × 2
#>   splits          id       
#>   <list>          <chr>    
#> 1 <split [16/16]> Resample1
#> 2 <split [16/16]> Resample2

library(purrr)
data(wa_churn, package = "modeldata")

set.seed(13)
resample1 <- mc_cv(wa_churn, times = 3, prop = .5)
map_dbl(
  resample1$splits,
  function(x) {
    dat <- as.data.frame(x)$churn
    mean(dat == "Yes")
  }
)
#> [1] 0.2709458 0.2621414 0.2632775

set.seed(13)
resample2 <- mc_cv(wa_churn, strata = churn, times = 3, prop = .5)
map_dbl(
  resample2$splits,
  function(x) {
    dat <- as.data.frame(x)$churn
    mean(dat == "Yes")
  }
)
#> [1] 0.2652655 0.2652655 0.2652655

set.seed(13)
resample3 <- mc_cv(wa_churn, strata = tenure, breaks = 6, times = 3, prop = .5)
map_dbl(
  resample3$splits,
  function(x) {
    dat <- as.data.frame(x)$churn
    mean(dat == "Yes")
  }
)
#> [1] 0.2636364 0.2599432 0.2576705
```
