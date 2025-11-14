# Bootstrap Sampling

A bootstrap sample is a sample that is the same size as the original
data set that is made using replacement. This results in analysis
samples that have multiple replicates of some of the original rows of
the data. The assessment set is defined as the rows of the original data
that were not included in the bootstrap sample. This is often referred
to as the "out-of-bag" (OOB) sample.

## Usage

``` r
bootstraps(
  data,
  times = 25,
  strata = NULL,
  breaks = 4,
  pool = 0.1,
  apparent = FALSE,
  ...
)
```

## Arguments

- data:

  A data frame.

- times:

  The number of bootstrap samples.

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

- apparent:

  A logical. Should an extra resample be added where the analysis and
  holdout subset are the entire data set. This is required for some
  estimators used by the
  [`summary()`](https://rdrr.io/r/base/summary.html) function that
  require the apparent error rate.

- ...:

  These dots are for future extensions and must be empty.

## Value

A tibble with classes `bootstraps`, `rset`, `tbl_df`, `tbl`, and
`data.frame`. The results include a column for the data split objects
and a column called `id` that has a character string with the resample
identifier.

## Details

The argument `apparent` enables the option of an additional "resample"
where the analysis and assessment data sets are the same as the original
data set. This can be required for some types of analysis of the
bootstrap results.

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
bootstraps(mtcars, times = 2)
#> # Bootstrap sampling 
#> # A tibble: 2 × 2
#>   splits          id        
#>   <list>          <chr>     
#> 1 <split [32/11]> Bootstrap1
#> 2 <split [32/14]> Bootstrap2
bootstraps(mtcars, times = 2, apparent = TRUE)
#> # Bootstrap sampling with apparent sample 
#> # A tibble: 3 × 2
#>   splits          id        
#>   <list>          <chr>     
#> 1 <split [32/12]> Bootstrap1
#> 2 <split [32/14]> Bootstrap2
#> 3 <split [32/32]> Apparent  

library(purrr)
library(modeldata)
#> 
#> Attaching package: ‘modeldata’
#> The following object is masked from ‘package:datasets’:
#> 
#>     penguins
data(wa_churn)

set.seed(13)
resample1 <- bootstraps(wa_churn, times = 3)
map_dbl(
  resample1$splits,
  function(x) {
    dat <- as.data.frame(x)$churn
    mean(dat == "Yes")
  }
)
#> [1] 0.2798523 0.2639500 0.2648019

set.seed(13)
resample2 <- bootstraps(wa_churn, strata = churn, times = 3)
map_dbl(
  resample2$splits,
  function(x) {
    dat <- as.data.frame(x)$churn
    mean(dat == "Yes")
  }
)
#> [1] 0.2653699 0.2653699 0.2653699

set.seed(13)
resample3 <- bootstraps(wa_churn, strata = tenure, breaks = 6, times = 3)
map_dbl(
  resample3$splits,
  function(x) {
    dat <- as.data.frame(x)$churn
    mean(dat == "Yes")
  }
)
#> [1] 0.2625302 0.2659378 0.2696294
```
