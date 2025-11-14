# V-Fold Cross-Validation

V-fold cross-validation (also known as k-fold cross-validation) randomly
splits the data into V groups of roughly equal size (called "folds"). A
resample of the analysis data consists of V-1 of the folds while the
assessment set contains the final fold. In basic V-fold cross-validation
(i.e. no repeats), the number of resamples is equal to V.

## Usage

``` r
vfold_cv(data, v = 10, repeats = 1, strata = NULL, breaks = 4, pool = 0.1, ...)
```

## Arguments

- data:

  A data frame.

- v:

  The number of partitions of the data set.

- repeats:

  The number of times to repeat the V-fold partitioning.

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

A tibble with classes `vfold_cv`, `rset`, `tbl_df`, `tbl`, and
`data.frame`. The results include a column for the data split objects
and one or more identification variables. For a single repeat, there
will be one column called `id` that has a character string with the fold
identifier. For repeats, `id` is the repeat number and an additional
column called `id2` that contains the fold information (within repeat).

## Details

With more than one repeat, the basic V-fold cross-validation is
conducted each time. For example, if three repeats are used with
`v = 10`, there are a total of 30 splits: three groups of 10 that are
generated separately.

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
vfold_cv(mtcars, v = 10)
#> #  10-fold cross-validation 
#> # A tibble: 10 × 2
#>    splits         id    
#>    <list>         <chr> 
#>  1 <split [28/4]> Fold01
#>  2 <split [28/4]> Fold02
#>  3 <split [29/3]> Fold03
#>  4 <split [29/3]> Fold04
#>  5 <split [29/3]> Fold05
#>  6 <split [29/3]> Fold06
#>  7 <split [29/3]> Fold07
#>  8 <split [29/3]> Fold08
#>  9 <split [29/3]> Fold09
#> 10 <split [29/3]> Fold10
vfold_cv(mtcars, v = 10, repeats = 2)
#> #  10-fold cross-validation repeated 2 times 
#> # A tibble: 20 × 3
#>    splits         id      id2   
#>    <list>         <chr>   <chr> 
#>  1 <split [28/4]> Repeat1 Fold01
#>  2 <split [28/4]> Repeat1 Fold02
#>  3 <split [29/3]> Repeat1 Fold03
#>  4 <split [29/3]> Repeat1 Fold04
#>  5 <split [29/3]> Repeat1 Fold05
#>  6 <split [29/3]> Repeat1 Fold06
#>  7 <split [29/3]> Repeat1 Fold07
#>  8 <split [29/3]> Repeat1 Fold08
#>  9 <split [29/3]> Repeat1 Fold09
#> 10 <split [29/3]> Repeat1 Fold10
#> 11 <split [28/4]> Repeat2 Fold01
#> 12 <split [28/4]> Repeat2 Fold02
#> 13 <split [29/3]> Repeat2 Fold03
#> 14 <split [29/3]> Repeat2 Fold04
#> 15 <split [29/3]> Repeat2 Fold05
#> 16 <split [29/3]> Repeat2 Fold06
#> 17 <split [29/3]> Repeat2 Fold07
#> 18 <split [29/3]> Repeat2 Fold08
#> 19 <split [29/3]> Repeat2 Fold09
#> 20 <split [29/3]> Repeat2 Fold10

library(purrr)
data(wa_churn, package = "modeldata")

set.seed(13)
folds1 <- vfold_cv(wa_churn, v = 5)
map_dbl(
  folds1$splits,
  function(x) {
    dat <- as.data.frame(x)$churn
    mean(dat == "Yes")
  }
)
#> [1] 0.2649982 0.2660632 0.2609159 0.2679681 0.2669033

set.seed(13)
folds2 <- vfold_cv(wa_churn, strata = churn, v = 5)
map_dbl(
  folds2$splits,
  function(x) {
    dat <- as.data.frame(x)$churn
    mean(dat == "Yes")
  }
)
#> [1] 0.2653532 0.2653532 0.2653532 0.2653532 0.2654365

set.seed(13)
folds3 <- vfold_cv(wa_churn, strata = tenure, breaks = 6, v = 5)
map_dbl(
  folds3$splits,
  function(x) {
    dat <- as.data.frame(x)$churn
    mean(dat == "Yes")
  }
)
#> [1] 0.2656250 0.2661104 0.2652228 0.2638396 0.2660518
```
