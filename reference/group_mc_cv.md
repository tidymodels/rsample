# Group Monte Carlo Cross-Validation

Group Monte Carlo cross-validation creates splits of the data based on
some grouping variable (which may have more than a single row associated
with it). One resample of Monte Carlo cross-validation takes a random
sample (without replacement) of groups in the original data set to be
used for analysis. All other data points are added to the assessment
set. A common use of this kind of resampling is when you have repeated
measures of the same subject.

## Usage

``` r
group_mc_cv(
  data,
  group,
  prop = 3/4,
  times = 25,
  ...,
  strata = NULL,
  pool = 0.1
)
```

## Arguments

- data:

  A data frame.

- group:

  A variable in `data` (single character or name) used for grouping
  observations with the same value to either the analysis or assessment
  set within a fold.

- prop:

  The proportion of data to be retained for modeling/analysis.

- times:

  The number of times to repeat the sampling.

- ...:

  These dots are for future extensions and must be empty.

- strata:

  A variable in `data` (single character or name) used to conduct
  stratified sampling. When not `NULL`, each resample is created within
  the stratification variable. Numeric `strata` are binned into
  quartiles.

- pool:

  A proportion of data used to determine if a particular group is too
  small and should be pooled into another group. We do not recommend
  decreasing this argument below its default of 0.1 because of the
  dangers of stratifying groups that are too small.

## Value

A tibble with classes `group_mc_cv`, `rset`, `tbl_df`, `tbl`, and
`data.frame`. The results include a column for the data split objects
and an identification variable.

## Examples

``` r
data(ames, package = "modeldata")

set.seed(123)
group_mc_cv(ames, group = Neighborhood, times = 5)
#> # Group Monte Carlo cross-validation (0.75/0.25) with 5 resamples  
#> # A tibble: 5 × 2
#>   splits             id       
#>   <list>             <chr>    
#> 1 <split [2395/535]> Resample1
#> 2 <split [2236/694]> Resample2
#> 3 <split [2168/762]> Resample3
#> 4 <split [2331/599]> Resample4
#> 5 <split [2115/815]> Resample5
```
