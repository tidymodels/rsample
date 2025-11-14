# Group Bootstraps

Group bootstrapping creates splits of the data based on some grouping
variable (which may have more than a single row associated with it). A
common use of this kind of resampling is when you have repeated measures
of the same subject. A bootstrap sample is a sample that is the same
size as the original data set that is made using replacement. This
results in analysis samples that have multiple replicates of some of the
original rows of the data. The assessment set is defined as the rows of
the original data that were not included in the bootstrap sample. This
is often referred to as the "out-of-bag" (OOB) sample.

## Usage

``` r
group_bootstraps(
  data,
  group,
  times = 25,
  apparent = FALSE,
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

- times:

  The number of bootstrap samples.

- apparent:

  A logical. Should an extra resample be added where the analysis and
  holdout subset are the entire data set. This is required for some
  estimators used by the
  [`summary()`](https://rdrr.io/r/base/summary.html) function that
  require the apparent error rate.

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

An tibble with classes `group_bootstraps` `bootstraps`, `rset`,
`tbl_df`, `tbl`, and `data.frame`. The results include a column for the
data split objects and a column called `id` that has a character string
with the resample identifier.

## Details

The argument `apparent` enables the option of an additional "resample"
where the analysis and assessment data sets are the same as the original
data set. This can be required for some types of analysis of the
bootstrap results.

## Examples

``` r
data(ames, package = "modeldata")

set.seed(13)
group_bootstraps(ames, Neighborhood, times = 3)
#> # Group bootstrap sampling 
#> # A tibble: 3 × 2
#>   splits              id        
#>   <list>              <chr>     
#> 1 <split [2959/1072]> Bootstrap1
#> 2 <split [2899/1334]> Bootstrap2
#> 3 <split [2937/1203]> Bootstrap3
group_bootstraps(ames, Neighborhood, times = 3, apparent = TRUE)
#> # Group bootstrap sampling with apparent sample 
#> # A tibble: 4 × 2
#>   splits              id        
#>   <list>              <chr>     
#> 1 <split [2969/1196]> Bootstrap1
#> 2 <split [2931/983]>  Bootstrap2
#> 3 <split [2896/1208]> Bootstrap3
#> 4 <split [2930/2930]> Apparent  
```
