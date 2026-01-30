# Leave-One-Out Cross-Validation

Leave-one-out (LOO) cross-validation uses one data point in the original
set as the assessment data and all other data points as the analysis
set. A LOO resampling set has as many resamples as rows in the original
data set.

## Usage

``` r
loo_cv(data, ...)
```

## Arguments

- data:

  A data frame.

- ...:

  These dots are for future extensions and must be empty.

## Value

An tibble with classes `loo_cv`, `rset`, `tbl_df`, `tbl`, and
`data.frame`. The results include a column for the data split objects
and one column called `id` that has a character string with the resample
identifier.

## Examples

``` r
loo_cv(mtcars)
#> # Leave-one-out cross-validation 
#> # A tibble: 32 × 2
#>    splits         id        
#>    <list>         <chr>     
#>  1 <split [31/1]> Resample1 
#>  2 <split [31/1]> Resample2 
#>  3 <split [31/1]> Resample3 
#>  4 <split [31/1]> Resample4 
#>  5 <split [31/1]> Resample5 
#>  6 <split [31/1]> Resample6 
#>  7 <split [31/1]> Resample7 
#>  8 <split [31/1]> Resample8 
#>  9 <split [31/1]> Resample9 
#> 10 <split [31/1]> Resample10
#> # ℹ 22 more rows
```
