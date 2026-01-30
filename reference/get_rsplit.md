# Retrieve individual rsplits objects from an rset

Retrieve individual rsplits objects from an rset

## Usage

``` r
get_rsplit(x, index, ...)

# S3 method for class 'rset'
get_rsplit(x, index, ...)

# Default S3 method
get_rsplit(x, index, ...)
```

## Arguments

- x:

  The `rset` object to retrieve an rsplit from.

- index:

  An integer indicating which rsplit to retrieve: `1` for the rsplit in
  the first row of the rset, `2` for the second, and so on.

- ...:

  Not currently used.

## Value

The rsplit object in row `index` of `rset`

## Examples

``` r
set.seed(123)
(starting_splits <- group_vfold_cv(mtcars, cyl, v = 3))
#> # Group 3-fold cross-validation 
#> # A tibble: 3 × 2
#>   splits          id       
#>   <list>          <chr>    
#> 1 <split [21/11]> Resample1
#> 2 <split [18/14]> Resample2
#> 3 <split [25/7]>  Resample3
get_rsplit(starting_splits, 1)
#> <Analysis/Assess/Total>
#> <21/11/32>
```
