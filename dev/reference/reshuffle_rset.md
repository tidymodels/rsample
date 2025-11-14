# "Reshuffle" an rset to re-generate a new rset with the same parameters

This function re-generates an rset object, using the same arguments used
to generate the original.

## Usage

``` r
reshuffle_rset(rset)
```

## Arguments

- rset:

  The `rset` object to be reshuffled

## Value

An rset of the same class as `rset`.

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
reshuffle_rset(starting_splits)
#> # Group 3-fold cross-validation 
#> # A tibble: 3 × 2
#>   splits          id       
#>   <list>          <chr>    
#> 1 <split [21/11]> Resample1
#> 2 <split [25/7]>  Resample2
#> 3 <split [18/14]> Resample3
```
