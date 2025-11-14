# Reverse the analysis and assessment sets

This functions "swaps" the analysis and assessment sets of either a
single `rsplit` or all `rsplit`s in the `splits` column of an `rset`
object.

## Usage

``` r
reverse_splits(x, ...)

# Default S3 method
reverse_splits(x, ...)

# S3 method for class 'permutations'
reverse_splits(x, ...)

# S3 method for class 'perm_split'
reverse_splits(x, ...)

# S3 method for class 'rsplit'
reverse_splits(x, ...)

# S3 method for class 'rset'
reverse_splits(x, ...)
```

## Arguments

- x:

  An `rset` or `rsplit` object.

- ...:

  Not currently used.

## Value

An object of the same class as `x`

## Examples

``` r
set.seed(123)
starting_splits <- vfold_cv(mtcars, v = 3)
reverse_splits(starting_splits)
#> #  3-fold cross-validation 
#> # A tibble: 3 × 2
#>   splits          id   
#>   <list>          <chr>
#> 1 <split [11/21]> Fold1
#> 2 <split [11/21]> Fold2
#> 3 <split [10/22]> Fold3
reverse_splits(starting_splits$splits[[1]])
#> <Analysis/Assess/Total>
#> <11/21/32>
```
