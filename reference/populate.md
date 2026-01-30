# Add Assessment Indices

Many `rsplit` and `rset` objects do not contain indicators for the
assessment samples. `populate()` can be used to fill the slot for the
appropriate indices.

## Usage

``` r
populate(x, ...)
```

## Arguments

- x:

  A `rsplit` and `rset` object.

- ...:

  Not currently used.

## Value

An object of the same kind with the integer indices.

## Examples

``` r
set.seed(28432)
fold_rs <- vfold_cv(mtcars)

fold_rs$splits[[1]]$out_id
#> [1] NA
complement(fold_rs$splits[[1]])
#> [1]  1  9 25 27

populate(fold_rs$splits[[1]])$out_id
#> [1]  1  9 25 27

fold_rs_all <- populate(fold_rs)
fold_rs_all$splits[[1]]$out_id
#> [1]  1  9 25 27
```
