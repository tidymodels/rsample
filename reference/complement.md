# Determine the Assessment Samples

This method and function help find which data belong in the analysis and
assessment sets.

## Usage

``` r
complement(x, ...)

# S3 method for class 'rsplit'
complement(x, ...)

# S3 method for class 'rof_split'
complement(x, ...)

# S3 method for class 'sliding_window_split'
complement(x, ...)

# S3 method for class 'sliding_index_split'
complement(x, ...)

# S3 method for class 'sliding_period_split'
complement(x, ...)

# S3 method for class 'apparent_split'
complement(x, ...)
```

## Arguments

- x:

  An `rsplit` object.

- ...:

  Not currently used.

## Value

A integer vector.

## Details

Given an `rsplit` object, `complement()` will determine which of the
data rows are contained in the assessment set. To save space, many of
the `rsplit` objects will not contain indices for the assessment split.

## See also

[`populate()`](https://rsample.tidymodels.org/reference/populate.md)

## Examples

``` r
set.seed(28432)
fold_rs <- vfold_cv(mtcars)
head(fold_rs$splits[[1]]$in_id)
#> [1] 2 3 4 5 6 7
fold_rs$splits[[1]]$out_id
#> [1] NA
complement(fold_rs$splits[[1]])
#> [1]  1  9 25 27
```
