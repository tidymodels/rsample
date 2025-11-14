# Find Labels from rsplit Object

Produce a tibble of identification variables so that single splits can
be linked to a particular resample.

## Usage

``` r
# S3 method for class 'rsplit'
labels(object, ...)
```

## Arguments

- object:

  An `rsplit` object

- ...:

  Not currently used.

## Value

A tibble.

## See also

add_resample_id

## Examples

``` r
cv_splits <- vfold_cv(mtcars)
labels(cv_splits$splits[[1]])
#> # A tibble: 1 × 1
#>   id    
#>   <chr> 
#> 1 Fold01
```
