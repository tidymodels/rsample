# Find Labels from rset Object

Produce a vector of resampling labels (e.g. "Fold1") from an `rset`
object. Currently,
[`nested_cv()`](https://rsample.tidymodels.org/reference/nested_cv.md)
is not supported.

## Usage

``` r
# S3 method for class 'rset'
labels(object, make_factor = FALSE, ...)

# S3 method for class 'vfold_cv'
labels(object, make_factor = FALSE, ...)
```

## Arguments

- object:

  An `rset` object.

- make_factor:

  A logical for whether the results should be a character or a factor.

- ...:

  Not currently used.

## Value

A single character or factor vector.

## Examples

``` r
labels(vfold_cv(mtcars))
#>  [1] "Fold01" "Fold02" "Fold03" "Fold04" "Fold05" "Fold06" "Fold07"
#>  [8] "Fold08" "Fold09" "Fold10"
```
