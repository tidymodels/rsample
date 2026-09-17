# Obtain a identifier for the resamples

This function returns a hash (or NA) for an attribute that is created
when the `rset` was initially constructed. This can be used to compare
with other resampling objects to see if they are the same.

## Usage

``` r
.get_fingerprint(x, ...)

# Default S3 method
.get_fingerprint(x, ...)

# S3 method for class 'rset'
.get_fingerprint(x, ...)
```

## Arguments

- x:

  An `rset` or `tune_results` object.

- ...:

  Not currently used.

## Value

A character value or `NA_character_` if the object was created prior to
rsample version 0.1.0.

## Examples

``` r
set.seed(1)
.get_fingerprint(vfold_cv(mtcars))
#> [1] "130189fbc45e7e0b8ce35136b6391bce"

set.seed(1)
.get_fingerprint(vfold_cv(mtcars))
#> [1] "130189fbc45e7e0b8ce35136b6391bce"

set.seed(2)
.get_fingerprint(vfold_cv(mtcars))
#> [1] "173365d0b9fab05cf3511c42f06a9f46"

set.seed(1)
.get_fingerprint(vfold_cv(mtcars, repeats = 2))
#> [1] "93e490b1ec691db12fdcaf481ee463ab"
```
