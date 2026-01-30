# Create a Validation Split for Tuning

`validation_set()` creates a the validation split for model tuning.

## Usage

``` r
validation_set(split, ...)

# S3 method for class 'val_split'
analysis(x, ...)

# S3 method for class 'val_split'
assessment(x, ...)

# S3 method for class 'val_split'
training(x, ...)

# S3 method for class 'val_split'
validation(x, ...)

# S3 method for class 'val_split'
testing(x, ...)
```

## Arguments

- split:

  An object of class `initial_validation_split`, such as resulting from
  [`initial_validation_split()`](https://rsample.tidymodels.org/reference/initial_validation_split.md)
  or
  [`group_initial_validation_split()`](https://rsample.tidymodels.org/reference/initial_validation_split.md).

- ...:

  These dots are for future extensions and must be empty.

- x:

  An `rsplit` object produced by `validation_set()`.

## Value

An tibble with classes `validation_set`, `rset`, `tbl_df`, `tbl`, and
`data.frame`. The results include a column for the data split object and
a column called `id` that has a character string with the resample
identifier.

## Examples

``` r
set.seed(1353)
car_split <- initial_validation_split(mtcars)
car_set <- validation_set(car_split)
```
