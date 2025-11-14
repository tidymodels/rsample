# Convert an `rsplit` object to a data frame

The analysis or assessment code can be returned as a data frame (as
dictated by the `data` argument) using `as.data.frame.rsplit()`.
`analysis()` and `assessment()` are shortcuts.

## Usage

``` r
# S3 method for class 'rsplit'
as.data.frame(x, row.names = NULL, optional = FALSE, data = "analysis", ...)

analysis(x, ...)

# Default S3 method
analysis(x, ...)

# S3 method for class 'rsplit'
analysis(x, ...)

assessment(x, ...)

# Default S3 method
assessment(x, ...)

# S3 method for class 'rsplit'
assessment(x, ...)
```

## Arguments

- x:

  An `rsplit` object.

- row.names:

  `NULL` or a character vector giving the row names for the data frame.
  Missing values are not allowed.

- optional:

  A logical: should the column names of the data be checked for
  legality?

- data:

  Either `"analysis"` or `"assessment"` to specify which data are
  returned.

- ...:

  Not currently used.

## Examples

``` r
library(dplyr)
set.seed(104)
folds <- vfold_cv(mtcars)

model_data_1 <- folds$splits[[1]] |> analysis()
holdout_data_1 <- folds$splits[[1]] |> assessment()
```
