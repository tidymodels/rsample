# Constructors for split objects

Constructors for split objects

## Usage

``` r
make_splits(x, ...)

# Default S3 method
make_splits(x, ...)

# S3 method for class 'list'
make_splits(x, data, class = NULL, ...)

# S3 method for class 'data.frame'
make_splits(x, assessment, class = NULL, ...)
```

## Arguments

- x:

  A list of integers with names "analysis" and "assessment", or a data
  frame of analysis or training data.

- ...:

  Not currently used.

- data:

  A data frame.

- class:

  An optional class to give the object.

- assessment:

  A data frame of assessment or testing data, which can be empty.

## Examples

``` r
df <- data.frame(
  year = 1900:1999,
  value = 10 + 8*1900:1999 + runif(100L, 0, 100)
)
split_from_indices <- make_splits(
  x = list(analysis = which(df$year <= 1980),
           assessment = which(df$year > 1980)),
  data = df
)
split_from_data_frame <- make_splits(
  x = df[df$year <= 1980,],
  assessment = df[df$year > 1980,]
)
identical(split_from_indices, split_from_data_frame)
#> [1] TRUE
```
