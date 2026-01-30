# Get the split arguments from an rset

Get the split arguments from an rset

## Usage

``` r
.get_split_args(x, allow_strata_false = FALSE)
```

## Arguments

- x:

  An `rset` or `initial_split` object.

- allow_strata_false:

  A logical to specify which value to use if no stratification was
  specified. The default is to use `strata = NULL`, the alternative is
  `strata = FALSE`.

## Value

A list of arguments used to create the rset.
