# Make groupings for grouped rsplits

This function powers grouped resampling by splitting the data based upon
a grouping variable and returning the assessment set indices for each
split.

## Usage

``` r
make_groups(
  data,
  group,
  v,
  balance = c("groups", "observations", "prop"),
  strata = NULL,
  ...
)
```

## Arguments

- data:

  A data frame.

- group:

  A variable in `data` (single character or name) used for grouping
  observations with the same value to either the analysis or assessment
  set within a fold.

- v:

  The number of partitions of the data set.

- balance:

  If `v` is less than the number of unique groups, how should groups be
  combined into folds? Should be one of `"groups"`, `"observations"`,
  `"prop"`.

- strata:

  A variable in `data` (single character or name) used to conduct
  stratified sampling. When not `NULL`, each resample is created within
  the stratification variable. Numeric `strata` are binned into
  quartiles.

- ...:

  Arguments passed to balance functions.

## Details

Not all `balance` options are accepted – or make sense – for all
resampling functions. For instance, `balance = "prop"` assigns groups to
folds at random, meaning that any given observation is not guaranteed to
be in one (and only one) assessment set. That means `balance = "prop"`
can't be used with
[`group_vfold_cv()`](https://rsample.tidymodels.org/dev/reference/group_vfold_cv.md),
and so isn't an option available for that function.

Similarly,
[`group_mc_cv()`](https://rsample.tidymodels.org/dev/reference/group_mc_cv.md)
and its derivatives don't assign data to one (and only one) assessment
set, but rather allow each observation to be in an assessment set
zero-or-more times. As a result, those functions don't have a `balance`
argument, and under the hood always specify `balance = "prop"` when they
call `make_groups()`.
