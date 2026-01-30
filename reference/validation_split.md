# Create a Validation Set

**\[deprecated\]**

This function is deprecated because it's part of an approach to
constructing a training, validation, and testing set by doing a sequence
of two binary splits: testing / not-testing (with
[`initial_split()`](https://rsample.tidymodels.org/reference/initial_split.md)
or one of its variants) and then not-testing split into
training/validation with `validation_split()`. Instead, now use
[`initial_validation_split()`](https://rsample.tidymodels.org/reference/initial_validation_split.md)
or one if its variants to construct the three sets via one 3-way split.

`validation_split()` takes a single random sample (without replacement)
of the original data set to be used for analysis. All other data points
are added to the assessment set (to be used as the validation set).
`validation_time_split()` does the same, but takes the *first* `prop`
samples for training, instead of a random selection.
`group_validation_split()` creates splits of the data based on some
grouping variable, so that all data in a "group" is assigned to the same
split.

Note that the input `data` to `validation_split()`,
`validation_time_split()`, and `group_validation_split()` should *not*
contain the testing data. To create a three-way split directly of the
entire data set, use
[`initial_validation_split()`](https://rsample.tidymodels.org/reference/initial_validation_split.md).

## Usage

``` r
validation_split(data, prop = 3/4, strata = NULL, breaks = 4, pool = 0.1, ...)

validation_time_split(data, prop = 3/4, lag = 0, ...)

group_validation_split(data, group, prop = 3/4, ..., strata = NULL, pool = 0.1)
```

## Arguments

- data:

  A data frame.

- prop:

  The proportion of data to be retained for modeling/analysis.

- strata:

  A variable in `data` (single character or name) used to conduct
  stratified sampling. When not `NULL`, each resample is created within
  the stratification variable. Numeric `strata` are binned into
  quartiles.

- breaks:

  A single number giving the number of bins desired to stratify a
  numeric stratification variable.

- pool:

  A proportion of data used to determine if a particular group is too
  small and should be pooled into another group. We do not recommend
  decreasing this argument below its default of 0.1 because of the
  dangers of stratifying groups that are too small.

- ...:

  These dots are for future extensions and must be empty.

- lag:

  A value to include a lag between the assessment and analysis set. This
  is useful if lagged predictors will be used during training and
  testing.

- group:

  A variable in `data` (single character or name) used for grouping
  observations with the same value to either the analysis or assessment
  set within a fold.

## Value

An tibble with classes `validation_split`, `rset`, `tbl_df`, `tbl`, and
`data.frame`. The results include a column for the data split objects
and a column called `id` that has a character string with the resample
identifier.

## Details

With a `strata` argument, the random sampling is conducted *within the
stratification variable*. This can help ensure that the resamples have
equivalent proportions as the original data set. For a categorical
variable, sampling is conducted separately within each class. For a
numeric stratification variable, `strata` is binned into quartiles,
which are then used to stratify. Strata below 10% of the total are
pooled together; see
[`make_strata()`](https://rsample.tidymodels.org/reference/make_strata.md)
for more details.

## See also

[`initial_validation_split()`](https://rsample.tidymodels.org/reference/initial_validation_split.md),
[`group_initial_validation_split()`](https://rsample.tidymodels.org/reference/initial_validation_split.md),
[`validation_set()`](https://rsample.tidymodels.org/reference/validation_set.md)

## Examples

``` r
cars_split <- initial_split(mtcars)
cars_not_testing <- training(cars_split)
validation_split(cars_not_testing, prop = .9)
#> Warning: `validation_split()` was deprecated in rsample 1.2.0.
#> ℹ Please use `initial_validation_split()` instead.
#> # Validation Set Split (0.9/0.1)  
#> # A tibble: 1 × 2
#>   splits         id        
#>   <list>         <chr>     
#> 1 <split [21/3]> validation
group_validation_split(cars_not_testing, cyl)
#> Warning: `group_validation_split()` was deprecated in rsample 1.2.0.
#> ℹ Please use `group_initial_validation_split()` instead.
#> # Group Validation Set Split (0.75/0.25)  
#> # A tibble: 1 × 2
#>   splits         id        
#>   <list>         <chr>     
#> 1 <split [19/5]> validation

data(drinks, package = "modeldata")
validation_time_split(drinks[1:200,])
#> Warning: `validation_time_split()` was deprecated in rsample 1.2.0.
#> ℹ Please use `initial_validation_time_split()` instead.
#> # Validation Set Split (0.75/0.25)  
#> # A tibble: 1 × 2
#>   splits           id        
#>   <list>           <chr>     
#> 1 <split [150/50]> validation

# Alternative
cars_split_3 <- initial_validation_split(mtcars)
validation_set(cars_split_3)
#> # A tibble: 1 × 2
#>   splits         id        
#>   <list>         <chr>     
#> 1 <split [19/6]> validation
```
