# Simple Training/Test Set Splitting

`initial_split()` creates a single binary split of the data into a
training set and testing set. `initial_time_split()` does the same, but
takes the *first* `prop` samples for training, instead of a random
selection. `group_initial_split()` creates splits of the data based on
some grouping variable, so that all data in a "group" is assigned to the
same split.

## Usage

``` r
initial_split(data, prop = 3/4, strata = NULL, breaks = 4, pool = 0.1, ...)

initial_time_split(data, prop = 3/4, lag = lifecycle::deprecated(), ...)

training(x, ...)

# Default S3 method
training(x, ...)

# S3 method for class 'rsplit'
training(x, ...)

testing(x, ...)

# Default S3 method
testing(x, ...)

# S3 method for class 'rsplit'
testing(x, ...)

group_initial_split(data, group, prop = 3/4, ..., strata = NULL, pool = 0.1)
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

  **\[deprecated\]** This is deprecated, please lag your predictors
  prior to splitting the dataset.

- x:

  An `rsplit` object produced by `initial_split()` or
  `initial_time_split()`.

- group:

  A variable in `data` (single character or name) used for grouping
  observations with the same value to either the analysis or assessment
  set within a fold.

## Value

An `rsplit` object that can be used with the `training()` and
`testing()` functions to extract the data in each split.

## Details

`training()` and `testing()` are used to extract the resulting data.

To avoid data leakage when using lagged variables, lag the predictors
before the initial split.

With a `strata` argument, the random sampling is conducted *within the
stratification variable*. This can help ensure that the resamples have
equivalent proportions as the original data set. For a categorical
variable, sampling is conducted separately within each class. For a
numeric stratification variable, `strata` is binned into quartiles,
which are then used to stratify. Strata below 10% of the total are
pooled together; see
[`make_strata()`](https://rsample.tidymodels.org/dev/reference/make_strata.md)
for more details.

## Examples

``` r
set.seed(1353)
car_split <- initial_split(mtcars)
train_data <- training(car_split)
test_data <- testing(car_split)

data(drinks, package = "modeldata")
drinks_split <- initial_time_split(drinks)
train_data <- training(drinks_split)
test_data <- testing(drinks_split)
c(max(train_data$date), min(test_data$date))
#> [1] "2011-03-01" "2011-04-01"

set.seed(1353)
car_split <- group_initial_split(mtcars, cyl)
train_data <- training(car_split)
test_data <- testing(car_split)
```
