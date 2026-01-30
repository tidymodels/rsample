# Create an Initial Train/Validation/Test Split

`initial_validation_split()` creates a random three-way split of the
data into a training set, a validation set, and a testing set.
`initial_validation_time_split()` does the same, but instead of a random
selection the training, validation, and testing set are in order of the
full data set, with the first observations being put into the training
set. `group_initial_validation_split()` creates similar random splits of
the data based on some grouping variable, so that all data in a "group"
are assigned to the same partition.

## Usage

``` r
initial_validation_split(
  data,
  prop = c(0.6, 0.2),
  strata = NULL,
  breaks = 4,
  pool = 0.1,
  ...
)

initial_validation_time_split(data, prop = c(0.6, 0.2), ...)

group_initial_validation_split(
  data,
  group,
  prop = c(0.6, 0.2),
  ...,
  strata = NULL,
  pool = 0.1
)

# S3 method for class 'initial_validation_split'
training(x, ...)

# S3 method for class 'initial_validation_split'
testing(x, ...)

validation(x, ...)

# Default S3 method
validation(x, ...)

# S3 method for class 'initial_validation_split'
validation(x, ...)
```

## Arguments

- data:

  A data frame.

- prop:

  A length-2 vector of proportions of data to be retained for training
  and validation data, respectively.

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

- group:

  A variable in `data` (single character or name) used for grouping
  observations with the same value to either the analysis or assessment
  set within a fold.

- x:

  An object of class `initial_validation_split`.

## Value

An `initial_validation_split` object that can be used with the
[`training()`](https://rsample.tidymodels.org/reference/initial_split.md),
`validation()`, and
[`testing()`](https://rsample.tidymodels.org/reference/initial_split.md)
functions to extract the data in each split.

## Details

[`training()`](https://rsample.tidymodels.org/reference/initial_split.md),
`validation()`, and
[`testing()`](https://rsample.tidymodels.org/reference/initial_split.md)
can be used to extract the resulting data sets. Use
[`validation_set()`](https://rsample.tidymodels.org/reference/validation_set.md)
to create an `rset` object for use with functions from the tune package
such as
[`tune::tune_grid()`](https://tune.tidymodels.org/reference/tune_grid.html).

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

[`validation_set()`](https://rsample.tidymodels.org/reference/validation_set.md)

## Examples

``` r
set.seed(1353)
car_split <- initial_validation_split(mtcars)
train_data <- training(car_split)
validation_data <- validation(car_split)
test_data <- testing(car_split)

data(drinks, package = "modeldata")
drinks_split <- initial_validation_time_split(drinks)
train_data <- training(drinks_split)
validation_data <- validation(drinks_split)
c(max(train_data$date), min(validation_data$date))
#> [1] "2007-05-01" "2007-06-01"

data(ames, package = "modeldata")
set.seed(1353)
ames_split <- group_initial_validation_split(ames, group = Neighborhood)
train_data <- training(ames_split)
validation_data <- validation(ames_split)
test_data <- testing(ames_split)
```
