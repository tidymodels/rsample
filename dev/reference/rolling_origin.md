# Rolling Origin Forecast Resampling

**\[superseded\]**

This resampling method is useful when the data set has a strong time
component. The resamples are not random and contain data points that are
consecutive values. The function assumes that the original data set are
sorted in time order.

This function is superseded by
[`sliding_window()`](https://rsample.tidymodels.org/dev/reference/slide-resampling.md),
[`sliding_index()`](https://rsample.tidymodels.org/dev/reference/slide-resampling.md),
and
[`sliding_period()`](https://rsample.tidymodels.org/dev/reference/slide-resampling.md)
which provide more flexibility and control. Superseded functions will
not go away, but active development will be focused on the new
functions.

## Usage

``` r
rolling_origin(
  data,
  initial = 5,
  assess = 1,
  cumulative = TRUE,
  skip = 0,
  lag = 0,
  ...
)
```

## Arguments

- data:

  A data frame.

- initial:

  The number of samples used for analysis/modeling in the initial
  resample.

- assess:

  The number of samples used for each assessment resample.

- cumulative:

  A logical. Should the analysis resample grow beyond the size specified
  by `initial` at each resample?.

- skip:

  A integer indicating how many (if any) *additional* resamples to skip
  to thin the total amount of data points in the analysis resample. See
  the example below.

- lag:

  A value to include a lag between the assessment and analysis set. This
  is useful if lagged predictors will be used during training and
  testing.

- ...:

  These dots are for future extensions and must be empty.

## Value

An tibble with classes `rolling_origin`, `rset`, `tbl_df`, `tbl`, and
`data.frame`. The results include a column for the data split objects
and a column called `id` that has a character string with the resample
identifier.

## Details

The main options, `initial` and `assess`, control the number of data
points from the original data that are in the analysis and assessment
set, respectively. When `cumulative = TRUE`, the analysis set will grow
as resampling continues while the assessment set size will always remain
static. `skip` enables the function to not use every data point in the
resamples. When `skip = 0`, the resampling data sets will increment by
one position. Suppose that the rows of a data set are consecutive days.
Using `skip = 6` will make the analysis data set to operate on *weeks*
instead of days. The assessment set size is not affected by this option.

## See also

[`sliding_window()`](https://rsample.tidymodels.org/dev/reference/slide-resampling.md),
[`sliding_index()`](https://rsample.tidymodels.org/dev/reference/slide-resampling.md),
and
[`sliding_period()`](https://rsample.tidymodels.org/dev/reference/slide-resampling.md)
for additional time based resampling functions.

## Examples

``` r
set.seed(1131)
ex_data <- data.frame(row = 1:20, some_var = rnorm(20))
dim(rolling_origin(ex_data))
#> [1] 15  2
dim(rolling_origin(ex_data, skip = 2))
#> [1] 5 2
dim(rolling_origin(ex_data, skip = 2, cumulative = FALSE))
#> [1] 5 2

# You can also roll over calendar periods by first nesting by that period,
# which is especially useful for irregular series where a fixed window
# is not useful. This example slides over 5 years at a time.
library(dplyr)
library(tidyr)
data(drinks, package = "modeldata")

drinks_annual <- drinks |>
  mutate(year = as.POSIXlt(date)$year + 1900) |>
  nest(data = c(-year))

multi_year_roll <- rolling_origin(drinks_annual, cumulative = FALSE)

analysis(multi_year_roll$splits[[1]])
#> # A tibble: 5 × 2
#>    year data             
#>   <dbl> <list>           
#> 1  1992 <tibble [12 × 2]>
#> 2  1993 <tibble [12 × 2]>
#> 3  1994 <tibble [12 × 2]>
#> 4  1995 <tibble [12 × 2]>
#> 5  1996 <tibble [12 × 2]>
assessment(multi_year_roll$splits[[1]])
#> # A tibble: 1 × 2
#>    year data             
#>   <dbl> <list>           
#> 1  1997 <tibble [12 × 2]>
```
