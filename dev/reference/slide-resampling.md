# Time-based Resampling

These resampling functions are focused on various forms of *time series*
resampling.

- `sliding_window()` uses the row number when computing the resampling
  indices. It is independent of any time index, but is useful with
  completely regular series.

- `sliding_index()` computes resampling indices relative to the `index`
  column. This is often a Date or POSIXct column, but doesn't have to
  be. This is useful when resampling irregular series, or for using
  irregular lookback periods such as `lookback = lubridate::years(1)`
  with daily data (where the number of days in a year may vary).

- `sliding_period()` first breaks up the `index` into less granular
  groups based on `period`, and then uses that to construct the
  resampling indices. This is extremely useful for constructing rolling
  monthly or yearly windows from daily data.

## Usage

``` r
sliding_window(
  data,
  ...,
  lookback = 0L,
  assess_start = 1L,
  assess_stop = 1L,
  complete = TRUE,
  step = 1L,
  skip = 0L
)

sliding_index(
  data,
  index,
  ...,
  lookback = 0L,
  assess_start = 1L,
  assess_stop = 1L,
  complete = TRUE,
  step = 1L,
  skip = 0L
)

sliding_period(
  data,
  index,
  period,
  ...,
  lookback = 0L,
  assess_start = 1L,
  assess_stop = 1L,
  complete = TRUE,
  step = 1L,
  skip = 0L,
  every = 1L,
  origin = NULL
)
```

## Arguments

- data:

  A data frame.

- ...:

  These dots are for future extensions and must be empty.

- lookback:

  The number of elements to look back from the current element when
  computing the resampling indices of the analysis set. The current row
  is always included in the analysis set.

  - For `sliding_window()`, a single integer defining the number of rows
    to look back from the current row.

  - For `sliding_index()`, a single object that will be subtracted from
    the `index` as `index - lookback` to define the boundary of where to
    start searching for rows to include in the current resample. This is
    often an integer value corresponding to the number of days to look
    back, or a lubridate Period object.

  - For `sliding_period()`, a single integer defining the number of
    groups to look back from the current group, where the groups were
    defined from breaking up the `index` according to the `period`.

  In all cases, `Inf` is also allowed to force an expanding window.

- assess_start, assess_stop:

  This combination of arguments determines how far into the future to
  look when constructing the assessment set. Together they construct a
  range of `[index + assess_start, index + assess_stop]` to search for
  rows to include in the assessment set.

  Generally, `assess_start` will always be `1` to indicate that the
  first value to potentially include in the assessment set should start
  one element after the current row, but it can be increased to a larger
  value to create "gaps" between the analysis and assessment set if you
  are worried about high levels of correlation in short term
  forecasting.

  - For `sliding_window()`, these are both single integers defining the
    number of rows to look forward from the current row.

  - For `sliding_index()`, these are single objects that will be added
    to the `index` to compute the range to search for rows to include in
    the assessment set. This is often an integer value corresponding to
    the number of days to look forward, or a lubridate Period object.

  - For `sliding_period()`, these are both single integers defining the
    number of groups to look forward from the current group, where the
    groups were defined from breaking up the `index` according to the
    `period`.

- complete:

  A single logical. When using `lookback` to compute the analysis sets,
  should only complete windows be considered? If set to `FALSE`, partial
  windows will be used until it is possible to create a complete window
  (based on `lookback`). This is a way to use an expanding window up to
  a certain point, and then switch to a sliding window.

- step:

  A single positive integer. After computing the resampling indices,
  `step` is used to thin out the results by selecting every `step`-th
  result by subsetting the indices with `seq(1L, n_indices, by = step)`.
  `step` is applied after `skip`. Note that `step` is independent of any
  time `index` used.

- skip:

  A single positive integer, or zero. After computing the resampling
  indices, the first `skip` results will be dropped by subsetting the
  indices with `seq(skip + 1L, n_indices)`. This can be especially
  useful when combined with `lookback = Inf`, which creates an expanding
  window starting from the first row. By skipping forward, you can drop
  the first few windows that have very few data points. `skip` is
  applied before `step`. Note that `skip` is independent of any time
  `index` used.

- index:

  The index to compute resampling indices relative to, specified as a
  bare column name. This must be an existing column in `data`.

  - For `sliding_index()`, this is commonly a date vector, but is not
    required.

  - For `sliding_period()`, it is required that this is a Date or
    POSIXct vector.

  The `index` must be an *increasing* vector, but duplicate values are
  allowed. Additionally, the index cannot contain any missing values.

- period:

  The period to group the `index` by. This is specified as a single
  string, such as `"year"` or `"month"`. See the `.period` argument of
  [`slider::slide_period()`](https://slider.r-lib.org/reference/slide_period.html)
  for the full list of options and further explanation.

- every:

  A single positive integer. The number of periods to group together.

  For example, if the `period` was set to `"year"` with an `every` value
  of 2, then the years 1970 and 1971 would be placed in the same group.

- origin:

  The reference date time value. The default when left as `NULL` is the
  epoch time of `1970-01-01 00:00:00`, *in the time zone of the index*.

  This is generally used to define the anchor time to count from, which
  is relevant when the `every` value is `> 1`.

## See also

[`rolling_origin()`](https://rsample.tidymodels.org/dev/reference/rolling_origin.md)

[`slider::slide()`](https://slider.r-lib.org/reference/slide.html),
[`slider::slide_index()`](https://slider.r-lib.org/reference/slide_index.html),
and
[`slider::slide_period()`](https://slider.r-lib.org/reference/slide_period.html),
which power these resamplers.

## Examples

``` r
library(vctrs)
#> 
#> Attaching package: ‘vctrs’
#> The following object is masked from ‘package:tibble’:
#> 
#>     data_frame
#> The following object is masked from ‘package:dplyr’:
#> 
#>     data_frame
library(tibble)
library(modeldata)
data("Chicago")

index <- new_date(c(1, 3, 4, 7, 8, 9, 13, 15, 16, 17))
df <- tibble(x = 1:10, index = index)
df
#> # A tibble: 10 × 2
#>        x index     
#>    <int> <date>    
#>  1     1 1970-01-02
#>  2     2 1970-01-04
#>  3     3 1970-01-05
#>  4     4 1970-01-08
#>  5     5 1970-01-09
#>  6     6 1970-01-10
#>  7     7 1970-01-14
#>  8     8 1970-01-16
#>  9     9 1970-01-17
#> 10    10 1970-01-18

# Look back two rows beyond the current row, for a total of three rows
# in each analysis set. Each assessment set is composed of the two rows after
# the current row.
sliding_window(df, lookback = 2, assess_stop = 2)
#> # Sliding window resampling 
#> # A tibble: 6 × 2
#>   splits        id    
#>   <list>        <chr> 
#> 1 <split [3/2]> Slice1
#> 2 <split [3/2]> Slice2
#> 3 <split [3/2]> Slice3
#> 4 <split [3/2]> Slice4
#> 5 <split [3/2]> Slice5
#> 6 <split [3/2]> Slice6

# Same as before, but step forward by 3 rows between each resampling slice,
# rather than just by 1.
rset <- sliding_window(df, lookback = 2, assess_stop = 2, step = 3)
rset
#> # Sliding window resampling 
#> # A tibble: 2 × 2
#>   splits        id    
#>   <list>        <chr> 
#> 1 <split [3/2]> Slice1
#> 2 <split [3/2]> Slice2

analysis(rset$splits[[1]])
#> # A tibble: 3 × 2
#>       x index     
#>   <int> <date>    
#> 1     1 1970-01-02
#> 2     2 1970-01-04
#> 3     3 1970-01-05
analysis(rset$splits[[2]])
#> # A tibble: 3 × 2
#>       x index     
#>   <int> <date>    
#> 1     4 1970-01-08
#> 2     5 1970-01-09
#> 3     6 1970-01-10

# Now slide relative to the `index` column in `df`. This time we look back
# 2 days from the current row's `index` value, and 2 days forward from
# it to construct the assessment set. Note that this series is irregular,
# so it produces different results than `sliding_window()`. Additionally,
# note that it is entirely possible for the assessment set to contain no
# data if you have a highly irregular series and "look forward" into a
# date range where no data points actually exist!
sliding_index(df, index, lookback = 2, assess_stop = 2)
#> # Sliding index resampling 
#> # A tibble: 7 × 2
#>   splits        id    
#>   <list>        <chr> 
#> 1 <split [2/1]> Slice1
#> 2 <split [2/0]> Slice2
#> 3 <split [1/2]> Slice3
#> 4 <split [2/1]> Slice4
#> 5 <split [3/0]> Slice5
#> 6 <split [1/1]> Slice6
#> 7 <split [2/2]> Slice7

# With `sliding_period()`, we can break up our date index into less granular
# chunks, and slide over them instead of the index directly. Here we'll use
# the Chicago data, which contains daily data spanning 16 years, and we'll
# break it up into rolling yearly chunks. Three years worth of data will
# be used for the analysis set, and one years worth of data will be held out
# for performance assessment.
sliding_period(
  Chicago,
  date,
  "year",
  lookback = 2,
  assess_stop = 1
)
#> # Sliding period resampling 
#> # A tibble: 13 × 2
#>    splits             id     
#>    <list>             <chr>  
#>  1 <split [1074/366]> Slice01
#>  2 <split [1096/365]> Slice02
#>  3 <split [1096/365]> Slice03
#>  4 <split [1096/365]> Slice04
#>  5 <split [1095/366]> Slice05
#>  6 <split [1096/365]> Slice06
#>  7 <split [1096/365]> Slice07
#>  8 <split [1096/365]> Slice08
#>  9 <split [1095/366]> Slice09
#> 10 <split [1096/365]> Slice10
#> 11 <split [1096/365]> Slice11
#> 12 <split [1096/365]> Slice12
#> 13 <split [1095/241]> Slice13

# Because `lookback = 2`, three years are required to form a "complete"
# window of data. To allow partial windows, set `complete = FALSE`.
# Here that first constructs two expanding windows until a complete three
# year window can be formed, at which point we switch to a sliding window.
sliding_period(
  Chicago,
  date,
  "year",
  lookback = 2,
  assess_stop = 1,
  complete = FALSE
)
#> # Sliding period resampling 
#> # A tibble: 15 × 2
#>    splits             id     
#>    <list>             <chr>  
#>  1 <split [344/365]>  Slice01
#>  2 <split [709/365]>  Slice02
#>  3 <split [1074/366]> Slice03
#>  4 <split [1096/365]> Slice04
#>  5 <split [1096/365]> Slice05
#>  6 <split [1096/365]> Slice06
#>  7 <split [1095/366]> Slice07
#>  8 <split [1096/365]> Slice08
#>  9 <split [1096/365]> Slice09
#> 10 <split [1096/365]> Slice10
#> 11 <split [1095/366]> Slice11
#> 12 <split [1096/365]> Slice12
#> 13 <split [1096/365]> Slice13
#> 14 <split [1096/365]> Slice14
#> 15 <split [1095/241]> Slice15

# Alternatively, you could break the resamples up by month. Here we'll
# use an expanding monthly window by setting `lookback = Inf`, and each
# assessment set will contain two months of data. To ensure that we have
# enough data to fit our models, we'll `skip` the first 4 expanding windows.
# Finally, to thin out the results, we'll `step` forward by 2 between
# each resample.
sliding_period(
  Chicago,
  date,
  "month",
  lookback = Inf,
  assess_stop = 2,
  skip = 4,
  step = 2
)
#> # Sliding period resampling 
#> # A tibble: 91 × 2
#>    splits           id     
#>    <list>           <chr>  
#>  1 <split [130/61]> Slice01
#>  2 <split [191/61]> Slice02
#>  3 <split [252/61]> Slice03
#>  4 <split [313/62]> Slice04
#>  5 <split [375/59]> Slice05
#>  6 <split [434/61]> Slice06
#>  7 <split [495/61]> Slice07
#>  8 <split [556/61]> Slice08
#>  9 <split [617/61]> Slice09
#> 10 <split [678/62]> Slice10
#> # ℹ 81 more rows
```
