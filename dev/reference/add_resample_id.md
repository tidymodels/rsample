# Augment a data set with resampling identifiers

For a data set, `add_resample_id()` will add at least one new column
that identifies which resample that the data came from. In most cases, a
single column is added but for some resampling methods, two or more are
added.

## Usage

``` r
add_resample_id(.data, split, dots = FALSE)
```

## Arguments

- .data:

  A data frame.

- split:

  A single `rset` object.

- dots:

  A single logical: should the id columns be prefixed with a "." to
  avoid name conflicts with `.data`?

## Value

An updated data frame.

## See also

labels.rsplit

## Examples

``` r
library(dplyr)
#> 
#> Attaching package: ‘dplyr’
#> The following objects are masked from ‘package:stats’:
#> 
#>     filter, lag
#> The following objects are masked from ‘package:base’:
#> 
#>     intersect, setdiff, setequal, union

set.seed(363)
car_folds <- vfold_cv(mtcars, repeats = 3)

analysis(car_folds$splits[[1]]) |>
  add_resample_id(car_folds$splits[[1]]) |>
  head()
#>                    mpg cyl disp  hp drat    wt  qsec vs am gear carb
#> Mazda RX4         21.0   6  160 110 3.90 2.620 16.46  0  1    4    4
#> Mazda RX4 Wag     21.0   6  160 110 3.90 2.875 17.02  0  1    4    4
#> Datsun 710        22.8   4  108  93 3.85 2.320 18.61  1  1    4    1
#> Hornet 4 Drive    21.4   6  258 110 3.08 3.215 19.44  1  0    3    1
#> Hornet Sportabout 18.7   8  360 175 3.15 3.440 17.02  0  0    3    2
#> Valiant           18.1   6  225 105 2.76 3.460 20.22  1  0    3    1
#>                        id    id2
#> Mazda RX4         Repeat1 Fold01
#> Mazda RX4 Wag     Repeat1 Fold01
#> Datsun 710        Repeat1 Fold01
#> Hornet 4 Drive    Repeat1 Fold01
#> Hornet Sportabout Repeat1 Fold01
#> Valiant           Repeat1 Fold01

car_bt <- bootstraps(mtcars)

analysis(car_bt$splits[[1]]) |>
  add_resample_id(car_bt$splits[[1]]) |>
  head()
#>                        mpg cyl  disp  hp drat    wt  qsec vs am gear
#> Toyota Corona...1     21.5   4 120.1  97 3.70 2.465 20.01  1  0    3
#> Mazda RX4...2         21.0   6 160.0 110 3.90 2.620 16.46  0  1    4
#> Chrysler Imperial...3 14.7   8 440.0 230 3.23 5.345 17.42  0  0    3
#> Volvo 142E...4        21.4   4 121.0 109 4.11 2.780 18.60  1  1    4
#> Chrysler Imperial...5 14.7   8 440.0 230 3.23 5.345 17.42  0  0    3
#> Volvo 142E...6        21.4   4 121.0 109 4.11 2.780 18.60  1  1    4
#>                       carb          id
#> Toyota Corona...1        1 Bootstrap01
#> Mazda RX4...2            4 Bootstrap01
#> Chrysler Imperial...3    4 Bootstrap01
#> Volvo 142E...4           2 Bootstrap01
#> Chrysler Imperial...5    4 Bootstrap01
#> Volvo 142E...6           2 Bootstrap01
```
