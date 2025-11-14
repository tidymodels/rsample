# Create or Modify Stratification Variables

This function can create strata from numeric data and make non-numeric
data more conducive for stratification.

## Usage

``` r
make_strata(x, breaks = 4, nunique = 5, pool = 0.1, depth = 20)
```

## Arguments

- x:

  An input vector.

- breaks:

  A single number giving the number of bins desired to stratify a
  numeric stratification variable.

- nunique:

  An integer for the number of unique value threshold in the algorithm.

- pool:

  A proportion of data used to determine if a particular group is too
  small and should be pooled into another group. We do not recommend
  decreasing this argument below its default of 0.1 because of the
  dangers of stratifying groups that are too small.

- depth:

  An integer that is used to determine the best number of percentiles
  that should be used. The number of bins are based on
  `min(5, floor(n / depth))` where `n = length(x)`. If `x` is numeric,
  there must be at least 40 rows in the data set (when `depth = 20`) to
  conduct stratified sampling.

## Value

A factor vector.

## Details

For numeric data, if the number of unique levels is less than `nunique`,
the data are treated as categorical data.

For categorical inputs, the function will find levels of `x` than occur
in the data with percentage less than `pool`. The values from these
groups will be randomly assigned to the remaining strata (as will data
points that have missing values in `x`).

For numeric data with more unique values than `nunique`, the data will
be converted to being categorical based on percentiles of the data. The
percentile groups will have no more than 20 percent of the data in each
group. Again, missing values in `x` are randomly assigned to groups.

## Examples

``` r
set.seed(61)
x1 <- rpois(100, lambda = 5)
table(x1)
#> x1
#>  1  2  3  4  5  6  7  8  9 10 11 
#>  3 16  8 19 14 18 11  4  5  1  1 
table(make_strata(x1))
#> 
#>  [1,3]  (3,5]  (5,6] (6,11] 
#>     27     33     18     22 

set.seed(554)
x2 <- rpois(100, lambda = 1)
table(x2)
#> x2
#>  0  1  2  3  4 
#> 36 34 19  6  5 
table(make_strata(x2))
#> 
#>  0  1  2 
#> 38 40 22 

# small groups are randomly assigned
x3 <- factor(x2)
table(x3)
#> x3
#>  0  1  2  3  4 
#> 36 34 19  6  5 
table(make_strata(x3))
#> 
#>  0  1  2 
#> 41 35 24 

x4 <- rep(LETTERS[1:7], c(37, 26, 3, 7, 11, 10, 2))
table(x4)
#> x4
#>  A  B  C  D  E  F  G 
#> 37 26  3  7 11 10  2 
table(make_strata(x4))
#> 
#>  A  B  E  F 
#> 40 27 14 15 
table(make_strata(x4, pool = 0.1))
#> 
#>  A  B  E  F 
#> 38 29 12 17 
table(make_strata(x4, pool = 0.0))
#> Warning: Stratifying groups that make up 0% of the data may be statistically
#> risky.
#> ℹ Consider increasing `pool` to at least 0.1.
#> 
#>  A  B  C  D  E  F  G 
#> 37 26  3  7 11 10  2 

# not enough data to stratify
x5 <- rnorm(20)
table(make_strata(x5))
#> Warning: The number of observations in each quantile is below the recommended
#> threshold of 20.
#> • Stratification will use 1 breaks instead.
#> Warning: Too little data to stratify.
#> • Resampling will be unstratified.
#> 
#> strata1 
#>      20 

set.seed(483)
x6 <- rnorm(200)
quantile(x6, probs = (0:10) / 10)
#>         0%        10%        20%        30%        40%        50% 
#> -2.9114060 -1.4508635 -0.9513821 -0.6257852 -0.3286468 -0.0364388 
#>        60%        70%        80%        90%       100% 
#>  0.2027140  0.4278573  0.7050643  1.2471852  2.6792505 
table(make_strata(x6, breaks = 10))
#> 
#>    [-2.91,-1.45]   (-1.45,-0.951]  (-0.951,-0.626]  (-0.626,-0.329] 
#>               20               20               20               20 
#> (-0.329,-0.0364]  (-0.0364,0.203]    (0.203,0.428]    (0.428,0.705] 
#>               20               20               20               20 
#>     (0.705,1.25]      (1.25,2.68] 
#>               20               20 
```
