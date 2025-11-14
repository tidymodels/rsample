# Introduction to rsample

## Terminology

We define a *resample* as the result of a two-way split of a data set.
For example, when bootstrapping, one part of the resample is a sample
with replacement of the original data. The other part of the split
contains the instances that were not contained in the bootstrap sample.
Cross-validation is another type of resampling.

## `rset` Objects Contain Many Resamples

The main class in the package (`rset`) is for a *set* or *collection* of
resamples. In 10-fold cross-validation, the set would consist of the 10
different resamples of the original data.

Like [modelr](https://cran.r-project.org/package=modelr), the resamples
are stored in data-frame-like `tibble` object. As a simple example, here
is a small set of bootstraps of the `mtcars` data:

``` r
library(rsample)
set.seed(8584)
bt_resamples <- bootstraps(mtcars, times = 3)
bt_resamples
#> # Bootstrap sampling 
#> # A tibble: 3 × 2
#>   splits          id        
#>   <list>          <chr>     
#> 1 <split [32/14]> Bootstrap1
#> 2 <split [32/12]> Bootstrap2
#> 3 <split [32/14]> Bootstrap3
```

## Individual Resamples are `rsplit` Objects

The resamples are stored in the `splits` column in an object that has
class `rsplit`.

In this package we use the following terminology for the two partitions
that comprise a resample:

- The *analysis* data are those that we selected in the resample. For a
  bootstrap, this is the sample with replacement. For 10-fold
  cross-validation, this is the 90% of the data. These data are often
  used to fit a model or calculate a statistic in traditional
  bootstrapping.
- The *assessment* data are usually the section of the original data not
  covered by the analysis set. Again, in 10-fold CV, this is the 10%
  held out. These data are often used to evaluate the performance of a
  model that was fit to the analysis data.

(Aside: While some might use the term “training” and “testing” for these
data sets, we avoid them since those labels often conflict with the data
that result from an initial partition of the data that is typically done
*before* resampling. The training/test split can be conducted using the
[`initial_split()`](https://rsample.tidymodels.org/dev/reference/initial_split.md)
function in this package.)

Let’s look at one of the `rsplit` objects

``` r
first_resample <- bt_resamples$splits[[1]]
first_resample
#> <Analysis/Assess/Total>
#> <32/14/32>
```

This indicates that there were 32 data points in the analysis set, 14
instances were in the assessment set, and that the original data
contained 32 data points. These results can also be determined using the
`dim` function on an `rsplit` object.

To obtain either of these data sets from an `rsplit`, the
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) function
can be used. By default, the analysis set is returned but the `data`
option can be used to return the assessment data:

``` r
head(as.data.frame(first_resample))
#>                     mpg cyl  disp  hp drat   wt qsec vs am gear carb
#> Fiat 128...1       32.4   4  78.7  66 4.08 2.20 19.5  1  1    4    1
#> Toyota Corolla...2 33.9   4  71.1  65 4.22 1.83 19.9  1  1    4    1
#> Toyota Corolla...3 33.9   4  71.1  65 4.22 1.83 19.9  1  1    4    1
#> AMC Javelin...4    15.2   8 304.0 150 3.15 3.44 17.3  0  0    3    2
#> Valiant...5        18.1   6 225.0 105 2.76 3.46 20.2  1  0    3    1
#> Merc 450SLC...6    15.2   8 275.8 180 3.07 3.78 18.0  0  0    3    3
as.data.frame(first_resample, data = "assessment")
#>                     mpg cyl  disp  hp drat   wt qsec vs am gear carb
#> Mazda RX4 Wag      21.0   6 160.0 110 3.90 2.88 17.0  0  1    4    4
#> Hornet 4 Drive     21.4   6 258.0 110 3.08 3.21 19.4  1  0    3    1
#> Merc 240D          24.4   4 146.7  62 3.69 3.19 20.0  1  0    4    2
#> Merc 230           22.8   4 140.8  95 3.92 3.15 22.9  1  0    4    2
#> Merc 280           19.2   6 167.6 123 3.92 3.44 18.3  1  0    4    4
#> Merc 280C          17.8   6 167.6 123 3.92 3.44 18.9  1  0    4    4
#> Merc 450SE         16.4   8 275.8 180 3.07 4.07 17.4  0  0    3    3
#> Merc 450SL         17.3   8 275.8 180 3.07 3.73 17.6  0  0    3    3
#> Cadillac Fleetwood 10.4   8 472.0 205 2.93 5.25 18.0  0  0    3    4
#> Chrysler Imperial  14.7   8 440.0 230 3.23 5.34 17.4  0  0    3    4
#> Honda Civic        30.4   4  75.7  52 4.93 1.61 18.5  1  1    4    2
#> Fiat X1-9          27.3   4  79.0  66 4.08 1.94 18.9  1  1    4    1
#> Lotus Europa       30.4   4  95.1 113 3.77 1.51 16.9  1  1    5    2
#> Volvo 142E         21.4   4 121.0 109 4.11 2.78 18.6  1  1    4    2
```

Alternatively, you can use the shortcuts `analysis(first_resample)` and
`assessment(first_resample)`.
