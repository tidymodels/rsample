# Permutation sampling

A permutation sample is the same size as the original data set and is
made by permuting/shuffling one or more columns. This results in
analysis samples where some columns are in their original order and some
columns are permuted to a random order. Unlike other sampling functions
in rsample, there is no assessment set and calling
[`assessment()`](https://rsample.tidymodels.org/dev/reference/as.data.frame.rsplit.md)
on a permutation split will throw an error.

## Usage

``` r
permutations(data, permute = NULL, times = 25, apparent = FALSE, ...)
```

## Arguments

- data:

  A data frame.

- permute:

  One or more columns to shuffle. This argument supports tidyselect
  selectors. Multiple expressions can be combined with
  [`c()`](https://rdrr.io/r/base/c.html). Variable names can be used as
  if they were positions in the data frame, so expressions like `x:y`
  can be used to select a range of variables. See
  [`language`](https://tidyselect.r-lib.org/reference/language.html) for
  more details.

- times:

  The number of permutation samples.

- apparent:

  A logical. Should an extra resample be added where the analysis is the
  standard data set.

- ...:

  These dots are for future extensions and must be empty.

## Value

A `tibble` with classes `permutations`, `rset`, `tbl_df`, `tbl`, and
`data.frame`. The results include a column for the data split objects
and a column called `id` that has a character string with the resample
identifier.

## Details

The argument `apparent` enables the option of an additional "resample"
where the analysis data set is the same as the original data set.
Permutation-based resampling can be especially helpful for computing a
statistic under the null hypothesis (e.g. t-statistic). This forms the
basis of a permutation test, which computes a test statistic under all
possible permutations of the data.

## Examples

``` r
permutations(mtcars, mpg, times = 2)
#> # Permutation sampling 
#> # Permuted columns: [mpg] 
#> # A tibble: 2 × 2
#>   splits         id           
#>   <list>         <chr>        
#> 1 <split [32/0]> Permutations1
#> 2 <split [32/0]> Permutations2
permutations(mtcars, mpg, times = 2, apparent = TRUE)
#> # Permutation sampling with apparent sample 
#> # Permuted columns: [mpg] 
#> # A tibble: 3 × 2
#>   splits          id           
#>   <list>          <chr>        
#> 1 <split [32/0]>  Permutations1
#> 2 <split [32/0]>  Permutations2
#> 3 <split [32/32]> Apparent     

library(purrr)
resample1 <- permutations(mtcars, starts_with("c"), times = 1)
resample1$splits[[1]] |> analysis()
#>                      mpg cyl  disp  hp drat    wt  qsec vs am gear
#> Mazda RX4           21.0   6 160.0 110 3.90 2.620 16.46  0  1    4
#> Mazda RX4 Wag       21.0   4 160.0 110 3.90 2.875 17.02  0  1    4
#> Datsun 710          22.8   8 108.0  93 3.85 2.320 18.61  1  1    4
#> Hornet 4 Drive      21.4   8 258.0 110 3.08 3.215 19.44  1  0    3
#> Hornet Sportabout   18.7   4 360.0 175 3.15 3.440 17.02  0  0    3
#> Valiant             18.1   8 225.0 105 2.76 3.460 20.22  1  0    3
#> Duster 360          14.3   8 360.0 245 3.21 3.570 15.84  0  0    3
#> Merc 240D           24.4   8 146.7  62 3.69 3.190 20.00  1  0    4
#> Merc 230            22.8   8 140.8  95 3.92 3.150 22.90  1  0    4
#> Merc 280            19.2   8 167.6 123 3.92 3.440 18.30  1  0    4
#> Merc 280C           17.8   8 167.6 123 3.92 3.440 18.90  1  0    4
#> Merc 450SE          16.4   8 275.8 180 3.07 4.070 17.40  0  0    3
#> Merc 450SL          17.3   6 275.8 180 3.07 3.730 17.60  0  0    3
#> Merc 450SLC         15.2   6 275.8 180 3.07 3.780 18.00  0  0    3
#> Cadillac Fleetwood  10.4   8 472.0 205 2.93 5.250 17.98  0  0    3
#> Lincoln Continental 10.4   8 460.0 215 3.00 5.424 17.82  0  0    3
#> Chrysler Imperial   14.7   8 440.0 230 3.23 5.345 17.42  0  0    3
#> Fiat 128            32.4   4  78.7  66 4.08 2.200 19.47  1  1    4
#> Honda Civic         30.4   6  75.7  52 4.93 1.615 18.52  1  1    4
#> Toyota Corolla      33.9   4  71.1  65 4.22 1.835 19.90  1  1    4
#> Toyota Corona       21.5   6 120.1  97 3.70 2.465 20.01  1  0    3
#> Dodge Challenger    15.5   4 318.0 150 2.76 3.520 16.87  0  0    3
#> AMC Javelin         15.2   4 304.0 150 3.15 3.435 17.30  0  0    3
#> Camaro Z28          13.3   4 350.0 245 3.73 3.840 15.41  0  0    3
#> Pontiac Firebird    19.2   8 400.0 175 3.08 3.845 17.05  0  0    3
#> Fiat X1-9           27.3   4  79.0  66 4.08 1.935 18.90  1  1    4
#> Porsche 914-2       26.0   6 120.3  91 4.43 2.140 16.70  0  1    5
#> Lotus Europa        30.4   4  95.1 113 3.77 1.513 16.90  1  1    5
#> Ford Pantera L      15.8   4 351.0 264 4.22 3.170 14.50  0  1    5
#> Ferrari Dino        19.7   6 145.0 175 3.62 2.770 15.50  0  1    5
#> Maserati Bora       15.0   8 301.0 335 3.54 3.570 14.60  0  1    5
#> Volvo 142E          21.4   4 121.0 109 4.11 2.780 18.60  1  1    4
#>                     carb
#> Mazda RX4              1
#> Mazda RX4 Wag          2
#> Datsun 710             4
#> Hornet 4 Drive         4
#> Hornet Sportabout      1
#> Valiant                4
#> Duster 360             2
#> Merc 240D              3
#> Merc 230               4
#> Merc 280               2
#> Merc 280C              2
#> Merc 450SE             4
#> Merc 450SL             4
#> Merc 450SLC            4
#> Cadillac Fleetwood     8
#> Lincoln Continental    2
#> Chrysler Imperial      3
#> Fiat 128               2
#> Honda Civic            1
#> Toyota Corolla         2
#> Toyota Corona          4
#> Dodge Challenger       1
#> AMC Javelin            2
#> Camaro Z28             2
#> Pontiac Firebird       3
#> Fiat X1-9              1
#> Porsche 914-2          4
#> Lotus Europa           1
#> Ford Pantera L         1
#> Ferrari Dino           6
#> Maserati Bora          4
#> Volvo 142E             2

resample2 <- permutations(mtcars, hp, times = 10, apparent = TRUE)
map_dbl(resample2$splits, function(x) {
  t.test(hp ~ vs, data = analysis(x))$statistic
})
#>  [1]  1.831884490  0.360219662 -1.271345514 -1.086517310  0.884050160
#>  [6]  1.130681222  0.369342268 -2.595445455  0.007920257  0.562836352
#> [11]  6.290837794
```
