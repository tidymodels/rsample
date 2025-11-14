# Extract Predictor Names from Formula or Terms

While [`all.vars()`](https://rdrr.io/r/base/allnames.html) returns all
variables used in a formula, this function only returns the variables
explicitly used on the right-hand side (i.e., it will not resolve dots
unless the object is terms with a data set specified).

## Usage

``` r
form_pred(object, ...)
```

## Arguments

- object:

  A model formula or
  [`stats::terms()`](https://rdrr.io/r/stats/terms.html) object.

- ...:

  Arguments to pass to
  [`all.vars()`](https://rdrr.io/r/base/allnames.html)

## Value

A character vector of names

## Examples

``` r
form_pred(y ~ x + z)
#> [1] "x" "z"
form_pred(terms(y ~ x + z))
#> [1] "x" "z"

form_pred(y ~ x + log(z))
#> [1] "x" "z"
form_pred(log(y) ~ x + z)
#> [1] "x" "z"

form_pred(y1 + y2 ~ x + z)
#> [1] "x" "z"
form_pred(log(y1) + y2 ~ x + z)
#> [1] "x" "z"

# will fail:
# form_pred(y ~ .)

form_pred(terms(mpg ~ (.)^2, data = mtcars))
#>  [1] "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"   "gear"
#> [10] "carb"
form_pred(terms(~ (.)^2, data = mtcars))
#>  [1] "mpg"  "cyl"  "disp" "hp"   "drat" "wt"   "qsec" "vs"   "am"  
#> [10] "gear" "carb"
```
