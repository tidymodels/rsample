# Convert Resampling Objects to Other Formats

These functions can convert resampling objects between rsample and
caret.

## Usage

``` r
rsample2caret(object, data = c("analysis", "assessment"))

caret2rsample(ctrl, data = NULL)
```

## Arguments

- object:

  An `rset` object. Currently,
  [`nested_cv()`](https://rsample.tidymodels.org/dev/reference/nested_cv.md)
  is not supported.

- data:

  The data that was originally used to produce the `ctrl` object.

- ctrl:

  An object produced by `caret::trainControl()` that has had the `index`
  and `indexOut` elements populated by integers. One method of getting
  this is to extract the `control` objects from an object produced by
  `train`.

## Value

`rsample2caret()` returns a list that mimics the `index` and `indexOut`
elements of a `trainControl` object. `caret2rsample()` returns an `rset`
object of the appropriate class.
