# Manual resampling

`manual_rset()` is used for constructing the most minimal rset possible.
It can be useful when you have custom rsplit objects built from
[`make_splits()`](https://rsample.tidymodels.org/reference/make_splits.md),
or when you want to create a new rset from splits contained within an
existing rset.

## Usage

``` r
manual_rset(splits, ids)
```

## Arguments

- splits:

  A list of `"rsplit"` objects. It is easiest to create these using
  [`make_splits()`](https://rsample.tidymodels.org/reference/make_splits.md).

- ids:

  A character vector of ids. The length of `ids` must be the same as the
  length of `splits`.

## Examples

``` r
df <- data.frame(x = c(1, 2, 3, 4, 5, 6))

# Create an rset from custom indices
indices <- list(
  list(analysis = c(1L, 2L), assessment = 3L),
  list(analysis = c(4L, 5L), assessment = 6L)
)

splits <- lapply(indices, make_splits, data = df)

manual_rset(splits, c("Split 1", "Split 2"))
#> # Manual resampling 
#> # A tibble: 2 × 2
#>   splits        id     
#>   <list>        <chr>  
#> 1 <split [2/1]> Split 1
#> 2 <split [2/1]> Split 2

# You can also use this to create an rset from a subset of an
# existing rset
resamples <- vfold_cv(mtcars)
best_split <- resamples[5, ]
manual_rset(best_split$splits, best_split$id)
#> # Manual resampling 
#> # A tibble: 1 × 2
#>   splits         id    
#>   <list>         <chr> 
#> 1 <split [29/3]> Fold05
```
