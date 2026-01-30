# Extending rsample with new rset subclasses

`rset_reconstruct()` encapsulates the logic for allowing new rset
subclasses to work properly with vctrs (through
[`vctrs::vec_restore()`](https://vctrs.r-lib.org/reference/vec_proxy.html))
and dplyr (through
[`dplyr::dplyr_reconstruct()`](https://dplyr.tidyverse.org/reference/dplyr_extending.html)).
It is intended to be a developer tool, and is not required for normal
usage of rsample.

## Usage

``` r
rset_reconstruct(x, to)
```

## Arguments

- x:

  A data frame to restore to an rset subclass.

- to:

  An rset subclass to restore to.

## Value

`x` restored to the rset subclass of `to`.

## Details

rset objects are considered "reconstructable" after a vctrs/dplyr
operation if:

- `x` and `to` both have an identical column named `"splits"` (column
  and row order do not matter).

- `x` and `to` both have identical columns prefixed with `"id"` (column
  and row order do not matter).

## Examples

``` r
to <- bootstraps(mtcars, times = 25)

# Imitate a vctrs/dplyr operation,
# where the class might be lost along the way
x <- tibble::as_tibble(to)

# Say we added a new column to `x`. Here we mock a `mutate()`.
x$foo <- "bar"

# This is still reconstructable to `to`
rset_reconstruct(x, to)
#> # Bootstrap sampling 
#> # A tibble: 25 × 3
#>    splits          id          foo  
#>    <list>          <chr>       <chr>
#>  1 <split [32/13]> Bootstrap01 bar  
#>  2 <split [32/8]>  Bootstrap02 bar  
#>  3 <split [32/13]> Bootstrap03 bar  
#>  4 <split [32/12]> Bootstrap04 bar  
#>  5 <split [32/12]> Bootstrap05 bar  
#>  6 <split [32/14]> Bootstrap06 bar  
#>  7 <split [32/11]> Bootstrap07 bar  
#>  8 <split [32/12]> Bootstrap08 bar  
#>  9 <split [32/15]> Bootstrap09 bar  
#> 10 <split [32/12]> Bootstrap10 bar  
#> # ℹ 15 more rows

# Say we lose the first row
x <- x[-1, ]

# This is no longer reconstructable to `to`, as `x` is no longer an rset
# bootstraps object with 25 bootstraps if one is lost!
rset_reconstruct(x, to)
#> # A tibble: 24 × 3
#>    splits          id          foo  
#>    <list>          <chr>       <chr>
#>  1 <split [32/8]>  Bootstrap02 bar  
#>  2 <split [32/13]> Bootstrap03 bar  
#>  3 <split [32/12]> Bootstrap04 bar  
#>  4 <split [32/12]> Bootstrap05 bar  
#>  5 <split [32/14]> Bootstrap06 bar  
#>  6 <split [32/11]> Bootstrap07 bar  
#>  7 <split [32/12]> Bootstrap08 bar  
#>  8 <split [32/15]> Bootstrap09 bar  
#>  9 <split [32/12]> Bootstrap10 bar  
#> 10 <split [32/11]> Bootstrap11 bar  
#> # ℹ 14 more rows
```
