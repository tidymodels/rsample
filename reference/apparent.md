# Sampling for the Apparent Error Rate

When building a model on a data set and re-predicting the same data, the
performance estimate from those predictions is often called the
"apparent" performance of the model. This estimate can be wildly
optimistic. "Apparent sampling" here means that the analysis and
assessment samples are the same. These resamples are sometimes used in
the analysis of bootstrap samples and should otherwise be avoided like
old sushi.

## Usage

``` r
apparent(data, ...)
```

## Arguments

- data:

  A data frame.

- ...:

  These dots are for future extensions and must be empty.

## Value

A tibble with a single row and classes `apparent`, `rset`, `tbl_df`,
`tbl`, and `data.frame`. The results include a column for the data split
objects and one column called `id` that has a character string with the
resample identifier.

## Examples

``` r
apparent(mtcars)
#> # Apparent sampling 
#> # A tibble: 1 × 2
#>   splits          id      
#>   <list>          <chr>   
#> 1 <split [32/32]> Apparent
```
