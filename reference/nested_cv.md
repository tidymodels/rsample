# Nested or Double Resampling

`nested_cv()` can be used to take the results of one resampling
procedure and conduct further resamples within each split. Any type of
resampling used in rsample can be used.

## Usage

``` r
nested_cv(data, outside, inside)
```

## Arguments

- data:

  A data frame.

- outside:

  The initial resampling specification. This can be an already created
  object or an expression of a new object (see the examples below). If
  the latter is used, the `data` argument does not need to be specified
  and, if it is given, will be ignored.

- inside:

  An expression for the type of resampling to be conducted within the
  initial procedure.

## Value

An tibble with `nested_cv` class and any other classes that outer
resampling process normally contains. The results include a column for
the outer data split objects, one or more `id` columns, and a column of
nested tibbles called `inner_resamples` with the additional resamples.

## Details

It is a bad idea to use bootstrapping as the outer resampling procedure
(see the example below)

## Examples

``` r
## Using expressions for the resampling procedures:
nested_cv(mtcars, outside = vfold_cv(v = 3), inside = bootstraps(times = 5))
#> # Nested resampling:
#> #  outer: 3-fold cross-validation
#> #  inner: Bootstrap sampling
#> # A tibble: 3 × 3
#>   splits          id    inner_resamples
#>   <list>          <chr> <list>         
#> 1 <split [21/11]> Fold1 <boot [5 × 2]> 
#> 2 <split [21/11]> Fold2 <boot [5 × 2]> 
#> 3 <split [22/10]> Fold3 <boot [5 × 2]> 

## Using an existing object:
folds <- vfold_cv(mtcars)
nested_cv(mtcars, folds, inside = bootstraps(times = 5))
#> # Nested resampling:
#> #  outer: `folds`
#> #  inner: Bootstrap sampling
#> # A tibble: 10 × 3
#>    splits         id     inner_resamples
#>    <list>         <chr>  <list>         
#>  1 <split [28/4]> Fold01 <boot [5 × 2]> 
#>  2 <split [28/4]> Fold02 <boot [5 × 2]> 
#>  3 <split [29/3]> Fold03 <boot [5 × 2]> 
#>  4 <split [29/3]> Fold04 <boot [5 × 2]> 
#>  5 <split [29/3]> Fold05 <boot [5 × 2]> 
#>  6 <split [29/3]> Fold06 <boot [5 × 2]> 
#>  7 <split [29/3]> Fold07 <boot [5 × 2]> 
#>  8 <split [29/3]> Fold08 <boot [5 × 2]> 
#>  9 <split [29/3]> Fold09 <boot [5 × 2]> 
#> 10 <split [29/3]> Fold10 <boot [5 × 2]> 

## The dangers of outer bootstraps:
set.seed(2222)
bad_idea <- nested_cv(mtcars,
  outside = bootstraps(times = 5),
  inside = vfold_cv(v = 3)
)
#> Warning: Using bootstrapping as the outer resample is dangerous since the inner resample might have the same data point in both the analysis and assessment set.

first_outer_split <- get_rsplit(bad_idea, 1)
outer_analysis <- analysis(first_outer_split)
sum(grepl("Camaro Z28", rownames(outer_analysis)))
#> [1] 3

## For the 3-fold CV used inside of each bootstrap, how are the replicated
## `Camaro Z28` data partitioned?
first_inner_split <- get_rsplit(bad_idea$inner_resamples[[1]], 1)
inner_analysis <- analysis(first_inner_split)
inner_assess <- assessment(first_inner_split)

sum(grepl("Camaro Z28", rownames(inner_analysis)))
#> [1] 1
sum(grepl("Camaro Z28", rownames(inner_assess)))
#> [1] 2
```
