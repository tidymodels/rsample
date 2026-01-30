# Group V-Fold Cross-Validation

Group V-fold cross-validation creates splits of the data based on some
grouping variable (which may have more than a single row associated with
it). The function can create as many splits as there are unique values
of the grouping variable or it can create a smaller set of splits where
more than one group is left out at a time. A common use of this kind of
resampling is when you have repeated measures of the same subject.

## Usage

``` r
group_vfold_cv(
  data,
  group = NULL,
  v = NULL,
  repeats = 1,
  balance = c("groups", "observations"),
  ...,
  strata = NULL,
  pool = 0.1
)
```

## Arguments

- data:

  A data frame.

- group:

  A variable in `data` (single character or name) used for grouping
  observations with the same value to either the analysis or assessment
  set within a fold.

- v:

  The number of partitions of the data set. If left as `NULL` (the
  default), `v` will be set to the number of unique values in the
  grouping variable, creating "leave-one-group-out" splits.

- repeats:

  The number of times to repeat the V-fold partitioning.

- balance:

  If `v` is less than the number of unique groups, how should groups be
  combined into folds? Should be one of `"groups"`, which will assign
  roughly the same number of groups to each fold, or `"observations"`,
  which will assign roughly the same number of observations to each
  fold.

- ...:

  These dots are for future extensions and must be empty.

- strata:

  A variable in `data` (single character or name) used to conduct
  stratified sampling. When not `NULL`, each resample is created within
  the stratification variable. Numeric `strata` are binned into
  quartiles.

- pool:

  A proportion of data used to determine if a particular group is too
  small and should be pooled into another group. We do not recommend
  decreasing this argument below its default of 0.1 because of the
  dangers of stratifying groups that are too small.

## Value

A tibble with classes `group_vfold_cv`, `rset`, `tbl_df`, `tbl`, and
`data.frame`. The results include a column for the data split objects
and an identification variable.

## Examples

``` r
data(ames, package = "modeldata")

set.seed(123)
group_vfold_cv(ames, group = Neighborhood, v = 5)
#> # Group 5-fold cross-validation 
#> # A tibble: 5 × 2
#>   splits             id       
#>   <list>             <chr>    
#> 1 <split [2449/481]> Resample1
#> 2 <split [2642/288]> Resample2
#> 3 <split [2218/712]> Resample3
#> 4 <split [2367/563]> Resample4
#> 5 <split [2044/886]> Resample5
group_vfold_cv(
  ames,
  group = Neighborhood,
  v = 5,
  balance = "observations"
)
#> # Group 5-fold cross-validation 
#> # A tibble: 5 × 2
#>   splits             id       
#>   <list>             <chr>    
#> 1 <split [2366/564]> Resample1
#> 2 <split [2279/651]> Resample2
#> 3 <split [2361/569]> Resample3
#> 4 <split [2361/569]> Resample4
#> 5 <split [2353/577]> Resample5
group_vfold_cv(ames, group = Neighborhood, v = 5, repeats = 2)
#> # Group 5-fold cross-validation 
#> # A tibble: 10 × 3
#>    splits             id      id2      
#>    <list>             <chr>   <chr>    
#>  1 <split [2077/853]> Repeat1 Resample1
#>  2 <split [2215/715]> Repeat1 Resample2
#>  3 <split [2392/538]> Repeat1 Resample3
#>  4 <split [2574/356]> Repeat1 Resample4
#>  5 <split [2462/468]> Repeat1 Resample5
#>  6 <split [2269/661]> Repeat2 Resample1
#>  7 <split [2426/504]> Repeat2 Resample2
#>  8 <split [2354/576]> Repeat2 Resample3
#>  9 <split [2547/383]> Repeat2 Resample4
#> 10 <split [2124/806]> Repeat2 Resample5

# Leave-one-group-out CV
group_vfold_cv(ames, group = Neighborhood)
#> # Group 28-fold cross-validation 
#> # A tibble: 28 × 2
#>    splits             id        
#>    <list>             <chr>     
#>  1 <split [2663/267]> Resample01
#>  2 <split [2779/151]> Resample02
#>  3 <split [2691/239]> Resample03
#>  4 <split [2748/182]> Resample04
#>  5 <split [2928/2]>   Resample05
#>  6 <split [2920/10]>  Resample06
#>  7 <split [2902/28]>  Resample07
#>  8 <split [2837/93]>  Resample08
#>  9 <split [2859/71]>  Resample09
#> 10 <split [2858/72]>  Resample10
#> # ℹ 18 more rows

library(dplyr)
data(Sacramento, package = "modeldata")

city_strata <- Sacramento |>
  group_by(city) |>
  summarize(strata = mean(price)) |>
  reframe(city = city,
          strata = cut(strata, quantile(strata), include.lowest = TRUE))

sacramento_data <- Sacramento |>
  full_join(city_strata, by = "city")

group_vfold_cv(sacramento_data, city, strata = strata)
#> Warning: Leaving `v = NULL` while using stratification will set `v` to the
#> number of groups present in the least common stratum.
#> ℹ Set `v` explicitly to override this warning.
#> # Group 14-fold cross-validation 
#> # A tibble: 14 × 2
#>    splits            id        
#>    <list>            <chr>     
#>  1 <split [881/51]>  Resample01
#>  2 <split [434/498]> Resample02
#>  3 <split [905/27]>  Resample03
#>  4 <split [913/19]>  Resample04
#>  5 <split [917/15]>  Resample05
#>  6 <split [885/47]>  Resample06
#>  7 <split [926/6]>   Resample07
#>  8 <split [793/139]> Resample08
#>  9 <split [903/29]>  Resample09
#> 10 <split [896/36]>  Resample10
#> 11 <split [904/28]>  Resample11
#> 12 <split [917/15]>  Resample12
#> 13 <split [855/77]>  Resample13
#> 14 <split [897/35]>  Resample14
```
