# Cluster Cross-Validation

Cluster cross-validation splits the data into V groups of disjointed
sets using k-means clustering of some variables. A resample of the
analysis data consists of V-1 of the folds/clusters while the assessment
set contains the final fold/cluster. In basic cross-validation (i.e. no
repeats), the number of resamples is equal to V.

## Usage

``` r
clustering_cv(
  data,
  vars,
  v = 10,
  repeats = 1,
  distance_function = "dist",
  cluster_function = c("kmeans", "hclust"),
  ...
)
```

## Arguments

- data:

  A data frame.

- vars:

  A vector of bare variable names to use to cluster the data.

- v:

  The number of partitions of the data set.

- repeats:

  The number of times to repeat the clustered partitioning.

- distance_function:

  Which function should be used for distance calculations? Defaults to
  [`stats::dist()`](https://rdrr.io/r/stats/dist.html). You can also
  provide your own function; see `Details`.

- cluster_function:

  Which function should be used for clustering? Options are either
  `"kmeans"` (to use
  [`stats::kmeans()`](https://rdrr.io/r/stats/kmeans.html)) or
  `"hclust"` (to use
  [`stats::hclust()`](https://rdrr.io/r/stats/hclust.html)). You can
  also provide your own function; see `Details`.

- ...:

  Extra arguments passed on to `cluster_function`.

## Value

A tibble with classes `rset`, `tbl_df`, `tbl`, and `data.frame`. The
results include a column for the data split objects and an
identification variable `id`.

## Details

The variables in the `vars` argument are used for k-means clustering of
the data into disjointed sets or for hierarchical clustering of the
data. These clusters are used as the folds for cross-validation.
Depending on how the data are distributed, there may not be an equal
number of points in each fold.

You can optionally provide a custom function to `distance_function`. The
function should take a data frame (as created via `data[vars]`) and
return a [`stats::dist()`](https://rdrr.io/r/stats/dist.html) object
with distances between data points.

You can optionally provide a custom function to `cluster_function`. The
function must take three arguments:

- `dists`, a [`stats::dist()`](https://rdrr.io/r/stats/dist.html) object
  with distances between data points

- `v`, a length-1 numeric for the number of folds to create

- `...`, to pass any additional named arguments to your function

The function should return a vector of cluster assignments of length
`nrow(data)`, with each element of the vector corresponding to the
matching row of the data frame.

## Examples

``` r
data(ames, package = "modeldata")
clustering_cv(ames, vars = c(Sale_Price, First_Flr_SF, Second_Flr_SF), v = 2)
#> # 2-cluster cross-validation 
#> # A tibble: 2 × 2
#>   splits             id   
#>   <list>             <chr>
#> 1 <split [2489/441]> Fold1
#> 2 <split [441/2489]> Fold2
```
