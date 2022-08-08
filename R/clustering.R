#' Cluster Cross-Validation
#'
#' Cluster cross-validation splits the data into V groups of
#'  disjointed sets using k-means clustering of some variables.
#'  A resample of the analysis data consists of V-1 of the
#'  folds/clusters while the assessment set contains the final fold/cluster. In
#'  basic cross-validation (i.e. no repeats), the number of resamples
#'  is equal to V.
#'
#' @details
#' The variables in the `vars` argument are used for k-means clustering of
#'  the data into disjointed sets or for hierarchical clustering of the data.
#'  These clusters are used as the folds for cross-validation. Depending on how
#'  the data are distributed, there may not be an equal number of points
#'  in each fold.
#'
#' You can optionally provide a custom function to `distance_function`. The
#' function should take a data frame (as created via `data[vars]`) and return
#' a [stats::dist()] object with distances between data points.
#'
#' You can optionally provide a custom function to `cluster_function`. The
#' function must take three arguments:
#' - `dists`, a [stats::dist()] object with distances between data points
#' - `v`, a length-1 numeric for the number of folds to create
#' - `...`, to pass any additional named arguments to your function
#'
#' The function should return a vector of cluster assignments of length
#' `nrow(data)`, with each element of the vector corresponding to the matching
#' row of the data frame.
#'
#' @param data A data frame to split into folds.
#' @param vars A vector of bare variable names to use to cluster the data.
#' @param v The number of partitions of the data set.
#' @param distance_function Which function should be used for distance calculations?
#' Defaults to [stats::dist()]. You can also provide your own
#' function; see `Details`.
#' @param cluster_function Which function should be used for clustering?
#' Options are either `"kmeans"` (to use [stats::kmeans()])
#' or `"hclust"` (to use [stats::hclust()]). You can also provide your own
#' function; see `Details`.
#' @param ... Extra arguments passed on to `cluster_function`.
#'
#' @return A tibble with classes `rset`, `tbl_df`, `tbl`, and `data.frame`.
#'  The results include a column for the data split objects and
#'  an identification variable `id`.
#'
#' @examplesIf rlang::is_installed("modeldata")
#' data(ames, package = "modeldata")
#' clustering_cv(ames, vars = c(Sale_Price, First_Flr_SF, Second_Flr_SF), v = 2)
#'
#' @rdname clustering_cv
#' @export
clustering_cv <- function(data,
                          vars,
                          v = 10,
                          distance_function = "dist",
                          cluster_function = c("kmeans", "hclust"),
                          ...) {
  if (!rlang::is_function(cluster_function)) {
    cluster_function <- rlang::arg_match(cluster_function)
  }

  vars <- tidyselect::eval_select(rlang::enquo(vars), data = data)
  if (rlang::is_empty(vars)) {
    rlang::abort("`vars` are required and must be variables in `data`.")
  }
  vars <- data[vars]
  dists <- rlang::exec(distance_function, vars)

  split_objs <- clustering_splits(
    data = data,
    dists = dists,
    v = v,
    cluster_function = cluster_function,
    ...
  )

  split_objs$splits <- map(split_objs$splits, rm_out)

  ## Save some overall information

  cv_att <- list(
    v = v,
    vars = names(vars),
    repeats = 1,
    distance_function = distance_function,
    cluster_function = cluster_function
  )

  new_rset(
    splits = split_objs$splits,
    ids = split_objs[, grepl("^id", names(split_objs))],
    attrib = cv_att,
    subclass = c("clustering_cv", "rset")
  )
}

clustering_splits <- function(data,
                              dists,
                              v = 10,
                              cluster_function = c("kmeans", "hclust"),
                              ...) {
  if (!rlang::is_function(cluster_function)) {
    cluster_function <- rlang::arg_match(cluster_function)
  }

  check_v(v, nrow(data), "rows", call = rlang::caller_env())
  n <- nrow(data)

  clusterer <- ifelse(
    rlang::is_function(cluster_function),
    "custom",
    cluster_function
  )
  folds <- switch(
    clusterer,
    "kmeans" = {
      clusters <- stats::kmeans(dists, centers = v, ...)
      clusters$cluster
    },
    "hclust" = {
      clusters <- stats::hclust(dists, ...)
      stats::cutree(clusters, k = v)
    },
    do.call(cluster_function, list(dists = dists, v = v, ...))
  )

  idx <- seq_len(n)
  indices <- split_unnamed(idx, folds)
  indices <- lapply(indices, default_complement, n = n)

  split_objs <- purrr::map(
    indices,
    make_splits,
    data = data,
    class = c("clustering_split")
  )
  tibble::tibble(
    splits = split_objs,
    id = names0(length(split_objs), "Fold")
  )
}
