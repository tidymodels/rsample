#' Spatial or Cluster Cross-Validation
#'
#' Spatial or cluster cross-validation splits the data into V groups of
#'  disjointed sets using k-means clustering of some variables, typically
#'  spatial coordinates. A resample of the analysis data consists of V-1 of the
#'  folds/clusters while the assessment set contains the final fold/cluster. In
#'  basic spatial cross-validation (i.e. no repeats), the number of resamples
#'  is equal to V.
#' @details
#' The variables in the `coords` argument are used for k-means clustering of
#'  the data into disjointed sets, as outlined in Brenning (2012). These
#'  clusters are used as the folds for cross-validation. Depending on how the
#'  data are distributed spatially, there may not be an equal number of points
#'  in each fold.
#'
#' When more than one repeat is requested, the basic V-fold cross-validation
#'  is conducted each time. For example, if three repeats are used with `v =
#'  10`, there are a total of 30 splits which are three groups of 10 that are
#'  generated separately.
#' @param data A data frame.
#' @param v The number of partitions of the data set.
#' @param repeats The number of times to repeat the V-fold partitioning.
#' @param coords A vector of variable names, typically spatial coordinates,
#'  to partition the data into disjointed sets via k-means clustering.
#' @param ... Extra arguments passed on to [stats::kmeans()].
#' @export
#' @return A tibble with classes `spatial_cv`, `rset`, `tbl_df`, `tbl`, and
#'  `data.frame`. The results include a column for the data split objects and
#'  one or more identification variables. For a single repeat, there will be
#'  one column called `id` that has a character string with the fold identifier.
#'  For repeats, `id` is the repeat number and an additional column called `id2`
#'  that contains the fold information (within repeat).
#'
#' @references
#'
#' A. Brenning, "Spatial cross-validation and bootstrap for the assessment of
#' prediction rules in remote sensing: The R package sperrorest," 2012 IEEE
#' International Geoscience and Remote Sensing Symposium, Munich, 2012,
#' pp. 5372-5375, doi: 10.1109/IGARSS.2012.6352393.
#'
#' @examples
#' data(ames, package = "modeldata")
#' spatial_cv(ames, v = 5, coords = c(Latitude, Longitude))
#' spatial_cv(ames, v = 5, repeats = 2, coords = c(Latitude, Longitude))
#'
#' @export
spatial_cv <- function(data, v = 10, repeats = 1, coords, ...) {

  coords <- tidyselect::eval_select(rlang::enquo(coords), data = data)

  if(is_empty(coords)) {
    rlang::abort("`coords` are required and must be variables in `data`.")
  }

  if (repeats == 1) {
    split_objs <- spatial_splits(data = data, v = v, coords = coords, ...)
  } else {
    for (i in 1:repeats) {
      tmp <- spatial_splits(data = data, v = v, coords = coords)
      tmp$id2 <- tmp$id
      tmp$id <- names0(repeats, "Repeat")[i]
      split_objs <- if (i == 1)
        tmp
      else
        rbind(split_objs, tmp)
    }
  }

  ## We remove the holdout indices since it will save space and we can
  ## derive them later when they are needed.

  split_objs$splits <- map(split_objs$splits, rm_out)

  ## Save some overall information

  cv_att <- list(v = v, repeats = repeats)

  new_rset(splits = split_objs$splits,
           ids = split_objs[, grepl("^id", names(split_objs))],
           attrib = cv_att,
           subclass = c("spatial_cv", "rset"))
}

spatial_splits <- function(data, v = 10, coords, ...) {

  if (!is.numeric(v) || length(v) != 1)
    rlang::abort("`v` must be a single integer.")

  n <- nrow(data)
  clusters <- kmeans(data[coords], centers = v, ...)
  folds <- clusters$cluster
  idx <- seq_len(n)
  indices <- split_unnamed(idx, folds)
  indices <- lapply(indices, vfold_complement, n = n)
  split_objs <- purrr::map(indices, make_splits, data = data, class = "spatial_split")
  tibble::tibble(splits = split_objs,
                 id = names0(length(split_objs), "Fold"))
}

#' @export
print.spatial_cv <- function(x, ...) {
  cat("# ", pretty(x), "\n")
  class(x) <- class(x)[!(class(x) %in% c("spatial_cv", "rset"))]
  print(x, ...)
}
