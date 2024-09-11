#' Convert Resampling Objects to Other Formats
#'
#' These functions can convert resampling objects between
#'  \pkg{rsample} and \pkg{caret}.
#'
#' @param object An `rset` object. Currently,
#'  `nested_cv` is not supported.
#' @return `rsample2caret` returns a list that mimics the
#'  `index` and `indexOut` elements of a
#'  `trainControl` object. `caret2rsample` returns an
#'  `rset` object of the appropriate class.
#' @export
rsample2caret <- function(object, data = c("analysis", "assessment")) {
  if (!inherits(object, "rset")) {
    cli_abort("{.arg object} must be an {.cls rset}.")
  }
  data <- rlang::arg_match(data)
  in_ind <- purrr::map(object$splits, as.integer, data = "analysis")
  names(in_ind) <- labels(object)
  out_ind <- purrr::map(object$splits, as.integer, data = "assessment")
  names(out_ind) <- names(in_ind)
  list(index = in_ind, indexOut = out_ind)
}

#' @rdname rsample2caret
#' @param ctrl An object produced by `trainControl` that has
#'  had the `index` and `indexOut` elements populated by
#'  integers. One method of getting this is to extract the
#'  `control` objects from an object produced by `train`.
#' @param data The data that was originally used to produce the
#'  `ctrl` object.
#' @export
caret2rsample <- function(ctrl, data = NULL) {
  if (is.null(data)) {
    cli_abort("Must supply original data")
  }
  if (!any(names(ctrl) == "index")) {
    cli_abort("{.arg ctrl} should have an element {.val index}")
  }
  if (!any(names(ctrl) == "indexOut")) {
    cli_abort("{.arg ctrl} should have an element {.val indexOut}")
  }
  if (is.null(ctrl$index)) {
    cli_abort("{.val ctrl$index} should be populated with integers")
  }
  if (is.null(ctrl$indexOut)) {
    cli_abort("{.val ctrl$indexOut} should be populated with integers")
  }

  indices <- purrr::map2(ctrl$index, ctrl$indexOut, extract_int)
  id_data <- names(indices)
  indices <- unname(indices)
  indices <- purrr::map(indices, add_data, y = data)
  indices <-
    map(indices, add_rsplit_class, cl = map_rsplit_method(ctrl$method))
  indices <- tibble::tibble(splits = indices)
  if (ctrl$method %in% c("repeatedcv", "adaptive_cv")) {
    id_data <- strsplit(id_data, split = ".", fixed = TRUE)
    id_data <- tibble::tibble(
      id = vapply(id_data, function(x) {
        x[2]
      }, character(1)),
      id2 = vapply(id_data, function(x) {
        x[1]
      }, character(1))
    )
  } else {
    id_data <- tibble::tibble(id = id_data)
  }

  new_rset(
    splits = indices$splits,
    ids = id_data[, grepl("^id", names(id_data))],
    attrib = map_attr(ctrl),
    subclass = c(map_rset_method(ctrl$method), "rset")
  )
}

extract_int <- function(x, y) {
  list(in_id = x, out_id = y)
}

add_data <- function(x, y) {
  c(list(data = y), x)
}

add_rsplit_class <- function(x, cl) {
  class(x) <- c("rsplit", cl)
  x
}

add_rset_class <- function(x, cl) {
  class(x) <- c(cl, "rset", "tbl_df", "tbl", "data.frame")
  x
}

map_rsplit_method <- function(method) {
  out <- switch(
    method,
    cv = ,
    repeatedcv = ,
    adaptive_cv = "vfold_split",
    boot = ,
    boot_all = ,
    boot632 = ,
    optimism_boot = ,
    adaptive_boot = "boot_split",
    LOOCV = "loo_split",
    LGOCV = ,
    adaptive_LGOCV = "mc_split",
    timeSlice = "rof_split",
    "error"
  )
  if (out == "error") {
    cli_abort(
      "Resampling method {.arg method} cannot be converted into an {.cls rsplit} object"
    )
  }
  out
}

map_rset_method <- function(method) {
  out <- switch(
    method,
    cv = ,
    repeatedcv = ,
    adaptive_cv = "vfold_cv",
    boot = ,
    boot_all = ,
    boot632 = ,
    optimism_boot = ,
    adaptive_boot = "bootstraps",
    LOOCV = "loo_cv",
    LGOCV = ,
    adaptive_LGOCV = "mc_cv",
    timeSlice = "rolling_origin",
    "error"
  )
  if (out == "error") {
    cli_abort("Resampling method {.arg method} cannot be converted into an {.cls rset} object")
  }
  out
}


map_attr <- function(object) {
  if (grepl("cv$", object$method)) {
    out <- list(
      v = object$number,
      repeats = ifelse(!is.na(object$repeats),
                       object$repeats, 1
      ),
      strata = TRUE
    )
  } else if (grepl("boot", object$method)) {
    out <- list(
      times = object$number,
      apparent = FALSE,
      strata = FALSE
    )
  } else if (grepl("LGOCV$", object$method)) {
    out <- list(
      times = object$number,
      prop = object$p,
      strata = FALSE
    )
  } else if (object$method == "LOOCV") {
    out <- list()
  } else if (object$method == "timeSlice") {
    out <- list(
      initial = object$initialWindow,
      assess = object$horizon,
      cumulative = !object$fixedWindow,
      skip = object$skip
    )
  } else {
    cli_abort("Method {.arg object$method} cannot be converted")
  }
  out
}
