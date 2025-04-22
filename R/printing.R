## The `pretty()` methods below are good for when you need to
## textually describe the resampling procedure. Note that they
## can have more than one element (in the case of nesting)

#' @export
pretty.vfold_cv <- function(x, ...) {
  details <- attributes(x)
  res <- paste0(details$v, "-fold cross-validation")
  if (details$repeats > 1) {
    res <- paste(res, "repeated", details$repeats, "times")
  }
  if (has_strata(details)) {
    res <- paste(res, "using stratification")
  }
  res
}

#' @export
pretty.loo_cv <- function(x, ...) {
  "Leave-one-out cross-validation"
}

#' @export
pretty.apparent <- function(x, ...) {
  "Apparent sampling"
}

#' @export
pretty.rolling_origin <- function(x, ...) {
  "Rolling origin forecast resampling"
}

#' @export
pretty.sliding_window <- function(x, ...) {
  "Sliding window resampling"
}

#' @export
pretty.sliding_index <- function(x, ...) {
  "Sliding index resampling"
}

#' @export
pretty.sliding_period <- function(x, ...) {
  "Sliding period resampling"
}

#' @export
pretty.mc_cv <- function(x, ...) {
  details <- attributes(x)
  res <- paste0(
    "Monte Carlo cross-validation (",
    signif(details$prop, 2),
    "/",
    signif(1 - details$prop, 2),
    ") with ",
    details$times,
    " resamples"
  )
  if (has_strata(details)) {
    res <- paste(res, " using stratification")
  }
  res
}

#' @export
pretty.validation_set <- function(x, ...) {
  details <- attributes(x)
  res <- paste0(
    "Validation Set (",
    signif(details$prop[1] / sum(details$prop), 2),
    "/",
    signif(details$prop[2] / sum(details$prop), 2),
    ")"
  )
  res
}

#' @export
pretty.validation_split <- function(x, ...) {
  details <- attributes(x)
  res <- paste0(
    "Validation Set Split (",
    signif(details$prop, 2),
    "/",
    signif(1 - details$prop, 2),
    ") "
  )
  if (has_strata(details)) {
    res <- paste(res, "using stratification")
  }
  res
}

#' @export
pretty.group_validation_split <- function(x, ...) {
  details <- attributes(x)
  res <- paste0(
    "Group Validation Set Split (",
    signif(details$prop, 2),
    "/",
    signif(1 - details$prop, 2),
    ") "
  )
  if (has_strata(details)) {
    res <- paste0(res, "using stratification")
  }
  res
}

#' @export
pretty.nested_cv <- function(x, ...) {
  details <- attributes(x)

  if (is_call(details$outside)) {
    class(x) <- class(x)[!(class(x) == "nested_cv")]
    outer_label <- pretty(x)
  } else {
    outer_label <- paste0("`", deparse(details$outside), "`")
  }

  inner_label <- if (is_call(details$inside)) {
    pretty(x$inner_resamples[[1]])
  } else {
    paste0("`", deparse(details$inside), "`")
  }

  res <- c(
    "Nested resampling:",
    paste(" outer:", outer_label),
    paste(" inner:", inner_label)
  )
  res
}

#' @export
pretty.bootstraps <- function(x, ...) {
  details <- attributes(x)
  res <- "Bootstrap sampling"
  if (has_strata(details)) {
    res <- paste(res, "using stratification")
  }
  if (details$apparent) {
    res <- paste(res, "with apparent sample")
  }
  res
}

#' @export
pretty.group_bootstraps <- function(x, ...) {
  details <- attributes(x)
  res <- "Group bootstrap sampling"
  if (has_strata(details)) {
    res <- paste(res, "using stratification")
  }
  if (details$apparent) {
    res <- paste(res, "with apparent sample")
  }
  res
}


#' @export
pretty.permutations <- function(x, ...) {
  details <- attributes(x)
  res <- "Permutation sampling"
  if (details$apparent) {
    res <- paste(res, "with apparent sample")
  }
  res
}

#' @export
pretty.group_vfold_cv <- function(x, ...) {
  details <- attributes(x)
  paste0("Group ", details$v, "-fold cross-validation")
}

#' @export
pretty.group_mc_cv <- function(x, ...) {
  details <- attributes(x)
  res <- paste0(
    "Group Monte Carlo cross-validation (",
    signif(details$prop, 2),
    "/",
    signif(1 - details$prop, 2),
    ") with ",
    details$times,
    " resamples "
  )

  if (has_strata(details)) {
    res <- paste0(res, "using stratification")
  }

  res
}

#' @export
pretty.manual_rset <- function(x, ...) {
  "Manual resampling"
}

#' @export
pretty.clustering_cv <- function(x, ...) {
  details <- attributes(x)
  paste0(details$v, "-cluster cross-validation")
}

# The print methods below control the display of rset objects

#' @export
print.apparent <- function(x, ...) {
  cat("#", pretty(x), "\n")
  class(x) <- class(x)[!(class(x) %in% c("apparent", "rset"))]
  print(x, ...)
}

#' @export
print.bootstraps <- function(x, ...) {
  cat("#", pretty(x), "\n")
  class(x) <- class(x)[!(class(x) %in% c("bootstraps", "rset"))]
  print(x, ...)
}

#' @export
print.group_bootstraps <- function(x, ...) {
  cat("#", pretty(x), "\n")
  class(x) <- class(x)[
    !(class(x) %in% c("group_bootstraps", "bootstraps", "group_rset", "rset"))
  ]
  print(x, ...)
}

#' @export
print.group_vfold_cv <- function(x, ...) {
  cat("#", pretty(x), "\n")
  class(x) <- class(x)[
    !(class(x) %in% c("group_vfold_cv", "vfold_cv", "group_rset", "rset"))
  ]
  print(x, ...)
}

#' @export
print.initial_split <- function(x, ...) {
  out_char <-
    if (is_missing_out_id(x)) {
      paste(length(complement(x)))
    } else {
      paste(length(x$out_id))
    }

  cat("<Training/Testing/Total>\n")
  cat("<", length(x$in_id), "/", out_char, "/", nrow(x$data), ">\n", sep = "")
}

#' @export
print.loo_cv <- function(x, ...) {
  cat("#", pretty(x), "\n")
  class(x) <- class(x)[!(class(x) %in% c("loo_cv", "rset"))]
  print(x, ...)
}

#' @export
print.manual_rset <- function(x, ...) {
  cat("#", pretty(x), "\n")
  class(x) <- class(x)[!(class(x) %in% c("manual_rset", "rset"))]
  print(x, ...)
}

#' @export
print.mc_cv <- function(x, ...) {
  cat("#", pretty(x), "\n")
  class(x) <- class(x)[!(class(x) %in% c("mc_cv", "rset"))]
  print(x, ...)
}

#' @export
print.nested_cv <- function(x, ...) {
  char_x <- paste("#", pretty(x))
  cat(char_x, sep = "\n")
  class(x) <- class(tibble())
  print(x, ...)
}


#' @export
print.permutations <- function(x, ...) {
  shuffled_cols <- paste(names(attr(x, "col_id")), collapse = ", ")
  cat("#", pretty(x), "\n")
  cat("# Permuted columns: [", shuffled_cols, "] \n", sep = "")
  class(x) <- class(x)[!(class(x) %in% c("permutations", "rset"))]
  print(x, ...)
}

#' @export
print.rolling_origin <- function(x, ...) {
  cat("#", pretty(x), "\n")
  class(x) <- class(x)[!(class(x) %in% c("rolling_origin", "rset"))]
  print(x, ...)
}

#' @export
print.sliding_window <- function(x, ...) {
  cat("#", pretty(x), "\n")
  class(x) <- class(x)[!(class(x) %in% c("sliding_window", "rset"))]
  print(x, ...)
}

#' @export
print.sliding_index <- function(x, ...) {
  cat("#", pretty(x), "\n")
  class(x) <- class(x)[!(class(x) %in% c("sliding_index", "rset"))]
  print(x, ...)
}

#' @export
print.sliding_period <- function(x, ...) {
  cat("#", pretty(x), "\n")
  class(x) <- class(x)[!(class(x) %in% c("sliding_period", "rset"))]
  print(x, ...)
}

#' @export
print.validation_split <- function(x, ...) {
  cat("#", pretty(x), "\n")
  class(x) <- class(x)[!(class(x) %in% c("validation_split", "rset"))]
  print(x, ...)
}

#' @export
print.group_validation_split <- function(x, ...) {
  cat("#", pretty(x), "\n")
  class(x) <- class(x)[
    !(class(x) %in%
      c("group_validation_split", "validation_split", "group_rset", "rset"))
  ]
  print(x, ...)
}


#' @export
print.val_split <- function(x, ...) {
  if (is_missing_out_id(x)) {
    out_char <- paste(length(complement(x)))
  } else {
    out_char <- paste(length(x$out_id))
  }

  cat("<Training/Validation/Total>\n")
  cat("<", length(x$in_id), "/", out_char, "/", nrow(x$data), ">\n", sep = "")
}

#' @export
print.group_mc_cv <- function(x, ...) {
  cat("#", pretty(x), "\n")
  class(x) <- class(x)[
    !(class(x) %in% c("group_mc_cv", "group_rset", "mc_cv", "rset"))
  ]
  print(x, ...)
}

#' @export
print.vfold_cv <- function(x, ...) {
  cat("# ", pretty(x), "\n")
  class(x) <- class(x)[!(class(x) %in% c("vfold_cv", "rset"))]
  print(x, ...)
}

#' @export
print.clustering_cv <- function(x, ...) {
  cat("#", pretty(x), "\n")
  class(x) <- class(x)[!(class(x) %in% c("clustering_cv", "rset"))]
  print(x, ...)
}

has_strata <- function(x) {
  !is.null(x$strata) && !identical(x$strata, FALSE)
}
