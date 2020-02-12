## adapted from
## https://github.com/hadley/dtplyr/blob/2308ff25e88bb81fe84f9051e37ddd9d572189ee/R/compat-dplyr-0.6.0.R
## and based on
## https://github.com/tidyverse/googledrive/commit/95455812d2e0d6bdf92b5f6728e3265bf65d8467#diff-ba61d4f2ccd992868e27305a9ab68a3c

## function is called in .onLoad()

register_s3_method <- function(pkg, generic, class, fun = NULL) { # nocov start
  stopifnot(is_string(pkg))
  envir <- asNamespace(pkg)

  stopifnot(is_string(generic))
  stopifnot(is_string(class))
  if (is.null(fun)) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  }
  stopifnot(is.function(fun))

  if (pkg %in% loadedNamespaces()) {
    registerS3method(generic, class, fun, envir = envir)
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = envir)
    }
  )
} # nocov end

reset_rset <- function(x) {
  stopifnot(inherits(x, "data.frame"))
  structure(x, class = c("rset", "tbl_df", "tbl", "data.frame"))
}

is_rset <- function(x) {
  is_tibble(x) &&
    all("splits" %in% names(x)) &&
    length(grepl("^id", names(x))) > 0
}

#' @export
`[.rset` <- function(x, i, j, drop = FALSE) {
  maybe_rset(NextMethod())
}

# A list of attribute names in the various resampling functions. These
# could get stripped off by dplyr operations
rsample_att <- c("times", "apparent", "strata", "v", "repeats",
                 "group", "prop", "outside", "inside",
                 "initial", "assess", "cumulative", "skip"
                 )

maybe_rset <- function(x, extras = NULL, att = NULL) {
  if (is_rset(x)) {
    x <- reset_rset(x)

    ## possibly reset attributes that dplyr methods removed
    att <- att[names(att) %in% rsample_att]
    if (length(att) > 0) {
      missing_att <- setdiff(names(att), attributes(x))
      if (length(missing_att) > 0) {
        for (i in missing_att)
          attr(x, i) <- att[[i]]
      }
    }

    ## Add an missing classes
    if(length(extras) > 0)
      class(x) <- c(extras, class(x))
  } else {
    x <- as_tibble(x)
  }
  x
}

## rsample does not import any generics from dplyr,
## but if dplyr is loaded and main verbs are used on a
## `rset` object generated from the various resampling
## functions, we want to retain the `rset`` class (and
## any others) if it is proper to do so therefore these
## S3 methods are registered manually in .onLoad()

base_classes <- c("rset", class(tibble()))

arrange.rset <- function(.data, ...) {
  # Find out if there are extra classes beyond `rset` such
  # as `vfold_cv` or `bootstraps` and add them in after
  # the `dplyr` method is executed.
  extra_classes <- setdiff(class(.data), base_classes)
  orig_att <- attributes(.data)
  maybe_rset(NextMethod(), extras = extra_classes, att = orig_att)
}

filter.rset <- function(.data, ...) {
  extra_classes <- setdiff(class(.data), base_classes)
  orig_att <- attributes(.data)
  maybe_rset(NextMethod(), extras = extra_classes, att = orig_att)
}

# `mutate` appears to add rownames but remove other attributes. We'll
# add them back in.
mutate.rset <- function(.data, ...) {
  extra_classes <- setdiff(class(.data), base_classes)
  orig_att <- attributes(.data)
  maybe_rset(NextMethod(), extras = extra_classes, att = orig_att)
}

rename.rset <- function(.data, ...) {
  extra_classes <- setdiff(class(.data), base_classes)
  orig_att <- attributes(.data)
  maybe_rset(NextMethod(), extras = extra_classes, att = orig_att)
}

select.rset <- function(.data, ...) {
  extra_classes <- setdiff(class(.data), base_classes)
  orig_att <- attributes(.data)
  maybe_rset(NextMethod(), extras = extra_classes, att = orig_att)
}

slice.rset <- function(.data, ...) {
  extra_classes <- setdiff(class(.data), base_classes)
  orig_att <- attributes(.data)
  maybe_rset(NextMethod(), extras = extra_classes, att = orig_att)
}
