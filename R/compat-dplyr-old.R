# These compat methods are for dplyr < 1.0.0.
# They are conditionally registered in `zzz.R` if an old version of dplyr is
# being used. Otherwise the methods in `compat-dplyr.R` are registered.
# Eventually these should be deprecated and dplyr 1.0.0 should be required.
# Only verbs that were previously supported in rsample are continually
# supported now. This means that there is more complete support when a user
# upgrades to dplyr >= 1.0.0. When support for dplyr < 1.0.0 is deprecated,
# move the tests in `compact-dplyr-old.R` to `compat-dplyr.R`. The methods
# are named with underscores between the generic and the class, not with dots,
# because when working interactively with `load_all()`, just having
# a function named `mutate.rset` will register a dplyr method internally in
# the package, even if you are on dplyr 1.0.0, which is highly confusing and
# undesirable. So we work around that by naming them in such a way that they
# are not picked up by S3 registration, and then we manually register them
# with the `method` argument of `s3_register()`.

dplyr_pre_1.0.0 <- function() {
  utils::packageVersion("dplyr") <= "0.8.5"
}

# Registered in `.onLoad()`
mutate_rset <- function(.data, ...) {
  out <- NextMethod()
  rset_maybe_reconstruct(out, .data)
}

# Registered in `.onLoad()`
arrange_rset <- function(.data, ...) {
  out <- NextMethod()
  rset_maybe_reconstruct(out, .data)
}

# Registered in `.onLoad()`
filter_rset <- function(.data, ...) {
  out <- NextMethod()
  rset_maybe_reconstruct(out, .data)
}

# Registered in `.onLoad()`
rename_rset <- function(.data, ...) {
  out <- NextMethod()
  rset_maybe_reconstruct(out, .data)
}

# Registered in `.onLoad()`
select_rset <- function(.data, ...) {
  out <- NextMethod()
  rset_maybe_reconstruct(out, .data)
}

# Registered in `.onLoad()`
slice_rset <- function(.data, ...) {
  out <- NextMethod()
  rset_maybe_reconstruct(out, .data)
}
