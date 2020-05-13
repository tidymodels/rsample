# nocov start

# From vctrs
on_package_load <- function(pkg, expr) {
  if (isNamespaceLoaded(pkg)) {
    expr
  } else {
    thunk <- function(...) expr
    setHook(packageEvent(pkg, "onLoad"), thunk)
  }
}

.onLoad <- function(libname, pkgname) {
  on_package_load("dplyr", {
    if (dplyr_pre_1.0.0()) {
      vctrs::s3_register("dplyr::mutate", "rset")
      vctrs::s3_register("dplyr::arrange", "rset")
      vctrs::s3_register("dplyr::filter", "rset")
      vctrs::s3_register("dplyr::rename", "rset")
      vctrs::s3_register("dplyr::select", "rset")
      vctrs::s3_register("dplyr::slice", "rset")
    } else {
      vctrs::s3_register("dplyr::dplyr_reconstruct", "rset")
    }
  })
}

# nocov end
