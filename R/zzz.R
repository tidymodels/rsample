# nocov start

.onLoad <- function(libname, pkgname) {
  vctrs::s3_register("dplyr::dplyr_reconstruct", "rset", method = dplyr_reconstruct_rset)
}

# nocov end
