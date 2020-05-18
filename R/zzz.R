# nocov start

.onLoad <- function(libname, pkgname) {
  if (dplyr_pre_1.0.0()) {
    vctrs::s3_register("dplyr::mutate", "rset", method = mutate_rset)
    vctrs::s3_register("dplyr::arrange", "rset", method = arrange_rset)
    vctrs::s3_register("dplyr::filter", "rset", method = filter_rset)
    vctrs::s3_register("dplyr::rename", "rset", method = rename_rset)
    vctrs::s3_register("dplyr::select", "rset", method = select_rset)
    vctrs::s3_register("dplyr::slice", "rset", method = slice_rset)
  } else {
    vctrs::s3_register("dplyr::dplyr_reconstruct", "rset", method = dplyr_reconstruct_rset)
  }
}

# nocov end
