.onLoad <- function(libname, pkgname) {

  if (requireNamespace("dplyr", quietly = TRUE)) {
    register_s3_method("dplyr", "arrange", "rset")
    register_s3_method("dplyr", "filter", "rset")
    register_s3_method("dplyr", "mutate", "rset")
    register_s3_method("dplyr", "rename", "rset")
    register_s3_method("dplyr", "select", "rset")
    register_s3_method("dplyr", "slice", "rset")
  }

  # Remove once tibble has implemented the methods
  if (rlang::is_installed("tibble") && !rlang::env_has(asNamespace("tibble"), "vec_ptype2.tbl_df")) {
    vctrs::s3_register("vctrs::vec_ptype2.tbl_df", "rset")
    vctrs::s3_register("vctrs::vec_ptype2.rset", "tbl_df")
    vctrs::s3_register("vctrs::vec_cast.rset", "tbl_df")
  }

  invisible()

}
