.onLoad <- function(libname, pkgname) {

  if (requireNamespace("dplyr", quietly = TRUE)) {
    register_s3_method("dplyr", "arrange", "rset")
    register_s3_method("dplyr", "filter", "rset")
    register_s3_method("dplyr", "mutate", "rset")
    register_s3_method("dplyr", "rename", "rset")
    register_s3_method("dplyr", "select", "rset")
    register_s3_method("dplyr", "slice", "rset")
  }
  
  invisible()
  
}
