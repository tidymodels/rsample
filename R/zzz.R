# nocov start

.onLoad <- function(libname, pkgname) {
  vctrs::s3_register("dplyr::dplyr_reconstruct", "rset", method = dplyr_reconstruct_rset)
}

release_questions <- function() {
  c(
    "Have you added all new classes to `rset_subclasses` (in R/compat-vctrs-helpers.R)?",
    "Have you re-generated vctrs compatibility functions (using inst/generate_vctrs.R) and document()ed?"
  )
}

# nocov end
