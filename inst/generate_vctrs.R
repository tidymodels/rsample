# This file generates helpers for compatibility with vctrs
# and is not a part of the rsample package proper

devtools::load_all()
read_utf8 <- function(x) base::readLines(x, encoding = "UTF-8", warn = FALSE)

template <- read_utf8("inst/vctrs_template.R")

for (name in names(rset_subclasses)) {
  generated_template <- whisker::whisker.render(template)
  generated_template <- c(
    "# This file was generated, do not edit by hand",
    "# Please edit inst/generate_vctrs.R instead",
    generated_template
  )
  writeLines(
    generated_template,
    file.path("R", paste0("z-compat-vctrs-", name, ".R"))
  )
}
