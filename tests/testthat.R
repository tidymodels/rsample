library(testthat)
library(rsample)

if (requireNamespace("xml2")) {
  test_check("rsample", reporter = MultiReporter$new(reporters = list(JunitReporter$new(file = "test-results.xml"), CheckReporter$new())))
} else {
  test_check("rsample")
}
