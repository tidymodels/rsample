test_that("grouped variants have the same classes as nongrouped outputs", {
  skip_if_not_installed("withr")
  grouped_variants <- grep("^group_", names(rset_subclasses), value = TRUE)

  for (x in grouped_variants) {

    grouped_variant <- do.call(x, list(data = test_data(), group = "y"))

    ungrouped_variant <- gsub("^group_", "", x)
    ungrouped_variant <- do.call(ungrouped_variant, list(data = test_data()))

    expect_true(
      all(class(ungrouped_variant) %in% class(grouped_variant))
    )

    expect_true(
      all(class(ungrouped_variant$splits[[1]]) %in% class(grouped_variant$splits[[1]]))
    )

  }

  # Test initial_split separately, as it doesn't return an rset
  grouped_variant <- group_initial_split(test_data(), y)
  ungrouped_variant <- initial_split(test_data())

  expect_true(
    all(class(ungrouped_variant) %in% class(grouped_variant))
  )

})

test_that("grouped variants are consistent across R sessions", {
  skip_if_not_installed("withr")
  grouped_variants <- grep("^group_", names(rset_subclasses), value = TRUE)

  for (x in grouped_variants) {
    withr::with_seed(
      123,
      rs <- do.call(x, list(data = dplyr::starwars, group = "name"))
    )
    expect_snapshot(
      analysis(rs$splits[[1]])
    )
  }
})
