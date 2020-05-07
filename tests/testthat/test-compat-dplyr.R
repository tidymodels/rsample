library(dplyr)

# ------------------------------------------------------------------------------
# dplyr_reconstruct()

test_that("reconstructing to an rset returns a bare tibble", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(dplyr_reconstruct(x, x))
    expect_identical(dplyr_reconstruct(x, x), rset_strip(x))
  }
})

test_that("reconstructing retains extra attributes", {
  for (x in rset_subclasses) {
    x2 <- x
    attr(x2, "foo") <- "bar"

    result <- dplyr_reconstruct(x, x2)

    expect_identical(attr(result, "foo"), "bar")
  }
})

# ------------------------------------------------------------------------------
# dplyr_col_modify()

test_that("can add columns and retain rset class", {
  for (x in rset_subclasses) {
    cols <- list(x = rep(1, vec_size(x)))

    result <- dplyr_col_modify(x, cols)

    expect_s3_class_rset(result)
    expect_identical(result$x, cols$x)
  }
})

test_that("modifying rset columns removes rset class", {
  for (x in rset_subclasses) {
    cols <- list(splits = rep(1, vec_size(x)))

    result <- dplyr_col_modify(x, cols)

    expect_s3_class_bare_tibble(result)
    expect_identical(result$splits, cols$splits)
  }

  for (x in rset_subclasses) {
    cols <- list(id = rep(1, vec_size(x)))

    result <- dplyr_col_modify(x, cols)

    expect_s3_class_bare_tibble(result)
    expect_identical(result$id, cols$id)
  }
})

# ------------------------------------------------------------------------------
# mutate()

test_that("mutate() can keep rset class", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(mutate(x, x = 1))
    expect_identical(mutate(x, x = 1)$x, rep(1, vec_size(x)))
  }
})

test_that("mutate() drops rset class if any rset columns are touched", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(mutate(x, splits = 1))
    expect_s3_class_bare_tibble(mutate(x, id = 1))

    expect_identical(mutate(x, splits = 1)$splits, rep(1, vec_size(x)))
    expect_identical(mutate(x, id = 1)$id, rep(1, vec_size(x)))
  }
})

test_that("adding a column that looks like an `id` drops the class", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(mutate(x, id20 = 1))
  }
})

# ------------------------------------------------------------------------------
# select()

test_that("select() can keep rset class", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(select(x, splits, id))
  }
})

test_that("select() drops rset class if any rset columns aren't selected", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(select(x, id))
    expect_s3_class_bare_tibble(select(x, splits))
  }
})

# ------------------------------------------------------------------------------
# slice()

test_that("slice() drops rset class", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(slice(x))
    expect_s3_class_bare_tibble(slice(x, 1:5))
  }
})

# ------------------------------------------------------------------------------
# arrange()

test_that("arrange() drops rset class", {
  skip("Until dplyr/5205")
  # https://github.com/tidyverse/dplyr/pull/5205

  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(arrange(x))
    expect_s3_class_bare_tibble(arrange(x, splits))
  }
})

# ------------------------------------------------------------------------------
# filter()

test_that("filter() drops rset class", {
  skip("Until dplyr/5206")
  # https://github.com/tidyverse/dplyr/pull/5206

  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(filter(x))
    expect_s3_class_bare_tibble(filter(x, is.character(id)))
  }
})

# ------------------------------------------------------------------------------
# summarise()

test_that("summarise() always drops the rset class", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(summarise(x, y = 1))
    expect_s3_class_bare_tibble(summarise(x, splits = splits[1], id = id[1]))
  }
})

# ------------------------------------------------------------------------------
# group_by()

test_that("group_by() always returns a bare grouped-df or bare tibble", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(group_by(x))
    expect_s3_class(group_by(x, splits), c("grouped_df", "tbl_df", "tbl", "data.frame"), exact = TRUE)
  }
})

# ------------------------------------------------------------------------------
# ungroup()

test_that("ungroup() returns a rset", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(ungroup(x))
  }
})

# ------------------------------------------------------------------------------
# rename()

# most of these tests are really covered by `names<-.rset` tests.

test_that("renaming can keep the rset class", {
  for (x in rset_subclasses) {
    x <- mutate(x, a = 1)
    x <- rename(x, b = a)
    expect_s3_class_rset(x)
  }
})

test_that("renaming `id` can keep the rset class", {
  for (x in rset_subclasses) {
    x <- rename(x, id2 = id)
    expect_s3_class_rset(x)
  }
})

test_that("renaming `id` to a non-id name drops the rset class", {
  for (x in rset_subclasses) {
    x <- rename(x, stuff = id)
    expect_s3_class_bare_tibble(x)
  }
})

# ------------------------------------------------------------------------------
# relocate()

test_that("can relocate() and keep the class", {
  for (x in rset_subclasses) {
    x <- relocate(x, id)
    expect_s3_class_rset(x)
  }
})

# ------------------------------------------------------------------------------
# distinct()

test_that("distinct() drops the class", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(distinct(x))
    expect_s3_class_bare_tibble(distinct(x, splits))
  }
})

# ------------------------------------------------------------------------------
# joins

test_that("joins drops the class", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(left_join(x, x, by = names(x)))
    expect_s3_class_bare_tibble(right_join(x, x, by = names(x)))
    expect_s3_class_bare_tibble(full_join(x, x, by = names(x)))
    expect_s3_class_bare_tibble(inner_join(x, x, by = names(x)))
    expect_s3_class_bare_tibble(semi_join(x, x, by = names(x)))
    expect_s3_class_bare_tibble(anti_join(x, x, by = names(x)))
    expect_s3_class_bare_tibble(nest_join(x, x, by = names(x)))
  }
})

# ------------------------------------------------------------------------------
# bind_rows()

test_that("bind_rows() drops the class", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(bind_rows(x))
    expect_s3_class_bare_tibble(bind_rows(x, x))
  }
})

# ------------------------------------------------------------------------------
# bind_cols()

test_that("bind_cols() drops the class", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(bind_cols(x))
    expect_s3_class_bare_tibble(bind_cols(x, x))
  }
})
