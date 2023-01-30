library(dplyr)
skip_if_not_installed("withr")
# ------------------------------------------------------------------------------
# dplyr_reconstruct()

test_that("dplyr_reconstruct() returns an rset subclass if `x` retains rset structure", {
  for (x in rset_subclasses) {
    expect_identical(dplyr_reconstruct(x, x), x)
    expect_s3_class_rset(dplyr_reconstruct(x, x))
  }
})

test_that("dplyr_reconstruct() returns bare tibble if `x` loses rset structure", {
  for (x in rset_subclasses) {
    col <- x[1]
    row <- x[0, ]

    expect_s3_class_bare_tibble(dplyr_reconstruct(col, x))
    expect_s3_class_bare_tibble(dplyr_reconstruct(row, x))
  }
})

test_that("dplyr_reconstruct() retains extra attributes of `to` when not falling back", {
  for (x in rset_subclasses) {
    to <- x
    attr(to, "foo") <- "bar"

    x_tbl <- x[1]

    expect_identical(attr(dplyr_reconstruct(x, to), "foo"), "bar")
    expect_identical(attr(dplyr_reconstruct(x_tbl, to), "foo"), NULL)

    expect_s3_class_rset(dplyr_reconstruct(x, to))
    expect_s3_class_bare_tibble(dplyr_reconstruct(x_tbl, to))
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

test_that("replacing rset columns with the exact same column retains rset class", {
  for (x in rset_subclasses) {
    cols <- list(splits = x$splits)

    result <- dplyr_col_modify(x, cols)

    expect_s3_class_rset(result)
    expect_identical(result, x)
  }
})

test_that("for nested_cv, `inner_resamples` is also a protected column", {
  x <- rset_subclasses$nested_cv
  cols <- list(inner_resamples = rep(1, vec_size(x)))
  expect_s3_class_bare_tibble(dplyr_col_modify(x, cols))
})

# ------------------------------------------------------------------------------
# dplyr_row_slice()

test_that("row slicing generally removes the rset subclass", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(dplyr_row_slice(x, 0))
  }
})

test_that("row slicing and duplicating any rows removes the rset subclass", {
  # Remove rsets with only 1 row
  subclasses <- rset_subclasses
  subclasses$apparent <- NULL
  subclasses$validation_split <- NULL
  subclasses$validation_time_split <- NULL
  subclasses$group_validation_split <- NULL
  subclasses$validation_set <- NULL

  for (x in subclasses) {
    loc <- seq_len(nrow(x))
    loc[length(loc)] <- 1L
    expect_s3_class_bare_tibble(dplyr_row_slice(x, loc))
  }
})

test_that("row slicing and selecting everything keeps the rset subclass", {
  for (x in rset_subclasses) {
    loc <- seq_len(nrow(x))
    expect_s3_class_rset(dplyr_row_slice(x, loc))
  }
})

test_that("rset subclass is kept if row order is changed but all rows are present", {
  for (x in rset_subclasses) {
    loc <- rev(seq_len(nrow(x)))
    expect_s3_class_rset(dplyr_row_slice(x, loc))
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

test_that("mutate() keeps rset class if replacement rset column is same as original", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(mutate(x, splits = splits))
    expect_s3_class_rset(mutate(x, id = id))
  }
})

test_that("adding a column that looks like an `id` drops the class", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(mutate(x, id9 = 1))
  }
})

# ------------------------------------------------------------------------------
# arrange()

test_that("arrange() keeps rset class when row order is modified", {
  for (x in rset_subclasses) {
    x <- mutate(x, rn = row_number())
    expect_s3_class_rset(arrange(x, desc(rn)))
  }
})

test_that("arrange() keeps rset class when row order is untouched", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(arrange(x))

    x <- mutate(x, rn = row_number())
    expect_s3_class_rset(arrange(x, rn))
  }
})

# ------------------------------------------------------------------------------
# filter()

test_that("filter() drops rset class when rows are modified", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(filter(x, 0 == 1))
    expect_s3_class_bare_tibble(filter(x, is.numeric(id)))
  }
})

test_that("filter() keeps rset class if row order is untouched", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(filter(x))
    expect_s3_class_rset(filter(x, is.character(id)))
  }
})

# ------------------------------------------------------------------------------
# rename()

test_that("renaming can keep the rset class", {
  for (x in rset_subclasses) {
    x <- mutate(x, a = 1)
    x <- rename(x, b = a)
    expect_s3_class_rset(x)
  }
})

test_that("renaming `id` at all drops the rset class", {
  for (x in rset_subclasses) {
    x <- rename(x, id9 = id)
    expect_s3_class_bare_tibble(x)
  }
})

test_that("renaming `id` to a non-id name drops the rset class", {
  for (x in rset_subclasses) {
    x <- rename(x, stuff = id)
    expect_s3_class_bare_tibble(x)
  }
})

test_that("for nested_cv, renaming `inner_resamples` drops the rset class", {
  x <- rset_subclasses$nested_cv
  x <- rename(x, inner_stuff = inner_resamples)
  expect_s3_class_bare_tibble(x)
})

# ------------------------------------------------------------------------------
# select()

test_that("select() can keep rset class", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(select(x, everything()))
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

test_that("slice() drops rset class when rows are modified", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(slice(x, 0))
  }
})

test_that("slice() keeps rset class when rows are untouched", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(slice(x, seq_len(nrow(x))))
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
# relocate()

test_that("can relocate() and keep the class", {
  for (x in rset_subclasses) {
    x <- relocate(x, id)
    expect_s3_class_rset(x)
  }
})

# ------------------------------------------------------------------------------
# distinct()

test_that("distinct() keeps the class if everything is intact", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(distinct(x))
  }
})

test_that("distinct() drops the class if any rset columns are lost", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(distinct(x, splits))
  }
})

# ------------------------------------------------------------------------------
# left_join()

test_that("left_join() can keep rset class if rset structure is intact", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(left_join(x, x, by = names(x)))

    y <- tibble(id = x$id[[1]], x = 1)
    expect_s3_class_rset(left_join(x, y, by = "id"))
  }
})

test_that("left_join() can lose rset class if rows are added", {
  for (x in rset_subclasses) {
    y <- tibble(id = x$id[[1]], x = 1:2)
    expect_s3_class_bare_tibble(left_join(x, y, by = "id"))
  }
})

# ------------------------------------------------------------------------------
# right_join()

test_that("right_join() can keep rset class if rset structure is intact", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(right_join(x, x, by = names(x)))

    x_names <- names(x)
    id_names <- x_names[col_starts_with_id(x_names)]

    y <- mutate(select(x, all_of(id_names)), x = 1)
    expect_s3_class_rset(right_join(x, y, by = id_names))
  }
})

test_that("right_join() can lose rset class if rows are added", {
  for (x in rset_subclasses) {
    y <- tibble(id = x$id[[1]], x = 1:2)
    expect_s3_class_bare_tibble(right_join(x, y, by = "id"))
  }
})

test_that("right_join() restores to the type of first input", {
  for (x in rset_subclasses) {
    y <- tibble(id = x$id[[1]], x = 1)
    # technically rset structure is intact, but `y` is a bare tibble!
    expect_s3_class_bare_tibble(right_join(y, x, by = "id"))
  }
})

# ------------------------------------------------------------------------------
# full_join()

test_that("full_join() can keep rset class if rset structure is intact", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(full_join(x, x, by = names(x)))
  }
})

test_that("full_join() can lose rset class if rows are added", {
  for (x in rset_subclasses) {
    y <- tibble(id = "foo", x = 1)
    expect_s3_class_bare_tibble(full_join(x, y, by = "id"))
  }
})

# ------------------------------------------------------------------------------
# anti_join()

test_that("anti_join() can keep rset class if rset structure is intact", {
  for (x in rset_subclasses) {
    y <- tibble(id = "foo")
    expect_s3_class_rset(anti_join(x, y, by = "id"))
  }
})

test_that("anti_join() can lose rset class if rows are removed", {
  for (x in rset_subclasses) {
    y <- tibble(id = x$id[[1]], x = 1)
    expect_s3_class_bare_tibble(anti_join(x, y, by = "id"))
  }
})

# ------------------------------------------------------------------------------
# semi_join()

test_that("semi_join() can keep rset class if rset structure is intact", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(semi_join(x, x, by = names(x)))
  }
})

test_that("semi_join() can lose rset class if rows are removed", {
  for (x in rset_subclasses) {
    y <- tibble(id = "foo", x = 1)
    expect_s3_class_bare_tibble(semi_join(x, y, by = "id"))
  }
})

# ------------------------------------------------------------------------------
# nest_join()

test_that("nest_join() can keep rset class if rset structure is intact", {
  for (x in rset_subclasses) {
    y <- mutate(x, foo = "bar")
    expect_s3_class_rset(nest_join(x, y, by = names(x)))
  }
})

# ------------------------------------------------------------------------------
# bind_rows()

test_that("bind_rows() keeps the class if there are no new rows/cols and the first object is an rset subclass", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(bind_rows(x))
    expect_s3_class_rset(bind_rows(x, tibble()))
    expect_s3_class_bare_tibble(bind_rows(tibble(), x))
  }
})

test_that("bind_rows() drops the class with new rows", {
  for (x in rset_subclasses) {
    expect_s3_class_bare_tibble(bind_rows(x, x))
  }
})

# ------------------------------------------------------------------------------
# bind_cols()

test_that("bind_cols() keeps the class if there are no new rows and the first object is an rset subclass", {
  for (x in rset_subclasses) {
    expect_s3_class_rset(bind_cols(x))
    expect_s3_class_rset(bind_cols(x, tibble(x = 1)))
    expect_s3_class_bare_tibble(bind_cols(tibble(x = 1), x))
  }
})

test_that("bind_cols() drops the class with new rows", {
  # Use rset subclass with 1 row, these get recycled
  x <- rset_subclasses$apparent
  expect_s3_class_bare_tibble(bind_cols(x, tibble(x = 1:2)))
})

