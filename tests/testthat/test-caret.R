###################################################################
## Test cases for caret -> rsample that mimic `trainControl`

check_indices <- function(newer, orig) {
  for (i in seq_along(newer$splits)) {
    expect_equal(
      as.integer(newer$splits[[i]]),
      orig$index[[i]]
    )
    expect_equal(
      as.integer(newer$splits[[i]], "assessment"),
      orig$indexOut[[i]]
    )
  }
  invisible(NULL)
}

###################################################################

test_that("basic v-fold", {
  cv_1 <- structure(
    list(
      method = "cv",
      index = structure(
        list(
          Fold1 = c(2L, 3L, 4L, 5L, 6L, 7L, 8L, 10L, 11L, 13L),
          Fold2 = c(
            1L,
            4L,
            6L,
            8L,
            9L,
            10L,
            11L,
            12L,
            13L,
            14L,
            15L
          ),
          Fold3 = c(1L, 2L, 3L, 5L, 7L, 9L, 12L, 14L, 15L)
        ),
        .Names = c("Fold1", "Fold2", "Fold3")
      ),
      indexOut = structure(
        list(
          Resample1 = c(1L, 9L, 12L, 14L, 15L),
          Resample2 = c(2L, 3L, 5L, 7L),
          Resample3 = c(4L, 6L, 8L, 10L, 11L, 13L)
        ),
        .Names = c("Resample1", "Resample2", "Resample3")
      ),
      number = 3,
      repeats = NA
    ),
    .Names = c("method", "index", "indexOut", "number", "repeats")
  )
  dat <- data.frame(y = 1:15, x = 15:1)
  vfold_obj_1 <- caret2rsample(cv_1, data = dat)

  check_indices(vfold_obj_1, cv_1)
  for (i in seq_along(vfold_obj_1$splits)) {
    expect_equal(vfold_obj_1$id[[i]], names(cv_1$index)[i])
  }
})

test_that("repeated v-fold", {
  cv_2 <-
    structure(
      list(
        method = "repeatedcv",
        index = structure(
          list(
            Fold1.Rep1 = c(1L, 3L, 4L, 6L, 9L, 10L, 12L, 13L, 14L, 15L),
            Fold2.Rep1 = c(2L, 5L, 7L, 8L, 10L, 11L, 13L, 14L, 15L),
            Fold3.Rep1 = c(1L, 2L, 3L, 4L, 5L, 6L, 7L, 8L, 9L, 11L, 12L),
            Fold1.Rep2 = c(1L, 2L, 3L, 5L, 6L, 7L, 10L, 11L, 12L, 14L),
            Fold2.Rep2 = c(2L, 4L, 6L, 8L, 9L, 11L, 13L, 14L, 15L),
            Fold3.Rep2 = c(1L, 3L, 4L, 5L, 7L, 8L, 9L, 10L, 12L, 13L, 15L)
          ),
          .Names = c(
            "Fold1.Rep1",
            "Fold2.Rep1",
            "Fold3.Rep1",
            "Fold1.Rep2",
            "Fold2.Rep2",
            "Fold3.Rep2"
          )
        ),
        indexOut = structure(
          list(
            Resample1 = c(2L, 5L, 7L, 8L, 11L),
            Resample2 = c(1L, 3L, 4L, 6L, 9L, 12L),
            Resample3 = c(10L, 13L, 14L, 15L),
            Resample4 = c(4L, 8L, 9L, 13L, 15L),
            Resample5 = c(1L, 3L, 5L, 7L, 10L, 12L),
            Resample6 = c(2L, 6L, 11L, 14L)
          ),
          .Names = c(
            "Resample1",
            "Resample2",
            "Resample3",
            "Resample4",
            "Resample5",
            "Resample6"
          )
        ),
        number = 3,
        repeats = 2
      ),
      .Names = c("method", "index", "indexOut", "number", "repeats")
    )
  dat <- data.frame(y = 1:15, x = 15:1)
  vfold_obj_2 <- caret2rsample(cv_2, data = dat)

  check_indices(vfold_obj_2, cv_2)
  for (i in seq_along(vfold_obj_2$splits)) {
    expect_equal(
      paste(vfold_obj_2$id2[[i]], vfold_obj_2$id[[i]], sep = "."),
      names(cv_2$index)[i]
    )
  }
})

test_that("basic boot", {
  bt_1 <-
    structure(
      list(
        method = "boot",
        index = structure(
          list(
            Resample1 = c(
              1L,
              1L,
              4L,
              4L,
              5L,
              7L,
              8L,
              10L,
              11L,
              11L,
              12L,
              13L,
              15L,
              15L,
              15L
            ),
            Resample2 = c(
              1L,
              2L,
              3L,
              5L,
              5L,
              5L,
              6L,
              7L,
              8L,
              9L,
              9L,
              9L,
              10L,
              10L,
              12L
            )
          ),
          .Names = c("Resample1", "Resample2")
        ),
        indexOut = structure(
          list(
            Resample1 = c(2L, 3L, 6L, 9L, 14L),
            Resample2 = c(4L, 11L, 13L, 14L, 15L)
          ),
          .Names = c("Resample1", "Resample2")
        ),
        number = 2
      ),
      .Names = c(
        "method",
        "index",
        "indexOut",
        "number"
      )
    )
  dat <- data.frame(y = 1:15, x = 15:1)
  bt_obj_1 <- caret2rsample(bt_1, data = dat)

  check_indices(bt_obj_1, bt_1)
  for (i in seq_along(bt_obj_1$splits)) {
    expect_equal(bt_obj_1$id[[i]], names(bt_1$index)[i])
  }
})

test_that("boot 632", {
  bt_2 <-
    structure(
      list(
        method = "boot",
        index = structure(
          list(
            Resample1 = c(
              1L,
              1L,
              4L,
              4L,
              5L,
              7L,
              8L,
              10L,
              11L,
              11L,
              12L,
              13L,
              15L,
              15L,
              15L
            ),
            Resample2 = c(
              1L,
              2L,
              3L,
              5L,
              5L,
              5L,
              6L,
              7L,
              8L,
              9L,
              9L,
              9L,
              10L,
              10L,
              12L
            )
          ),
          .Names = c("Resample1", "Resample2")
        ),
        indexOut = structure(
          list(
            Resample1 = c(2L, 3L, 6L, 9L, 14L),
            Resample2 = c(4L, 11L, 13L, 14L, 15L)
          ),
          .Names = c("Resample1", "Resample2")
        ),
        number = 2
      ),
      .Names = c(
        "method",
        "index",
        "indexOut",
        "number"
      )
    )
  bt_2$method <- "boot632"
  dat <- data.frame(y = 1:15, x = 15:1)
  bt_obj_2 <- caret2rsample(bt_2, data = dat)

  check_indices(bt_obj_2, bt_2)
  for (i in seq_along(bt_obj_2$splits)) {
    expect_equal(bt_obj_2$id[[i]], names(bt_2$index)[i])
  }
})

test_that("boot optim", {
  bt_3 <-
    structure(
      list(
        method = "boot",
        index = structure(
          list(
            Resample1 = c(
              1L,
              1L,
              4L,
              4L,
              5L,
              7L,
              8L,
              10L,
              11L,
              11L,
              12L,
              13L,
              15L,
              15L,
              15L
            ),
            Resample2 = c(
              1L,
              2L,
              3L,
              5L,
              5L,
              5L,
              6L,
              7L,
              8L,
              9L,
              9L,
              9L,
              10L,
              10L,
              12L
            )
          ),
          .Names = c("Resample1", "Resample2")
        ),
        indexOut = structure(
          list(
            Resample1 = c(2L, 3L, 6L, 9L, 14L),
            Resample2 = c(4L, 11L, 13L, 14L, 15L)
          ),
          .Names = c("Resample1", "Resample2")
        ),
        number = 2
      ),
      .Names = c(
        "method",
        "index",
        "indexOut",
        "number"
      )
    )
  bt_3$method <- "optimism_boot"
  dat <- data.frame(y = 1:15, x = 15:1)
  bt_obj_3 <- caret2rsample(bt_3, data = dat)

  check_indices(bt_obj_3, bt_3)
  for (i in seq_along(bt_obj_3$splits)) {
    expect_equal(bt_obj_3$id[[i]], names(bt_3$index)[i])
  }
})

test_that("boot all", {
  bt_4 <-
    structure(
      list(
        method = "boot",
        index = structure(
          list(
            Resample1 = c(
              1L,
              1L,
              4L,
              4L,
              5L,
              7L,
              8L,
              10L,
              11L,
              11L,
              12L,
              13L,
              15L,
              15L,
              15L
            ),
            Resample2 = c(
              1L,
              2L,
              3L,
              5L,
              5L,
              5L,
              6L,
              7L,
              8L,
              9L,
              9L,
              9L,
              10L,
              10L,
              12L
            )
          ),
          .Names = c("Resample1", "Resample2")
        ),
        indexOut = structure(
          list(
            Resample1 = c(2L, 3L, 6L, 9L, 14L),
            Resample2 = c(4L, 11L, 13L, 14L, 15L)
          ),
          .Names = c("Resample1", "Resample2")
        ),
        number = 2
      ),
      .Names = c(
        "method",
        "index",
        "indexOut",
        "number"
      )
    )
  bt_4$method <- "boot_all"
  dat <- data.frame(y = 1:15, x = 15:1)
  bt_obj_4 <- caret2rsample(bt_4, data = dat)

  check_indices(bt_obj_4, bt_4)
  for (i in seq_along(bt_obj_4$splits)) {
    expect_equal(bt_obj_4$id[[i]], names(bt_4$index)[i])
  }
})

test_that("adaptive boot", {
  bt_5 <-
    structure(
      list(
        method = "boot",
        index = structure(
          list(
            Resample1 = c(
              1L,
              1L,
              4L,
              4L,
              5L,
              7L,
              8L,
              10L,
              11L,
              11L,
              12L,
              13L,
              15L,
              15L,
              15L
            ),
            Resample2 = c(
              1L,
              2L,
              3L,
              5L,
              5L,
              5L,
              6L,
              7L,
              8L,
              9L,
              9L,
              9L,
              10L,
              10L,
              12L
            )
          ),
          .Names = c("Resample1", "Resample2")
        ),
        indexOut = structure(
          list(
            Resample1 = c(2L, 3L, 6L, 9L, 14L),
            Resample2 = c(4L, 11L, 13L, 14L, 15L)
          ),
          .Names = c("Resample1", "Resample2")
        ),
        number = 2
      ),
      .Names = c(
        "method",
        "index",
        "indexOut",
        "number"
      )
    )
  bt_5$method <- "adaptive_boot"
  dat <- data.frame(y = 1:15, x = 15:1)
  bt_obj_5 <- caret2rsample(bt_5, data = dat)

  check_indices(bt_obj_5, bt_5)
  for (i in seq_along(bt_obj_5$splits)) {
    expect_equal(bt_obj_5$id[[i]], names(bt_5$index)[i])
  }
})


test_that("loo", {
  loo_1 <-
    structure(
      list(
        method = "LOOCV",
        index = structure(
          list(
            Fold01 = 2:15,
            Fold02 = c(
              1L,
              3L,
              4L,
              5L,
              6L,
              7L,
              8L,
              9L,
              10L,
              11L,
              12L,
              13L,
              14L,
              15L
            ),
            Fold03 = c(
              1L,
              2L,
              4L,
              5L,
              6L,
              7L,
              8L,
              9L,
              10L,
              11L,
              12L,
              13L,
              14L,
              15L
            ),
            Fold04 = c(
              1L,
              2L,
              3L,
              5L,
              6L,
              7L,
              8L,
              9L,
              10L,
              11L,
              12L,
              13L,
              14L,
              15L
            ),
            Fold05 = c(
              1L,
              2L,
              3L,
              4L,
              6L,
              7L,
              8L,
              9L,
              10L,
              11L,
              12L,
              13L,
              14L,
              15L
            ),
            Fold06 = c(
              1L,
              2L,
              3L,
              4L,
              5L,
              7L,
              8L,
              9L,
              10L,
              11L,
              12L,
              13L,
              14L,
              15L
            ),
            Fold07 = c(
              1L,
              2L,
              3L,
              4L,
              5L,
              6L,
              8L,
              9L,
              10L,
              11L,
              12L,
              13L,
              14L,
              15L
            ),
            Fold08 = c(
              1L,
              2L,
              3L,
              4L,
              5L,
              6L,
              7L,
              9L,
              10L,
              11L,
              12L,
              13L,
              14L,
              15L
            ),
            Fold09 = c(
              1L,
              2L,
              3L,
              4L,
              5L,
              6L,
              7L,
              8L,
              10L,
              11L,
              12L,
              13L,
              14L,
              15L
            ),
            Fold10 = c(
              1L,
              2L,
              3L,
              4L,
              5L,
              6L,
              7L,
              8L,
              9L,
              11L,
              12L,
              13L,
              14L,
              15L
            ),
            Fold11 = c(
              1L,
              2L,
              3L,
              4L,
              5L,
              6L,
              7L,
              8L,
              9L,
              10L,
              12L,
              13L,
              14L,
              15L
            ),
            Fold12 = c(
              1L,
              2L,
              3L,
              4L,
              5L,
              6L,
              7L,
              8L,
              9L,
              10L,
              11L,
              13L,
              14L,
              15L
            ),
            Fold13 = c(
              1L,
              2L,
              3L,
              4L,
              5L,
              6L,
              7L,
              8L,
              9L,
              10L,
              11L,
              12L,
              14L,
              15L
            ),
            Fold14 = c(
              1L,
              2L,
              3L,
              4L,
              5L,
              6L,
              7L,
              8L,
              9L,
              10L,
              11L,
              12L,
              13L,
              15L
            ),
            Fold15 = 1:14
          ),
          .Names = c(
            "Fold01",
            "Fold02",
            "Fold03",
            "Fold04",
            "Fold05",
            "Fold06",
            "Fold07",
            "Fold08",
            "Fold09",
            "Fold10",
            "Fold11",
            "Fold12",
            "Fold13",
            "Fold14",
            "Fold15"
          )
        ),
        indexOut = structure(
          list(
            Resample01 = 1L,
            Resample02 = 2L,
            Resample03 = 3L,
            Resample04 = 4L,
            Resample05 = 5L,
            Resample06 = 6L,
            Resample07 = 7L,
            Resample08 = 8L,
            Resample09 = 9L,
            Resample10 = 10L,
            Resample11 = 11L,
            Resample12 = 12L,
            Resample13 = 13L,
            Resample14 = 14L,
            Resample15 = 15L
          ),
          .Names = c(
            "Resample01",
            "Resample02",
            "Resample03",
            "Resample04",
            "Resample05",
            "Resample06",
            "Resample07",
            "Resample08",
            "Resample09",
            "Resample10",
            "Resample11",
            "Resample12",
            "Resample13",
            "Resample14",
            "Resample15"
          )
        )
      ),
      .Names = c("method", "index", "indexOut")
    )
  dat <- data.frame(y = 1:15, x = 15:1)
  loo_obj <- caret2rsample(loo_1, data = dat)

  check_indices(loo_obj, loo_1)
  for (i in seq_along(loo_obj$splits)) {
    expect_equal(loo_obj$id[[i]], names(loo_1$index)[i])
  }
})

test_that("mcv", {
  lgo1 <-
    structure(
      list(
        method = "LGOCV",
        index = structure(
          list(
            Resample1 = c(1L, 4L, 5L, 6L, 7L, 9L, 10L, 14L),
            Resample2 = c(2L, 4L, 5L, 6L, 9L, 10L, 14L, 15L),
            Resample3 = c(1L, 2L, 3L, 5L, 6L, 7L, 8L, 9L)
          ),
          .Names = c("Resample1", "Resample2", "Resample3")
        ),
        indexOut = structure(
          list(
            Resample1 = c(2L, 3L, 8L, 11L, 12L, 13L, 15L),
            Resample2 = c(1L, 3L, 7L, 8L, 11L, 12L, 13L),
            Resample3 = c(4L, 10L, 11L, 12L, 13L, 14L, 15L)
          ),
          .Names = c("Resample1", "Resample2", "Resample3")
        ),
        number = 3,
        p = 0.5
      ),
      .Names = c("method", "index", "indexOut", "number", "p")
    )
  dat <- data.frame(y = 1:15, x = 15:1)
  mcv_obj <- caret2rsample(lgo1, data = dat)

  check_indices(mcv_obj, lgo1)
  for (i in seq_along(mcv_obj$splits)) {
    expect_equal(mcv_obj$id[[i]], names(lgo1$index)[i])
  }
})

test_that("rolling origin", {
  rof_1 <-
    structure(
      list(
        method = "timeSlice",
        index = structure(
          list(
            Training04 = 1:4,
            Training05 = 2:5,
            Training06 = 3:6,
            Training07 = 4:7,
            Training08 = 5:8,
            Training09 = 6:9,
            Training10 = 7:10
          ),
          .Names = c(
            "Training04",
            "Training05",
            "Training06",
            "Training07",
            "Training08",
            "Training09",
            "Training10"
          )
        ),
        indexOut = structure(
          list(
            Testing04 = 5:9,
            Testing05 = 6:10,
            Testing06 = 7:11,
            Testing07 = 8:12,
            Testing08 = 9:13,
            Testing09 = 10:14,
            Testing10 = 11:15
          ),
          .Names = c(
            "Testing04",
            "Testing05",
            "Testing06",
            "Testing07",
            "Testing08",
            "Testing09",
            "Testing10"
          )
        ),
        initialWindow = 4,
        horizon = 5,
        fixedWindow = TRUE,
        skip = 0
      ),
      .Names = c(
        "method",
        "index",
        "indexOut",
        "initialWindow",
        "horizon",
        "fixedWindow",
        "skip"
      )
    )
  dat <- data.frame(y = 1:15, x = 15:1)
  rof_obj <- caret2rsample(rof_1, data = dat)

  check_indices(rof_obj, rof_1)
  for (i in seq_along(rof_obj$splits)) {
    expect_equal(rof_obj$id[[i]], names(rof_1$index)[i])
  }
})
