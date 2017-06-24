## ----setup, echo = FALSE, message = FALSE--------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", warning = FALSE)
options(tibble.print_min = 4L, tibble.print_max = 4L)

## ----mtcars_bt, message=FALSE--------------------------------------------
library(rsample)
set.seed(8584)
bt_resamples <- bootstraps(mtcars, times = 3)
bt_resamples

## ----rsplit--------------------------------------------------------------
first_resample <- bt_resamples$splits[[1]]
first_resample

## ----rsplit_df-----------------------------------------------------------
head(as.data.frame(first_resample))
as.data.frame(first_resample, data = "assessment")

