## ----setup, echo = FALSE, message = FALSE--------------------------------
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", 
                      warning = FALSE, message = FALSE)
options(tibble.print_min = 4L, tibble.print_max = 4L)
library(ggplot2)
theme_set(theme_bw())

## ----Sacramento, message=FALSE-------------------------------------------
library(caret)
data(Sacramento)
str(Sacramento)

## ----model_vfold, message=FALSE------------------------------------------
library(rsample)
set.seed(4622)
rs_obj <- vfold_cv(Sacramento, V = 10, repeats = 10)
rs_obj

## ----lm_func-------------------------------------------------------------
library(broom)
## splits will be the `rsplit` object with the 90/10 partition
holdout_resids <- function(splits, ...) {
  # Fit the model to the 90%
  mod <- lm(..., data = analysis(splits))
  # Save the 10%
  holdout <- assessment(splits)
  # `augment` will save the predictions with the holdout data set
  res <- broom::augment(mod, newdata = holdout)
  # Calculate the residuals
  res$.resid <- log(holdout$price) - res$.fitted
  # Return the assessment data set wit the additional columns
  res
}

## ----onefold-------------------------------------------------------------
str(
  holdout_resids(rs_obj$splits[[1]], 
                 log(price) ~ beds + baths + sqft + type
  )
)

## ----model_along, message=FALSE------------------------------------------
library(purrr)
rs_obj$results <- map(rs_obj$splits,
                      holdout_resids,
                      log(price) ~ beds + baths + sqft + type)
rs_obj

## ----model_rmse----------------------------------------------------------
rmse <- function(x) sqrt(mean(x$.resid^2))
rs_obj$rmse <- map_dbl(rs_obj$results, rmse)

## ----type_plot-----------------------------------------------------------
ggplot(Sacramento, aes(x = type, y = price)) + 
  geom_boxplot() + 
  scale_y_log10()

## ----mean_diff-----------------------------------------------------------
mean_diff <- function(splits) {
  x <- analysis(splits)
  median(x$price[x$type == "Residential"]) - 
      median(x$price[x$type == "Multi_Family"])     
}

## ----boot_mean_diff------------------------------------------------------
set.seed(353)
bt_resamples <- bootstraps(Sacramento, times = 200)

## ----stats---------------------------------------------------------------
bt_resamples$statistic <- map_dbl(bt_resamples$splits, mean_diff)

## ----stats_plot----------------------------------------------------------
ggplot(bt_resamples, aes(x = statistic)) + 
  geom_line(stat = "density", adjust = 1.25) + 
  xlab("median(Residential) - median(Multi_Family)")

## ----ci------------------------------------------------------------------
quantile(bt_resamples$statistic, 
         probs = c(0.025, 0.500, 0.975))

## ----coefs---------------------------------------------------------------
lm_coefs <- function(splits, ...) {
  ## use `analysis` or `as.data.frame` to get the analysis data
  mod <- lm(..., data = analysis(splits))
  as.data.frame(t(coef(mod)))
}
bt_resamples$betas <- map(.x = bt_resamples$splits, 
                          .f = lm_coefs, 
                          log(price) ~ beds + baths + sqft + type)
bt_resamples
bt_resamples$betas[[1]]

