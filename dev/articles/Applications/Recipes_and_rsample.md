# Recipes with rsample

The [recipes](https://recipes.tidymodels.org/) package contains a data
preprocessor that can be used to avoid the potentially expensive formula
methods as well as providing a richer set of data manipulation tools
than base R can provide. This document uses version 1.3.1 of recipes.

In many cases, the preprocessing steps might contain quantities that
require statistical estimation of parameters, such as

- signal extraction using principal component analysis

- imputation of missing values

- transformations of individual variables (e.g. Box-Cox transformations)

It is critical that any complex preprocessing steps be contained
*inside* of resampling so that the model performance estimates take into
account the variability of these steps.

Before discussing how rsample can use recipes, let’s look at an example
recipe for the Ames housing data.

## An Example Recipe

For illustration, the Ames housing data will be used. There are sale
prices of homes along with various other descriptors for the property:

``` r
data(ames, package = "modeldata")
```

Suppose that we will again fit a simple regression model with the
formula:

``` r
log10(Sale_Price) ~ Neighborhood + House_Style + Year_Sold + Lot_Area
```

The distribution of the lot size is right-skewed:

``` r
library(ggplot2)
theme_set(theme_bw())
ggplot(ames, aes(x = Lot_Area)) + 
  geom_histogram(binwidth = 5000, col = "red", fill ="red", alpha = .5)
```

![The histogram of the lot size, showing a long right tail of the
empirical
distribution.](Recipes_and_rsample_files/figure-html/build-1.png)

It might benefit the model if we estimate a transformation of the data
using the Box-Cox procedure.

Also, note that the frequencies of the neighborhoods can vary:

``` r
ggplot(ames, aes(x = Neighborhood)) + geom_bar() + coord_flip() + xlab("")
```

![A bar plot of the counts of different neighborhoods, with the
frequencies ranging from over 400 observations of North_Ames to only a
handful for Green_Hills and
Landmark.](Recipes_and_rsample_files/figure-html/hood-1.png)

When these are resampled, some neighborhoods will not be included in the
test set and this will result in a column of dummy variables with zero
entries. The same is true for the `House_Style` variable. We might want
to collapse rarely occurring values into “other” categories.

To define the design matrix, an initial recipe is created:

``` r
library(recipes)

# Apply log10 transformation outside the recipe
# https://www.tmwr.org/recipes.html#skip-equals-true
ames <-
  ames |>
  mutate(Sale_Price = log10(Sale_Price))

rec <- 
  recipe(Sale_Price ~ Neighborhood + House_Style + Year_Sold + Lot_Area, 
         data = ames) |>
  # Collapse rarely occurring jobs into "other"
  step_other(Neighborhood, House_Style, threshold = 0.05) |>
  # Dummy variables on the qualitative predictors
  step_dummy(all_nominal()) |>
  # Unskew a predictor
  step_BoxCox(Lot_Area) |>
  # Normalize
  step_center(all_predictors()) |>
  step_scale(all_predictors()) 
rec
```

This recreates the work that the formula method traditionally uses with
the additional steps.

While the original data object `ames` is used in the call, it is only
used to define the variables and their characteristics so a single
recipe is valid across all resampled versions of the data. The recipe
can be estimated on the analysis component of the resample.

If we execute the recipe on the entire data set:

``` r
rec_training_set <- prep(rec, training = ames)
rec_training_set
```

    ## 

    ## ── Recipe ─────────────────────────────────────────────────────────────

    ## 

    ## ── Inputs

    ## Number of variables by role

    ## outcome:   1
    ## predictor: 4

    ## 

    ## ── Training information

    ## Training data contained 2930 data points and no incomplete rows.

    ## 

    ## ── Operations

    ## • Collapsing factor levels for: Neighborhood House_Style | Trained

    ## • Dummy variables from: Neighborhood House_Style | Trained

    ## • Box-Cox transformation on: Lot_Area | Trained

    ## • Centering for: Year_Sold Lot_Area, ... | Trained

    ## • Scaling for: Year_Sold Lot_Area, ... | Trained

To get the values of the data, the `bake` function can be used:

``` r
# By default, the selector `everything()` is used to 
# return all the variables. Other selectors can be used too. 
bake(rec_training_set, new_data = head(ames))
```

    ## # A tibble: 6 × 14
    ##   Year_Sold Lot_Area Sale_Price Neighborhood_College_Creek
    ##       <dbl>    <dbl>      <dbl>                      <dbl>
    ## 1      1.68    2.70        5.33                     -0.317
    ## 2      1.68    0.506       5.02                     -0.317
    ## 3      1.68    0.930       5.24                     -0.317
    ## 4      1.68    0.423       5.39                     -0.317
    ## 5      1.68    0.865       5.28                     -0.317
    ## 6      1.68    0.197       5.29                     -0.317
    ## # ℹ 10 more variables: Neighborhood_Old_Town <dbl>,
    ## #   Neighborhood_Edwards <dbl>, Neighborhood_Somerset <dbl>,
    ## #   Neighborhood_Northridge_Heights <dbl>, Neighborhood_Gilbert <dbl>,
    ## #   Neighborhood_Sawyer <dbl>, Neighborhood_other <dbl>,
    ## #   House_Style_One_Story <dbl>, House_Style_Two_Story <dbl>,
    ## #   House_Style_other <dbl>

Note that there are fewer dummy variables for `Neighborhood` and
`House_Style` than in the data.

Also, the above code using
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) benefits
from the default argument of `retain = TRUE`, which keeps the processed
version of the data set so that we don’t have to reapply the steps to
extract the processed values. For the data used to train the recipe, we
would have used:

``` r
bake(rec_training_set, new_data = NULL) |> head()
```

    ## # A tibble: 6 × 14
    ##   Year_Sold Lot_Area Sale_Price Neighborhood_College_Creek
    ##       <dbl>    <dbl>      <dbl>                      <dbl>
    ## 1      1.68    2.70        5.33                     -0.317
    ## 2      1.68    0.506       5.02                     -0.317
    ## 3      1.68    0.930       5.24                     -0.317
    ## 4      1.68    0.423       5.39                     -0.317
    ## 5      1.68    0.865       5.28                     -0.317
    ## 6      1.68    0.197       5.29                     -0.317
    ## # ℹ 10 more variables: Neighborhood_Old_Town <dbl>,
    ## #   Neighborhood_Edwards <dbl>, Neighborhood_Somerset <dbl>,
    ## #   Neighborhood_Northridge_Heights <dbl>, Neighborhood_Gilbert <dbl>,
    ## #   Neighborhood_Sawyer <dbl>, Neighborhood_other <dbl>,
    ## #   House_Style_One_Story <dbl>, House_Style_Two_Story <dbl>,
    ## #   House_Style_other <dbl>

The next section will explore recipes and bootstrap resampling for
modeling:

``` r
library(rsample)
set.seed(7712)
bt_samples <- bootstraps(ames)
bt_samples
```

    ## # Bootstrap sampling 
    ## # A tibble: 25 × 2
    ##    splits              id         
    ##    <list>              <chr>      
    ##  1 <split [2930/1095]> Bootstrap01
    ##  2 <split [2930/1082]> Bootstrap02
    ##  3 <split [2930/1072]> Bootstrap03
    ##  4 <split [2930/1077]> Bootstrap04
    ##  5 <split [2930/1074]> Bootstrap05
    ##  6 <split [2930/1088]> Bootstrap06
    ##  7 <split [2930/1076]> Bootstrap07
    ##  8 <split [2930/1068]> Bootstrap08
    ##  9 <split [2930/1062]> Bootstrap09
    ## 10 <split [2930/1064]> Bootstrap10
    ## # ℹ 15 more rows

``` r
bt_samples$splits[[1]]
```

    ## <Analysis/Assess/Total>
    ## <2930/1095/2930>

## Working with Resamples

We can add a recipe column to the tibble. recipes has a convenience
function called
[`prepper()`](https://recipes.tidymodels.org/reference/prepper.html)
that can be used to call
[`prep()`](https://recipes.tidymodels.org/reference/prep.html) but has
the split object as the first argument (for easier purrring):

``` r
library(purrr)

bt_samples$recipes <- map(bt_samples$splits, prepper, recipe = rec)
bt_samples
```

    ## # Bootstrap sampling 
    ## # A tibble: 25 × 3
    ##    splits              id          recipes 
    ##    <list>              <chr>       <list>  
    ##  1 <split [2930/1095]> Bootstrap01 <recipe>
    ##  2 <split [2930/1082]> Bootstrap02 <recipe>
    ##  3 <split [2930/1072]> Bootstrap03 <recipe>
    ##  4 <split [2930/1077]> Bootstrap04 <recipe>
    ##  5 <split [2930/1074]> Bootstrap05 <recipe>
    ##  6 <split [2930/1088]> Bootstrap06 <recipe>
    ##  7 <split [2930/1076]> Bootstrap07 <recipe>
    ##  8 <split [2930/1068]> Bootstrap08 <recipe>
    ##  9 <split [2930/1062]> Bootstrap09 <recipe>
    ## 10 <split [2930/1064]> Bootstrap10 <recipe>
    ## # ℹ 15 more rows

``` r
bt_samples$recipes[[1]]
```

    ## 

    ## ── Recipe ─────────────────────────────────────────────────────────────

    ## 

    ## ── Inputs

    ## Number of variables by role

    ## outcome:   1
    ## predictor: 4

    ## 

    ## ── Training information

    ## Training data contained 2930 data points and no incomplete rows.

    ## 

    ## ── Operations

    ## • Collapsing factor levels for: Neighborhood House_Style | Trained

    ## • Dummy variables from: Neighborhood House_Style | Trained

    ## • Box-Cox transformation on: Lot_Area | Trained

    ## • Centering for: Year_Sold Lot_Area, ... | Trained

    ## • Scaling for: Year_Sold Lot_Area, ... | Trained

Now, to fit the model, the fit function only needs the recipe as input.
This is because the above code implicitly used the `retain = TRUE`
option in
[`prep()`](https://recipes.tidymodels.org/reference/prep.html).
Otherwise, the split objects would also be needed to
[`bake()`](https://recipes.tidymodels.org/reference/bake.html) the
recipe (as it will in the prediction function below).

``` r
fit_lm <- function(rec_obj, ...) 
  lm(..., data = bake(rec_obj, new_data = NULL, everything()))

bt_samples$lm_mod <- 
  map(
    bt_samples$recipes, 
    fit_lm, 
    Sale_Price ~ .
  )
bt_samples
```

    ## # Bootstrap sampling 
    ## # A tibble: 25 × 4
    ##    splits              id          recipes  lm_mod
    ##    <list>              <chr>       <list>   <list>
    ##  1 <split [2930/1095]> Bootstrap01 <recipe> <lm>  
    ##  2 <split [2930/1082]> Bootstrap02 <recipe> <lm>  
    ##  3 <split [2930/1072]> Bootstrap03 <recipe> <lm>  
    ##  4 <split [2930/1077]> Bootstrap04 <recipe> <lm>  
    ##  5 <split [2930/1074]> Bootstrap05 <recipe> <lm>  
    ##  6 <split [2930/1088]> Bootstrap06 <recipe> <lm>  
    ##  7 <split [2930/1076]> Bootstrap07 <recipe> <lm>  
    ##  8 <split [2930/1068]> Bootstrap08 <recipe> <lm>  
    ##  9 <split [2930/1062]> Bootstrap09 <recipe> <lm>  
    ## 10 <split [2930/1064]> Bootstrap10 <recipe> <lm>  
    ## # ℹ 15 more rows

To get predictions, the function needs three arguments: the splits (to
get the assessment data), the recipe (to process them), and the model.
To iterate over these, the function
[`purrr::pmap()`](https://purrr.tidyverse.org/reference/pmap.html) is
used:

``` r
pred_lm <- function(split_obj, rec_obj, model_obj, ...) {
  mod_data <- bake(
    rec_obj, 
    new_data = assessment(split_obj),
    all_predictors(),
    all_outcomes()
  ) 
  
  out <- mod_data |> select(Sale_Price)
  out$predicted <- predict(model_obj, newdata = mod_data |> select(-Sale_Price))
  out
}

bt_samples$pred <- 
  pmap(
    lst(
      split_obj = bt_samples$splits, 
      rec_obj = bt_samples$recipes, 
      model_obj = bt_samples$lm_mod
    ),
    pred_lm 
  )
bt_samples
```

    ## # Bootstrap sampling 
    ## # A tibble: 25 × 5
    ##    splits              id          recipes  lm_mod pred                
    ##    <list>              <chr>       <list>   <list> <list>              
    ##  1 <split [2930/1095]> Bootstrap01 <recipe> <lm>   <tibble [1,095 × 2]>
    ##  2 <split [2930/1082]> Bootstrap02 <recipe> <lm>   <tibble [1,082 × 2]>
    ##  3 <split [2930/1072]> Bootstrap03 <recipe> <lm>   <tibble [1,072 × 2]>
    ##  4 <split [2930/1077]> Bootstrap04 <recipe> <lm>   <tibble [1,077 × 2]>
    ##  5 <split [2930/1074]> Bootstrap05 <recipe> <lm>   <tibble [1,074 × 2]>
    ##  6 <split [2930/1088]> Bootstrap06 <recipe> <lm>   <tibble [1,088 × 2]>
    ##  7 <split [2930/1076]> Bootstrap07 <recipe> <lm>   <tibble [1,076 × 2]>
    ##  8 <split [2930/1068]> Bootstrap08 <recipe> <lm>   <tibble [1,068 × 2]>
    ##  9 <split [2930/1062]> Bootstrap09 <recipe> <lm>   <tibble [1,062 × 2]>
    ## 10 <split [2930/1064]> Bootstrap10 <recipe> <lm>   <tibble [1,064 × 2]>
    ## # ℹ 15 more rows

Calculating the RMSE:

``` r
library(yardstick)
results <- map(bt_samples$pred, rmse, Sale_Price, predicted) |> list_rbind()
results
```

    ## # A tibble: 25 × 3
    ##    .metric .estimator .estimate
    ##    <chr>   <chr>          <dbl>
    ##  1 rmse    standard       0.132
    ##  2 rmse    standard       0.128
    ##  3 rmse    standard       0.129
    ##  4 rmse    standard       0.123
    ##  5 rmse    standard       0.125
    ##  6 rmse    standard       0.140
    ##  7 rmse    standard       0.129
    ##  8 rmse    standard       0.130
    ##  9 rmse    standard       0.122
    ## 10 rmse    standard       0.127
    ## # ℹ 15 more rows

``` r
mean(results$.estimate)
```

    ## [1] 0.129
