# rsample 1.2.0

* The new `initial_validation_split()`, along with variants `initial_validation_time_split()` and `group_initial_validation_split()`, generates a three-way split of the data into training, validation, and test sets. With the new `validation_set()`, this can be turned into an `rset` object for tuning (#403, #446).

* `validation_split()`, `validation_time_split()`, and `group_validation_split()` have been soft-deprecated in favor of the new functions implementing a 3-way split (`initial_validation_split()`, `initial_validation_time_split()`, and `group_initial_validation_split()`) (#449).

* Functions which don't use the ellipsis `...` now enforce empty dots (#429).

* `make_splits()` gained an example in the documentation (@AngelFelizR, #432).

* `training()`, `testing()`, `analysis()`, and `assessment()` are now S3 generics with methods for `rsplit` objects. Previously they manually required the input to be an `rsplit` object (#384).

* The `int_*()` functions are now S3 generics and have corresponding methods for class `bootstraps` (#435). 

* The underlying mechanics of data splitting were changed so that `Surv` objects maintain their class. This change affects the row names of the resulting objects; they are reindexed from one instead of being a subset of the original row names (#443).

* rsample does not re-export `gather()` anymore (#451).


# rsample 1.1.1

* All grouped resampling functions (`group_vfold_cv()`, `group_mc_cv()`, `group_initial_split()` and `group_validation_split()`, and `group_bootstraps()`) now support stratification. Strata must be constant within each group (@mikemahoney218, #317, #360, #363, #364, #365).

* Added a new function, `clustering_cv()`, for blocked cross-validation in various predictor spaces. This is a very flexible function, taking arguments to both `distance_function` and `cluster_function`, allowing it to be used for spatial clustering as well as potentially phylogenetic and other forms of clustering (@mikemahoney218, #351).

* `bootstraps()` and `group_bootstraps()` now warn if resampling returns any empty assessment sets. Previously, `bootstraps()` was silent while `group_bootstraps()` errored (@mikemahoney218, #356, #357).

* The assessment set of `validation_time_split()` now also contains the lagged observations (#376).

* The new helper `get_rsplit()` lets you conveniently access the `rsplit` objects inside an `rset` objects (@mikemahoney218, #399).

* The result of `initial_time_split()` now has its own subclass `"initial_time_split"`, in addition to existing classes (#397).

* The dependency on the ellipsis package has been removed (#393).

* Removed an overly strict test in preparation for dplyr 1.1.0 (#380).


# rsample 1.1.0

* rset objects now include all parameters used to create them as attributes (#329).

* Objects returned by sliding functions now have an `index` attribute, where appropriate, containing the column name used as an index (#329).

* Objects returned by `permutations()` now have a `permutes` attribute containing the column name used for permutation (#329).

* Added `breaks` and `pool` as attributes to all functions which support stratification (#329).

* Changed the "strata" attribute on rset objects so that it now is either a character vector identifying the column used to stratify the data, and is not present (set to `NULL`) if stratification was not used. (#329)

* Added a new function, `reshuffle_rset()`, which takes an `rset` object and generates a new version of it using the same arguments but the current random seed. (#79, #329)

* Added arguments to control how `group_vfold_cv()` combines groups. Use `balance = "groups"` to assign (roughly) the same number of groups to each fold, or `balance = "observations"` to assign (roughly) the same number of observations to each fold.

* Added a `repeats` argument to `group_vfold_cv()` (#330).

* Added new functions for grouped resampling: `group_mc_cv()` (#313), `group_initial_split()` and `group_validation_split()` (#315), and `group_bootstraps()` (#316).

* Added a new function, `reverse_splits()`, to swap analysis and assessment splits (#319, #284).

* Improved the error thrown when calling `assessment()` on a `perm_split` object created by `permutations()` (#321, #322).

# rsample 1.0.0

* Fixed how `nested_cv()` handles call objects so variables in the environment can be used when specifying resampling schemes (#81).

* Updated to testthat 3e (#280) and added better checking for `vfold_cv()` (#293).

* Finally removed the `gather()` method for `rset` objects. Use `tidyr::pivot_longer()` instead (#280).

* Changed `initial_split()` to avoid calling tidyselect twice on `strata` (#296). This fix stops `initial_split()` from generating messages like:

```
  Note: Using an external vector in selections is ambiguous.
  i Use `all_of(strata)` instead of `strata` to silence this message.
  i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.
```

* Added better printing methods for initial split objects.

# rsample 0.1.1

* Updated documentation on stratified sampling (#245).

* Changed `make_splits()` to an S3 generic, with the original functionality a method for `list` and a new method for dataframes that allows users to create a split from existing analysis & assessment sets (@LiamBlake, #246).

* Added `validation_time_split()` for a single validation sample taking the first samples for training (@mine-cetinkaya-rundel, #256).

* Escalated the deprecation of the `gather()` method for `rset` objects to a hard deprecation. Use `tidyr::pivot_longer()` instead (#257).

* Changed resample "fingerprint" to hash the indices only rather than the entire resample result (including the data object). This is much faster and will still ensure the same resample for the same original data object (#259).

# rsample 0.1.0

* Fixed how `mc_cv()`, `initial_split()`, and `validation_split()` use the `prop` argument to first compute the assessment indices, rather than the analysis indices. This is a minor but **breaking change** in some situations; the previous implementation could cause an inconsistency in the sizes of the generated analysis and assessment sets when compared to how `prop` is documented to function (#217, @issactoast).

* Fixed problem with creation of `apparent()` (#223) and `caret2rsample()` (#232) resamples.

* Re-licensed package from GPL-2 to MIT. See [consent from copyright holders here](https://github.com/tidymodels/rsample/issues/226).

* Attempts to stratify on a `Surv` object now error more informatively (#230). 

* Exposed `pool` argument from `make_strata()` in user-facing resampling functions (#229).

* Deprecated the `gather()` method for `rset` objects in favor of `tidyr::pivot_longer()` (#233).

* Fixed bug in `make_strata()` for numeric variables with `NA` values (@brian-j-smith, #236).

# rsample 0.0.9

* New `rset_reconstruct()`, a developer tool to ease creation of new rset subclasses (#210).

* Added `permutations()`, a function for creating permutation resamples by performing column-wise shuffling (@mattwarkentin, #198).

* Fixed an issue where empty assessment sets couldn't be created by `make_splits()` (#188).

* `rset` objects now contain a "fingerprint" attribute that can be used to check to see if the same object uses the same resamples. 

* The `reg_intervals()` function is a convenience function for `lm()`, `glm()`, `survreg()`, and `coxph()` models (#206). 

* A few internal functions were exported so that `rsample`-adjacent packages can use the same underlying code. 

* The `obj_sum()` method for `rsplit` objects was updated (#215).

* Changed the inheritance structure for `rsplit` objects from specific to general and simplified the methods for the `complement()` generic (#216).


# rsample 0.0.8

* New `manual_rset()` for constructing rset objects manually from custom rsplits (tidymodels/tune#273).

* Three new time based resampling functions have been added: `sliding_window()`, `sliding_index()`, and `sliding_period()`, which have more flexibility than the pre-existing `rolling_origin()`.
  
* Correct `alpha` parameter handling for bootstrap CI functions (#179, #184).

# rsample 0.0.7

* Lower threshold for pooling strata to 10% (from 15%) (#149).

* The `print()` methods for `rsplit` and `val_split` objects were adjusted to show `"<Analysis/Assess/Total>"` and `<Training/Validation/Total>`, respectively. 

* The `drinks`, `attrition`, and `two_class_dat` data sets were removed. They are in the `modeldata` package. 

* Compatability with `dplyr` 1.0.0.

# `rsample` 0.0.6

* Added `validation_set()` for making a single resample.

* Correct the tidy method for bootstraps (#115).

* Changes for upcoming `tibble release. 

* Exported constructors for `rset` and `split` objects (#40)

* `initial_time_split()` and `rolling_origin()` now have a `lag` parameter that ensures that previous data are available so that lagged variables can be calculated. (#135, #136)

# `rsample` 0.0.5

* Added three functions to compute different bootstrap confidence intervals. 
* A new function (`add_resample_id()`) augments a data frame with columns for the resampling identifier. 
* Updated `initial_split()`, `mc_cv()`, `vfold_cv()`, `bootstraps()`, and `group_vfold_cv()` to use tidyselect on the stratification variable.
* Updated `initial_split()`, `mc_cv()`, `vfold_cv()`, `bootstraps()` with new `breaks` parameter that specifies the number of bins to stratify by for a numeric stratification variable.


# `rsample` 0.0.4

Small maintenance release. 

## Minor improvements and fixes

 * `fill()` was removed per the deprecation warning. 
 * Small changes were made for the new version of `tibble`. 

# `rsample` 0.0.3

## New features

* Added function `initial_time_split()` for ordered initial sampling appropriate for time series data.

## Minor improvements and fixes

* `fill()` has been renamed `populate()` to avoid a conflict with `tidyr::fill()`.

* Changed the R version requirement to be R >= 3.1 instead of 3.3.3. 

* The `recipes`-related `prepper` function was [moved to the `recipes` package](https://github.com/tidymodels/rsample/issues/48). This makes the `rsample` install footprint much smaller.

* `rsplit` objects are shown differently inside of a tibble.

* Moved from the `broom` package to the `generics` package.


# `rsample` 0.0.2

* `initial_split`, `training`, and `testing` were added to do training/testing splits prior to resampling. 
* Another resampling method, `group_vfold_cv`, was added. 
* `caret2rsample` and `rsample2caret` can convert `rset` objects to those used by `caret::trainControl` and vice-versa. 
* A function called `form_pred` can be used to determine the original names of the predictors in a formula or `terms` object. 
* A vignette and a function (`prepper`) were included to facilitate using the `recipes` with `rsample`.
* A `gather` method was added for `rset` objects.
* A `labels` method was added for `rsplit` objects. This can help identify which resample is being used even when the whole `rset` object is not available. 
* A variety of `dplyr` methods were added (e.g. `filter`, `mutate`, etc) that work without dropping classes or attributes of the `rsample` objects. 

# `rsample` 0.0.1 (2017-07-08)

Initial public version on CRAN

