# Package index

## Resampling methods

- [`initial_split()`](https://rsample.tidymodels.org/dev/reference/initial_split.md)
  [`initial_time_split()`](https://rsample.tidymodels.org/dev/reference/initial_split.md)
  [`training()`](https://rsample.tidymodels.org/dev/reference/initial_split.md)
  [`testing()`](https://rsample.tidymodels.org/dev/reference/initial_split.md)
  [`group_initial_split()`](https://rsample.tidymodels.org/dev/reference/initial_split.md)
  : Simple Training/Test Set Splitting
- [`initial_validation_split()`](https://rsample.tidymodels.org/dev/reference/initial_validation_split.md)
  [`initial_validation_time_split()`](https://rsample.tidymodels.org/dev/reference/initial_validation_split.md)
  [`group_initial_validation_split()`](https://rsample.tidymodels.org/dev/reference/initial_validation_split.md)
  [`training(`*`<initial_validation_split>`*`)`](https://rsample.tidymodels.org/dev/reference/initial_validation_split.md)
  [`testing(`*`<initial_validation_split>`*`)`](https://rsample.tidymodels.org/dev/reference/initial_validation_split.md)
  [`validation()`](https://rsample.tidymodels.org/dev/reference/initial_validation_split.md)
  : Create an Initial Train/Validation/Test Split
- [`validation_set()`](https://rsample.tidymodels.org/dev/reference/validation_set.md)
  [`analysis(`*`<val_split>`*`)`](https://rsample.tidymodels.org/dev/reference/validation_set.md)
  [`assessment(`*`<val_split>`*`)`](https://rsample.tidymodels.org/dev/reference/validation_set.md)
  [`training(`*`<val_split>`*`)`](https://rsample.tidymodels.org/dev/reference/validation_set.md)
  [`validation(`*`<val_split>`*`)`](https://rsample.tidymodels.org/dev/reference/validation_set.md)
  [`testing(`*`<val_split>`*`)`](https://rsample.tidymodels.org/dev/reference/validation_set.md)
  : Create a Validation Split for Tuning
- [`bootstraps()`](https://rsample.tidymodels.org/dev/reference/bootstraps.md)
  : Bootstrap Sampling
- [`group_bootstraps()`](https://rsample.tidymodels.org/dev/reference/group_bootstraps.md)
  : Group Bootstraps
- [`vfold_cv()`](https://rsample.tidymodels.org/dev/reference/vfold_cv.md)
  : V-Fold Cross-Validation
- [`group_vfold_cv()`](https://rsample.tidymodels.org/dev/reference/group_vfold_cv.md)
  : Group V-Fold Cross-Validation
- [`mc_cv()`](https://rsample.tidymodels.org/dev/reference/mc_cv.md) :
  Monte Carlo Cross-Validation
- [`group_mc_cv()`](https://rsample.tidymodels.org/dev/reference/group_mc_cv.md)
  : Group Monte Carlo Cross-Validation
- [`clustering_cv()`](https://rsample.tidymodels.org/dev/reference/clustering_cv.md)
  : Cluster Cross-Validation
- [`nested_cv()`](https://rsample.tidymodels.org/dev/reference/nested_cv.md)
  : Nested or Double Resampling
- [`loo_cv()`](https://rsample.tidymodels.org/dev/reference/loo_cv.md) :
  Leave-One-Out Cross-Validation
- [`rolling_origin()`](https://rsample.tidymodels.org/dev/reference/rolling_origin.md)
  **\[superseded\]** : Rolling Origin Forecast Resampling
- [`sliding_window()`](https://rsample.tidymodels.org/dev/reference/slide-resampling.md)
  [`sliding_index()`](https://rsample.tidymodels.org/dev/reference/slide-resampling.md)
  [`sliding_period()`](https://rsample.tidymodels.org/dev/reference/slide-resampling.md)
  : Time-based Resampling
- [`apparent()`](https://rsample.tidymodels.org/dev/reference/apparent.md)
  : Sampling for the Apparent Error Rate
- [`permutations()`](https://rsample.tidymodels.org/dev/reference/permutations.md)
  : Permutation sampling
- [`manual_rset()`](https://rsample.tidymodels.org/dev/reference/manual_rset.md)
  : Manual resampling

## Analysis

- [`int_pctl()`](https://rsample.tidymodels.org/dev/reference/int_pctl.md)
  [`int_t()`](https://rsample.tidymodels.org/dev/reference/int_pctl.md)
  [`int_bca()`](https://rsample.tidymodels.org/dev/reference/int_pctl.md)
  : Bootstrap confidence intervals
- [`reg_intervals()`](https://rsample.tidymodels.org/dev/reference/reg_intervals.md)
  : A convenience function for confidence intervals with linear-ish
  parametric models

## Utilities

- [`.get_fingerprint()`](https://rsample.tidymodels.org/dev/reference/get_fingerprint.md)
  : Obtain a identifier for the resamples

- [`as.data.frame(`*`<rsplit>`*`)`](https://rsample.tidymodels.org/dev/reference/as.data.frame.rsplit.md)
  [`analysis()`](https://rsample.tidymodels.org/dev/reference/as.data.frame.rsplit.md)
  [`assessment()`](https://rsample.tidymodels.org/dev/reference/as.data.frame.rsplit.md)
  :

  Convert an `rsplit` object to a data frame

- [`add_resample_id()`](https://rsample.tidymodels.org/dev/reference/add_resample_id.md)
  : Augment a data set with resampling identifiers

- [`complement()`](https://rsample.tidymodels.org/dev/reference/complement.md)
  : Determine the Assessment Samples

- [`form_pred()`](https://rsample.tidymodels.org/dev/reference/form_pred.md)
  : Extract Predictor Names from Formula or Terms

- [`get_rsplit()`](https://rsample.tidymodels.org/dev/reference/get_rsplit.md)
  : Retrieve individual rsplits objects from an rset

- [`labels(`*`<rset>`*`)`](https://rsample.tidymodels.org/dev/reference/labels.rset.md)
  [`labels(`*`<vfold_cv>`*`)`](https://rsample.tidymodels.org/dev/reference/labels.rset.md)
  : Find Labels from rset Object

- [`labels(`*`<rsplit>`*`)`](https://rsample.tidymodels.org/dev/reference/labels.rsplit.md)
  : Find Labels from rsplit Object

- [`make_splits()`](https://rsample.tidymodels.org/dev/reference/make_splits.md)
  : Constructors for split objects

- [`make_strata()`](https://rsample.tidymodels.org/dev/reference/make_strata.md)
  : Create or Modify Stratification Variables

- [`populate()`](https://rsample.tidymodels.org/dev/reference/populate.md)
  : Add Assessment Indices

- [`reshuffle_rset()`](https://rsample.tidymodels.org/dev/reference/reshuffle_rset.md)
  : "Reshuffle" an rset to re-generate a new rset with the same
  parameters

- [`reverse_splits()`](https://rsample.tidymodels.org/dev/reference/reverse_splits.md)
  : Reverse the analysis and assessment sets

- [`rsample2caret()`](https://rsample.tidymodels.org/dev/reference/rsample2caret.md)
  [`caret2rsample()`](https://rsample.tidymodels.org/dev/reference/rsample2caret.md)
  : Convert Resampling Objects to Other Formats

- [`rset_reconstruct()`](https://rsample.tidymodels.org/dev/reference/rset_reconstruct.md)
  : Extending rsample with new rset subclasses

- [`tidy(`*`<rsplit>`*`)`](https://rsample.tidymodels.org/dev/reference/tidy.rsplit.md)
  [`tidy(`*`<rset>`*`)`](https://rsample.tidymodels.org/dev/reference/tidy.rsplit.md)
  [`tidy(`*`<vfold_cv>`*`)`](https://rsample.tidymodels.org/dev/reference/tidy.rsplit.md)
  [`tidy(`*`<nested_cv>`*`)`](https://rsample.tidymodels.org/dev/reference/tidy.rsplit.md)
  : Tidy Resampling Object

## Compatibility

- [`rsample-dplyr`](https://rsample.tidymodels.org/dev/reference/rsample-dplyr.md)
  : Compatibility with dplyr
