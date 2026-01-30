# Changelog

## rsample (development version)

## rsample 1.3.2

CRAN release: 2026-01-30

- [`.get_split_args()`](https://rsample.tidymodels.org/dev/reference/dot-get_split_args.md)
  now works with rset objects created by functions from spatialsample
  without requiring the package to be attached
  ([\#599](https://github.com/tidymodels/rsample/issues/599)).

- The `lag` argument for
  [`initial_time_split()`](https://rsample.tidymodels.org/dev/reference/initial_split.md)
  has been soft deprecated
  ([@bjornkallerud](https://github.com/bjornkallerud),
  [\#592](https://github.com/tidymodels/rsample/issues/592)).

- The example for
  [`group_vfold_cv()`](https://rsample.tidymodels.org/dev/reference/group_vfold_cv.md)
  has been updated to use current dplyr functions
  ([\#597](https://github.com/tidymodels/rsample/issues/597)).

## rsample 1.3.1

CRAN release: 2025-07-29

- The new
  [`internal_calibration_split()`](https://rsample.tidymodels.org/dev/reference/internal_calibration_split.md)
  function and its methods for various resamples is for usage in tune to
  create a internal split of the analysis set to fit the preprocessor
  and model on one part and the post-processor on the other part
  ([\#483](https://github.com/tidymodels/rsample/issues/483),
  [\#488](https://github.com/tidymodels/rsample/issues/488),
  [\#489](https://github.com/tidymodels/rsample/issues/489),
  [\#569](https://github.com/tidymodels/rsample/issues/569),
  [\#575](https://github.com/tidymodels/rsample/issues/575),
  [\#577](https://github.com/tidymodels/rsample/issues/577),
  [\#582](https://github.com/tidymodels/rsample/issues/582)).

- New accessor function
  [`calibration()`](https://rsample.tidymodels.org/dev/reference/internal_calibration_split.md)
  for the calibration set of an internal calibration split
  ([\#581](https://github.com/tidymodels/rsample/issues/581)).

## rsample 1.3.0

CRAN release: 2025-04-02

- Bootstrap intervals via
  [`int_pctl()`](https://rsample.tidymodels.org/dev/reference/int_pctl.md),
  [`int_t()`](https://rsample.tidymodels.org/dev/reference/int_pctl.md),
  and
  [`int_bca()`](https://rsample.tidymodels.org/dev/reference/int_pctl.md)
  now allow for more flexible grouping
  ([\#465](https://github.com/tidymodels/rsample/issues/465)).

- Errors and warnings are now styled via cli
  ([\#499](https://github.com/tidymodels/rsample/issues/499),
  [\#502](https://github.com/tidymodels/rsample/issues/502)). Largely
  done by [@PriKalra](https://github.com/PriKalra)
  ([\#523](https://github.com/tidymodels/rsample/issues/523),
  [\#526](https://github.com/tidymodels/rsample/issues/526),
  [\#528](https://github.com/tidymodels/rsample/issues/528),
  [\#530](https://github.com/tidymodels/rsample/issues/530),
  [\#531](https://github.com/tidymodels/rsample/issues/531),
  [\#532](https://github.com/tidymodels/rsample/issues/532)),
  [@Dpananos](https://github.com/Dpananos)
  ([\#516](https://github.com/tidymodels/rsample/issues/516),
  [\#517](https://github.com/tidymodels/rsample/issues/517),
  [\#529](https://github.com/tidymodels/rsample/issues/529)), and
  [@JamesHWade](https://github.com/JamesHWade)
  ([\#518](https://github.com/tidymodels/rsample/issues/518)) as part of
  the tidyverse dev day.

- [`rolling_origin()`](https://rsample.tidymodels.org/dev/reference/rolling_origin.md)
  is now superseded by
  [`sliding_window()`](https://rsample.tidymodels.org/dev/reference/slide-resampling.md),
  [`sliding_index()`](https://rsample.tidymodels.org/dev/reference/slide-resampling.md),
  and
  [`sliding_period()`](https://rsample.tidymodels.org/dev/reference/slide-resampling.md)
  which provide more flexibility and control
  ([@nmercadeb](https://github.com/nmercadeb),
  [\#524](https://github.com/tidymodels/rsample/issues/524)).

- The deprecation of
  [`validation_split()`](https://rsample.tidymodels.org/dev/reference/validation_split.md),
  [`validation_time_split()`](https://rsample.tidymodels.org/dev/reference/validation_split.md),
  and
  [`group_validation_split()`](https://rsample.tidymodels.org/dev/reference/validation_split.md)
  has been moved to the next level so that they now warn.

### Bug fixes

- [`vfold_cv()`](https://rsample.tidymodels.org/dev/reference/vfold_cv.md)
  now utilizes the `breaks` argument correctly for repeated
  cross-validation ([@ZWael](https://github.com/ZWael),
  [\#471](https://github.com/tidymodels/rsample/issues/471)).

- Grouped resampling functions now work with an explicit `strata = NULL`
  instead of strata being either a name or missing
  ([\#485](https://github.com/tidymodels/rsample/issues/485)).

### Breaking changes

- [`vfold_cv()`](https://rsample.tidymodels.org/dev/reference/vfold_cv.md)
  and
  [`clustering_cv()`](https://rsample.tidymodels.org/dev/reference/clustering_cv.md)
  now error on implicit leave-one-out cross-validation
  ([@seb09](https://github.com/seb09),
  [\#527](https://github.com/tidymodels/rsample/issues/527)).

- The class of grouped MC splits is now `group_mc_split` instead of
  `grouped_mc_split`, aligning it with the other grouped splits
  ([\#478](https://github.com/tidymodels/rsample/issues/478)).

- The `rsplit` objects of an
  [`apparent()`](https://rsample.tidymodels.org/dev/reference/apparent.md)
  split now have the correct class inheritance structure. The order is
  now `apparent_split` and then `rsplit` rather than the other way
  around ([\#477](https://github.com/tidymodels/rsample/issues/477)).

### Documentation improvements

- Improved documentation and formatting: function names are now more
  easily identifiable through either `()` at the end or being links to
  the function documentation ([@brshallo](https://github.com/brshallo) ,
  [\#521](https://github.com/tidymodels/rsample/issues/521)).

- Fixed example for
  [`nested_cv()`](https://rsample.tidymodels.org/dev/reference/nested_cv.md)
  ([@seb09](https://github.com/seb09),
  [\#520](https://github.com/tidymodels/rsample/issues/520)).

- Formatting improvement: package names are now not in backticks anymore
  ([@agmurray](https://github.com/agmurray),
  [\#525](https://github.com/tidymodels/rsample/issues/525)).

- Improved documentation for
  [`initial_split()`](https://rsample.tidymodels.org/dev/reference/initial_split.md)
  and friends ([@laurabrianna](https://github.com/laurabrianna),
  [\#519](https://github.com/tidymodels/rsample/issues/519)).

- Removed trailing space in printing of
  [`mc_cv()`](https://rsample.tidymodels.org/dev/reference/mc_cv.md)
  objects ([@ccani007](https://github.com/ccani007),
  [\#464](https://github.com/tidymodels/rsample/issues/464)).

## rsample 1.2.1

CRAN release: 2024-03-25

- [`nested_cv()`](https://rsample.tidymodels.org/dev/reference/nested_cv.md)
  no longer errors if `outside` is a long call
  ([\#459](https://github.com/tidymodels/rsample/issues/459),
  [\#461](https://github.com/tidymodels/rsample/issues/461)).

- The `validation_set` class now has its own
  [`pretty()`](https://rdrr.io/r/base/pretty.html) method
  ([\#456](https://github.com/tidymodels/rsample/issues/456)).

## rsample 1.2.0

CRAN release: 2023-08-23

- The new
  [`initial_validation_split()`](https://rsample.tidymodels.org/dev/reference/initial_validation_split.md),
  along with variants
  [`initial_validation_time_split()`](https://rsample.tidymodels.org/dev/reference/initial_validation_split.md)
  and
  [`group_initial_validation_split()`](https://rsample.tidymodels.org/dev/reference/initial_validation_split.md),
  generates a three-way split of the data into training, validation, and
  test sets. With the new
  [`validation_set()`](https://rsample.tidymodels.org/dev/reference/validation_set.md),
  this can be turned into an `rset` object for tuning
  ([\#403](https://github.com/tidymodels/rsample/issues/403),
  [\#446](https://github.com/tidymodels/rsample/issues/446)).

- [`validation_split()`](https://rsample.tidymodels.org/dev/reference/validation_split.md),
  [`validation_time_split()`](https://rsample.tidymodels.org/dev/reference/validation_split.md),
  and
  [`group_validation_split()`](https://rsample.tidymodels.org/dev/reference/validation_split.md)
  have been soft-deprecated in favor of the new functions implementing a
  3-way split
  ([`initial_validation_split()`](https://rsample.tidymodels.org/dev/reference/initial_validation_split.md),
  [`initial_validation_time_split()`](https://rsample.tidymodels.org/dev/reference/initial_validation_split.md),
  and
  [`group_initial_validation_split()`](https://rsample.tidymodels.org/dev/reference/initial_validation_split.md))
  ([\#449](https://github.com/tidymodels/rsample/issues/449)).

- Functions which don’t use the ellipsis `...` now enforce empty dots
  ([\#429](https://github.com/tidymodels/rsample/issues/429)).

- [`make_splits()`](https://rsample.tidymodels.org/dev/reference/make_splits.md)
  gained an example in the documentation
  ([@AngelFelizR](https://github.com/AngelFelizR),
  [\#432](https://github.com/tidymodels/rsample/issues/432)).

- [`training()`](https://rsample.tidymodels.org/dev/reference/initial_split.md),
  [`testing()`](https://rsample.tidymodels.org/dev/reference/initial_split.md),
  [`analysis()`](https://rsample.tidymodels.org/dev/reference/as.data.frame.rsplit.md),
  and
  [`assessment()`](https://rsample.tidymodels.org/dev/reference/as.data.frame.rsplit.md)
  are now S3 generics with methods for `rsplit` objects. Previously they
  manually required the input to be an `rsplit` object
  ([\#384](https://github.com/tidymodels/rsample/issues/384)).

- The `int_*()` functions are now S3 generics and have corresponding
  methods for class `bootstraps`
  ([\#435](https://github.com/tidymodels/rsample/issues/435)).

- The underlying mechanics of data splitting were changed so that `Surv`
  objects maintain their class. This change affects the row names of the
  resulting objects; they are reindexed from one instead of being a
  subset of the original row names
  ([\#443](https://github.com/tidymodels/rsample/issues/443)).

- rsample does not re-export
  [`gather()`](https://tidyr.tidyverse.org/reference/gather.html)
  anymore ([\#451](https://github.com/tidymodels/rsample/issues/451)).

## rsample 1.1.1

CRAN release: 2022-12-07

- All grouped resampling functions
  ([`group_vfold_cv()`](https://rsample.tidymodels.org/dev/reference/group_vfold_cv.md),
  [`group_mc_cv()`](https://rsample.tidymodels.org/dev/reference/group_mc_cv.md),
  [`group_initial_split()`](https://rsample.tidymodels.org/dev/reference/initial_split.md)
  and
  [`group_validation_split()`](https://rsample.tidymodels.org/dev/reference/validation_split.md),
  and
  [`group_bootstraps()`](https://rsample.tidymodels.org/dev/reference/group_bootstraps.md))
  now support stratification. Strata must be constant within each group
  ([@mikemahoney218](https://github.com/mikemahoney218),
  [\#317](https://github.com/tidymodels/rsample/issues/317),
  [\#360](https://github.com/tidymodels/rsample/issues/360),
  [\#363](https://github.com/tidymodels/rsample/issues/363),
  [\#364](https://github.com/tidymodels/rsample/issues/364),
  [\#365](https://github.com/tidymodels/rsample/issues/365)).

- Added a new function,
  [`clustering_cv()`](https://rsample.tidymodels.org/dev/reference/clustering_cv.md),
  for blocked cross-validation in various predictor spaces. This is a
  very flexible function, taking arguments to both `distance_function`
  and `cluster_function`, allowing it to be used for spatial clustering
  as well as potentially phylogenetic and other forms of clustering
  ([@mikemahoney218](https://github.com/mikemahoney218),
  [\#351](https://github.com/tidymodels/rsample/issues/351)).

- [`bootstraps()`](https://rsample.tidymodels.org/dev/reference/bootstraps.md)
  and
  [`group_bootstraps()`](https://rsample.tidymodels.org/dev/reference/group_bootstraps.md)
  now warn if resampling returns any empty assessment sets. Previously,
  [`bootstraps()`](https://rsample.tidymodels.org/dev/reference/bootstraps.md)
  was silent while
  [`group_bootstraps()`](https://rsample.tidymodels.org/dev/reference/group_bootstraps.md)
  errored ([@mikemahoney218](https://github.com/mikemahoney218),
  [\#356](https://github.com/tidymodels/rsample/issues/356),
  [\#357](https://github.com/tidymodels/rsample/issues/357)).

- The assessment set of
  [`validation_time_split()`](https://rsample.tidymodels.org/dev/reference/validation_split.md)
  now also contains the lagged observations
  ([\#376](https://github.com/tidymodels/rsample/issues/376)).

- The new helper
  [`get_rsplit()`](https://rsample.tidymodels.org/dev/reference/get_rsplit.md)
  lets you conveniently access the `rsplit` objects inside an `rset`
  objects ([@mikemahoney218](https://github.com/mikemahoney218),
  [\#399](https://github.com/tidymodels/rsample/issues/399)).

- The result of
  [`initial_time_split()`](https://rsample.tidymodels.org/dev/reference/initial_split.md)
  now has its own subclass `"initial_time_split"`, in addition to
  existing classes
  ([\#397](https://github.com/tidymodels/rsample/issues/397)).

- The dependency on the ellipsis package has been removed
  ([\#393](https://github.com/tidymodels/rsample/issues/393)).

- Removed an overly strict test in preparation for dplyr 1.1.0
  ([\#380](https://github.com/tidymodels/rsample/issues/380)).

## rsample 1.1.0

CRAN release: 2022-08-08

- rset objects now include all parameters used to create them as
  attributes
  ([\#329](https://github.com/tidymodels/rsample/issues/329)).

- Objects returned by sliding functions now have an `index` attribute,
  where appropriate, containing the column name used as an index
  ([\#329](https://github.com/tidymodels/rsample/issues/329)).

- Objects returned by
  [`permutations()`](https://rsample.tidymodels.org/dev/reference/permutations.md)
  now have a `permutes` attribute containing the column name used for
  permutation
  ([\#329](https://github.com/tidymodels/rsample/issues/329)).

- Added `breaks` and `pool` as attributes to all functions which support
  stratification
  ([\#329](https://github.com/tidymodels/rsample/issues/329)).

- Changed the “strata” attribute on rset objects so that it now is
  either a character vector identifying the column used to stratify the
  data, and is not present (set to `NULL`) if stratification was not
  used. ([\#329](https://github.com/tidymodels/rsample/issues/329))

- Added a new function,
  [`reshuffle_rset()`](https://rsample.tidymodels.org/dev/reference/reshuffle_rset.md),
  which takes an `rset` object and generates a new version of it using
  the same arguments but the current random seed.
  ([\#79](https://github.com/tidymodels/rsample/issues/79),
  [\#329](https://github.com/tidymodels/rsample/issues/329))

- Added arguments to control how
  [`group_vfold_cv()`](https://rsample.tidymodels.org/dev/reference/group_vfold_cv.md)
  combines groups. Use `balance = "groups"` to assign (roughly) the same
  number of groups to each fold, or `balance = "observations"` to assign
  (roughly) the same number of observations to each fold.

- Added a `repeats` argument to
  [`group_vfold_cv()`](https://rsample.tidymodels.org/dev/reference/group_vfold_cv.md)
  ([\#330](https://github.com/tidymodels/rsample/issues/330)).

- Added new functions for grouped resampling:
  [`group_mc_cv()`](https://rsample.tidymodels.org/dev/reference/group_mc_cv.md)
  ([\#313](https://github.com/tidymodels/rsample/issues/313)),
  [`group_initial_split()`](https://rsample.tidymodels.org/dev/reference/initial_split.md)
  and
  [`group_validation_split()`](https://rsample.tidymodels.org/dev/reference/validation_split.md)
  ([\#315](https://github.com/tidymodels/rsample/issues/315)), and
  [`group_bootstraps()`](https://rsample.tidymodels.org/dev/reference/group_bootstraps.md)
  ([\#316](https://github.com/tidymodels/rsample/issues/316)).

- Added a new function,
  [`reverse_splits()`](https://rsample.tidymodels.org/dev/reference/reverse_splits.md),
  to swap analysis and assessment splits
  ([\#319](https://github.com/tidymodels/rsample/issues/319),
  [\#284](https://github.com/tidymodels/rsample/issues/284)).

- Improved the error thrown when calling
  [`assessment()`](https://rsample.tidymodels.org/dev/reference/as.data.frame.rsplit.md)
  on a `perm_split` object created by
  [`permutations()`](https://rsample.tidymodels.org/dev/reference/permutations.md)
  ([\#321](https://github.com/tidymodels/rsample/issues/321),
  [\#322](https://github.com/tidymodels/rsample/issues/322)).

## rsample 1.0.0

CRAN release: 2022-06-24

- Fixed how
  [`nested_cv()`](https://rsample.tidymodels.org/dev/reference/nested_cv.md)
  handles call objects so variables in the environment can be used when
  specifying resampling schemes
  ([\#81](https://github.com/tidymodels/rsample/issues/81)).

- Updated to testthat 3e
  ([\#280](https://github.com/tidymodels/rsample/issues/280)) and added
  better checking for
  [`vfold_cv()`](https://rsample.tidymodels.org/dev/reference/vfold_cv.md)
  ([\#293](https://github.com/tidymodels/rsample/issues/293)).

- Finally removed the
  [`gather()`](https://tidyr.tidyverse.org/reference/gather.html) method
  for `rset` objects. Use
  [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
  instead ([\#280](https://github.com/tidymodels/rsample/issues/280)).

- Changed
  [`initial_split()`](https://rsample.tidymodels.org/dev/reference/initial_split.md)
  to avoid calling tidyselect twice on `strata`
  ([\#296](https://github.com/tidymodels/rsample/issues/296)). This fix
  stops
  [`initial_split()`](https://rsample.tidymodels.org/dev/reference/initial_split.md)
  from generating messages like:

&nbsp;

      Note: Using an external vector in selections is ambiguous.
      i Use `all_of(strata)` instead of `strata` to silence this message.
      i See <https://tidyselect.r-lib.org/reference/faq-external-vector.html>.

- Added better printing methods for initial split objects.

## rsample 0.1.1

CRAN release: 2021-11-08

- Updated documentation on stratified sampling
  ([\#245](https://github.com/tidymodels/rsample/issues/245)).

- Changed
  [`make_splits()`](https://rsample.tidymodels.org/dev/reference/make_splits.md)
  to an S3 generic, with the original functionality a method for `list`
  and a new method for dataframes that allows users to create a split
  from existing analysis & assessment sets
  ([@LiamBlake](https://github.com/LiamBlake),
  [\#246](https://github.com/tidymodels/rsample/issues/246)).

- Added
  [`validation_time_split()`](https://rsample.tidymodels.org/dev/reference/validation_split.md)
  for a single validation sample taking the first samples for training
  ([@mine-cetinkaya-rundel](https://github.com/mine-cetinkaya-rundel),
  [\#256](https://github.com/tidymodels/rsample/issues/256)).

- Escalated the deprecation of the
  [`gather()`](https://tidyr.tidyverse.org/reference/gather.html) method
  for `rset` objects to a hard deprecation. Use
  [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
  instead ([\#257](https://github.com/tidymodels/rsample/issues/257)).

- Changed resample “fingerprint” to hash the indices only rather than
  the entire resample result (including the data object). This is much
  faster and will still ensure the same resample for the same original
  data object
  ([\#259](https://github.com/tidymodels/rsample/issues/259)).

## rsample 0.1.0

CRAN release: 2021-05-08

- Fixed how
  [`mc_cv()`](https://rsample.tidymodels.org/dev/reference/mc_cv.md),
  [`initial_split()`](https://rsample.tidymodels.org/dev/reference/initial_split.md),
  and
  [`validation_split()`](https://rsample.tidymodels.org/dev/reference/validation_split.md)
  use the `prop` argument to first compute the assessment indices,
  rather than the analysis indices. This is a minor but **breaking
  change** in some situations; the previous implementation could cause
  an inconsistency in the sizes of the generated analysis and assessment
  sets when compared to how `prop` is documented to function
  ([\#217](https://github.com/tidymodels/rsample/issues/217),
  [@issactoast](https://github.com/issactoast)).

- Fixed problem with creation of
  [`apparent()`](https://rsample.tidymodels.org/dev/reference/apparent.md)
  ([\#223](https://github.com/tidymodels/rsample/issues/223)) and
  [`caret2rsample()`](https://rsample.tidymodels.org/dev/reference/rsample2caret.md)
  ([\#232](https://github.com/tidymodels/rsample/issues/232)) resamples.

- Re-licensed package from GPL-2 to MIT. See [consent from copyright
  holders here](https://github.com/tidymodels/rsample/issues/226).

- Attempts to stratify on a `Surv` object now error more informatively
  ([\#230](https://github.com/tidymodels/rsample/issues/230)).

- Exposed `pool` argument from
  [`make_strata()`](https://rsample.tidymodels.org/dev/reference/make_strata.md)
  in user-facing resampling functions
  ([\#229](https://github.com/tidymodels/rsample/issues/229)).

- Deprecated the
  [`gather()`](https://tidyr.tidyverse.org/reference/gather.html) method
  for `rset` objects in favor of
  [`tidyr::pivot_longer()`](https://tidyr.tidyverse.org/reference/pivot_longer.html)
  ([\#233](https://github.com/tidymodels/rsample/issues/233)).

- Fixed bug in
  [`make_strata()`](https://rsample.tidymodels.org/dev/reference/make_strata.md)
  for numeric variables with `NA` values
  ([@brian-j-smith](https://github.com/brian-j-smith),
  [\#236](https://github.com/tidymodels/rsample/issues/236)).

## rsample 0.0.9

CRAN release: 2021-02-17

- New
  [`rset_reconstruct()`](https://rsample.tidymodels.org/dev/reference/rset_reconstruct.md),
  a developer tool to ease creation of new rset subclasses
  ([\#210](https://github.com/tidymodels/rsample/issues/210)).

- Added
  [`permutations()`](https://rsample.tidymodels.org/dev/reference/permutations.md),
  a function for creating permutation resamples by performing
  column-wise shuffling
  ([@mattwarkentin](https://github.com/mattwarkentin),
  [\#198](https://github.com/tidymodels/rsample/issues/198)).

- Fixed an issue where empty assessment sets couldn’t be created by
  [`make_splits()`](https://rsample.tidymodels.org/dev/reference/make_splits.md)
  ([\#188](https://github.com/tidymodels/rsample/issues/188)).

- `rset` objects now contain a “fingerprint” attribute that can be used
  to check to see if the same object uses the same resamples.

- The
  [`reg_intervals()`](https://rsample.tidymodels.org/dev/reference/reg_intervals.md)
  function is a convenience function for
  [`lm()`](https://rdrr.io/r/stats/lm.html),
  [`glm()`](https://rdrr.io/r/stats/glm.html), `survreg()`, and
  `coxph()` models
  ([\#206](https://github.com/tidymodels/rsample/issues/206)).

- A few internal functions were exported so that rsample-adjacent
  packages can use the same underlying code.

- The [`obj_sum()`](https://pillar.r-lib.org/reference/type_sum.html)
  method for `rsplit` objects was updated
  ([\#215](https://github.com/tidymodels/rsample/issues/215)).

- Changed the inheritance structure for `rsplit` objects from specific
  to general and simplified the methods for the
  [`complement()`](https://rsample.tidymodels.org/dev/reference/complement.md)
  generic ([\#216](https://github.com/tidymodels/rsample/issues/216)).

## rsample 0.0.8

CRAN release: 2020-09-23

- New
  [`manual_rset()`](https://rsample.tidymodels.org/dev/reference/manual_rset.md)
  for constructing rset objects manually from custom rsplits
  (tidymodels/tune#273).

- Three new time based resampling functions have been added:
  [`sliding_window()`](https://rsample.tidymodels.org/dev/reference/slide-resampling.md),
  [`sliding_index()`](https://rsample.tidymodels.org/dev/reference/slide-resampling.md),
  and
  [`sliding_period()`](https://rsample.tidymodels.org/dev/reference/slide-resampling.md),
  which have more flexibility than the pre-existing
  [`rolling_origin()`](https://rsample.tidymodels.org/dev/reference/rolling_origin.md).

- Correct `alpha` parameter handling for bootstrap CI functions
  ([\#179](https://github.com/tidymodels/rsample/issues/179),
  [\#184](https://github.com/tidymodels/rsample/issues/184)).

## rsample 0.0.7

CRAN release: 2020-06-04

- Lower threshold for pooling strata to 10% (from 15%)
  ([\#149](https://github.com/tidymodels/rsample/issues/149)).

- The [`print()`](https://rdrr.io/r/base/print.html) methods for
  `rsplit` and `val_split` objects were adjusted to show
  `"<Analysis/Assess/Total>"` and `<Training/Validation/Total>`,
  respectively.

- The `drinks`, `attrition`, and `two_class_dat` data sets were removed.
  They are in the modeldata package.

- Compatability with dplyr 1.0.0.

## rsample 0.0.6

CRAN release: 2020-03-31

- Added
  [`validation_set()`](https://rsample.tidymodels.org/dev/reference/validation_set.md)
  for making a single resample.

- Correct the tidy method for bootstraps
  ([\#115](https://github.com/tidymodels/rsample/issues/115)).

- Changes for upcoming \`tibble release.

- Exported constructors for `rset` and `split` objects
  ([\#40](https://github.com/tidymodels/rsample/issues/40))

- [`initial_time_split()`](https://rsample.tidymodels.org/dev/reference/initial_split.md)
  and
  [`rolling_origin()`](https://rsample.tidymodels.org/dev/reference/rolling_origin.md)
  now have a `lag` parameter that ensures that previous data are
  available so that lagged variables can be calculated.
  ([\#135](https://github.com/tidymodels/rsample/issues/135),
  [\#136](https://github.com/tidymodels/rsample/issues/136))

## rsample 0.0.5

CRAN release: 2019-07-12

- Added three functions to compute different bootstrap confidence
  intervals.
- A new function
  ([`add_resample_id()`](https://rsample.tidymodels.org/dev/reference/add_resample_id.md))
  augments a data frame with columns for the resampling identifier.
- Updated
  [`initial_split()`](https://rsample.tidymodels.org/dev/reference/initial_split.md),
  [`mc_cv()`](https://rsample.tidymodels.org/dev/reference/mc_cv.md),
  [`vfold_cv()`](https://rsample.tidymodels.org/dev/reference/vfold_cv.md),
  [`bootstraps()`](https://rsample.tidymodels.org/dev/reference/bootstraps.md),
  and
  [`group_vfold_cv()`](https://rsample.tidymodels.org/dev/reference/group_vfold_cv.md)
  to use tidyselect on the stratification variable.
- Updated
  [`initial_split()`](https://rsample.tidymodels.org/dev/reference/initial_split.md),
  [`mc_cv()`](https://rsample.tidymodels.org/dev/reference/mc_cv.md),
  [`vfold_cv()`](https://rsample.tidymodels.org/dev/reference/vfold_cv.md),
  [`bootstraps()`](https://rsample.tidymodels.org/dev/reference/bootstraps.md)
  with new `breaks` parameter that specifies the number of bins to
  stratify by for a numeric stratification variable.

## rsample 0.0.4

CRAN release: 2019-01-07

Small maintenance release.

### Minor improvements and fixes

- [`fill()`](https://tidyr.tidyverse.org/reference/fill.html) was
  removed per the deprecation warning.
- Small changes were made for the new version of tibble.

## rsample 0.0.3

CRAN release: 2018-11-20

### New features

- Added function
  [`initial_time_split()`](https://rsample.tidymodels.org/dev/reference/initial_split.md)
  for ordered initial sampling appropriate for time series data.

### Minor improvements and fixes

- [`fill()`](https://tidyr.tidyverse.org/reference/fill.html) has been
  renamed
  [`populate()`](https://rsample.tidymodels.org/dev/reference/populate.md)
  to avoid a conflict with
  [`tidyr::fill()`](https://tidyr.tidyverse.org/reference/fill.html).

- Changed the R version requirement to be R \>= 3.1 instead of 3.3.3.

- The recipes-related
  [`prepper()`](https://recipes.tidymodels.org/reference/prepper.html)
  function was [moved to the recipes
  package](https://github.com/tidymodels/rsample/issues/48). This makes
  the rsample install footprint much smaller.

- `rsplit` objects are shown differently inside of a tibble.

- Moved from the broom package to the generics package.

## rsample 0.0.2

CRAN release: 2017-11-12

- `initial_split`, `training`, and `testing` were added to do
  training/testing splits prior to resampling.
- Another resampling method, `group_vfold_cv`, was added.
- `caret2rsample` and `rsample2caret` can convert `rset` objects to
  those used by `caret::trainControl` and vice-versa.
- A function called `form_pred` can be used to determine the original
  names of the predictors in a formula or `terms` object.
- A vignette and a function (`prepper`) were included to facilitate
  using the recipes with rsample.
- A `gather` method was added for `rset` objects.
- A `labels` method was added for `rsplit` objects. This can help
  identify which resample is being used even when the whole `rset`
  object is not available.
- A variety of dplyr methods were added
  (e.g. [`filter()`](https://dplyr.tidyverse.org/reference/filter.html),
  [`mutate()`](https://dplyr.tidyverse.org/reference/mutate.html), etc)
  that work without dropping classes or attributes of the rsample
  objects.

## rsample 0.0.1 (2017-07-08)

CRAN release: 2017-07-08

Initial public version on CRAN
