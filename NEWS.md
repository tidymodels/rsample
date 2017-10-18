# `rsample` 0.0.2

* `initial_split`, `training`, and `testing` were added to do training/testing splits prior to resampling. 
* Another resampling method, `group_vfold_cv`, was added. 
* `caret2rsample` and `rsample2caret` can convert `rset` objects to those used by `caret::trainControl` and vice-versa. 
* A function called `form_pred` can be used to determine the original names of the predictors in a formula or `terms` object. 
* A vignette and a function (`prepper`) were included to facilitate using the `recipes` with `rsample`.
* A `gather` method was added for `rset` objects.

# `rsample` 0.0.1

Initial public version on CRAN

