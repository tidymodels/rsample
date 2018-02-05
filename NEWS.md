# `rsample` 0.0.2.9000

* Changed the R version requirement to be R >= 3.1 instead of 3.3.3. 


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

