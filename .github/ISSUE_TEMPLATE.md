If you are filing a bug, make sure these boxes are checked before submitting your issue— thank you!

- [ ] Start a new R session
- [ ] Install the latest version of of the package: `update.packages(oldPkgs="rsample", ask=FALSE)`
- [ ] [Write a minimal reproducible example](https://stackoverflow.com/a/5963610)
- [ ] run `sessionInfo()` and add the results to the issue. Even better would be to use the [`sessioninfo`](https://github.com/r-lib/sessioninfo) package's `session_info()`.  

### Minimal, reproducible example:

__Please read this page__: [reprex = {repr}oducible {ex}ample](https://github.com/jennybc/reprex#what-is-a-reprex) 

Text and example code modified from [the R FAQ on stackoverflow](https://stackoverflow.com/a/5963610)

#### _Minimal_ Reproducible Dataset:

If you are not using a data set in a package then use e.g. `dput()` to give us something that can be put in R immediately, e.g. 

```r
dput(your_data_frame_here)
```

Without a dataset, there usually isn't much that we can do to help. 

If your data frame has a factor with many levels, the `dput` output can be unwieldy because it will still list all the possible factor levels even if they aren't present in the the subset of your data. To solve this issue, you can use the `droplevels()` function. For example, in `data(hpc_data, package = "modeldata")`, the `protocol` variable has many levels. Notice below how `protocol` is a factor with only one level here: `dput(droplevels(head(hpc_data, 4)))`.

#### Minimal, runnable code:

```r
library(rsample)

# Set the seed before using random numbers
set.seed(4121)
cv <- tidy(vfold_cv(mtcars, v = 5)) 
```

### Session Info:

```r
sessionInfo()

# or sessioninfo::session_info()

```

Be sure to test your chunks of code in an empty R session before submitting your issue!
