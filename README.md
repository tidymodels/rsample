

# rsample

[![Travis-CI Build Status](https://travis-ci.org/tidymodels/rsample.svg?branch=master)](https://travis-ci.org/tidymodels/rsample)
[![R build status](https://github.com/tidymodels/rsample/workflows/R-CMD-check/badge.svg)](https://github.com/tidymodels/rsample/actions)
[![Coverage Status](https://img.shields.io/codecov/c/github/tidymodels/rsample/master.svg)](https://codecov.io/github/tidymodels/rsample?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/rsample)](https://cran.r-project.org/package=rsample)
[![Downloads](http://cranlogs.r-pkg.org/badges/rsample)](https://cran.r-project.org/package=rsample)
![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)

`rsample` contains a set of functions that can create different types of resamples and corresponding classes for their analysis. 
The goal is to have a modular set of methods that can be used across different R packages for:
 
 * traditional resampling techniques for estimating the sampling distribution of a statistic and
 * estimating model performance using a holdout set
 
The scope of `rsample` is to provide the basic building blocks for creating and analyzing resamples of a data set but does not include code for modeling or calculating statistics. The "Working with Resample Sets" vignette gives demonstrations of how `rsample` tools can be used.  

Note that resampled data sets created by `rsample` are directly accessible in a resampling object but do not contain much overhead in memory. Since the original data is not modified, R does not make an automatic copy. 
 
For example, creating 50 bootstraps of a data set does not create an object that is 50-fold larger in memory:

```r
library(rsample)
#> Loading required package: tidyr
library(mlbench)

data(LetterRecognition)
lobstr::obj_size(LetterRecognition)
#> 2,644,640 B

set.seed(35222)
boots <- bootstraps(LetterRecognition, times = 50)
lobstr::obj_size(boots)
#> 6,686,512 B

# Object size per resample
object_size(boots)/nrow(boots)
#> Error in object_size(boots): could not find function "object_size"

# Fold increase is <<< 50
as.numeric(lobstr::obj_size(boots)/lobstr::obj_size(LetterRecognition))
#> [1] 2.528326
```

<sup>Created on 2020-03-30 by the [reprex package](https://reprex.tidyverse.org) (v0.3.0)</sup>

The memory usage for 50 boostrap samples is less than 3-fold more than the original data set. 

## Installation

To install it, use:


```r
install.packages("rsample")

## For the devel version:
require(devtools)
install_github("tidymodels/rsample")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
install_dev("rsample")
```

## Contributing

We welcome contributions of all types!

If you have never made a pull request to an R package before, `rsample` is an excellent place to start. Find an [issue](https://github.com/tidymodels/rsample/issues/) with the **Beginner Friendly** tag and comment that you'd like to take it on and we'll help you get started.

We encourage typo corrections, bug reports, bug fixes and feature requests. Feedback on the clarity of the documentation is especially valuable.


## Code of Conduct

Please note that the rsample project is released with a [Contributor Code of Conduct](https://tidymodels.github.io/rsample/CODE_OF_CONDUCT.html). By contributing to this project, you agree to abide by its terms.

