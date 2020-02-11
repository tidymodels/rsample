---
output: github_document
---



# rsample <img src="man/figures/logo.png" align="right" height="80px"/> 

[![Travis-CI Build Status](https://travis-ci.org/tidymodels/rsample.svg?branch=master)](https://travis-ci.org/tidymodels/rsample)
[![Coverage Status](https://img.shields.io/codecov/c/github/tidymodels/rsample/master.svg)](https://codecov.io/github/tidymodels/rsample?branch=master)
[![CRAN_Status_Badge](http://www.r-pkg.org/badges/version/rsample)](https://cran.r-project.org/package=rsample)
[![Downloads](http://cranlogs.r-pkg.org/badges/rsample)](https://cran.r-project.org/package=rsample)
![](https://img.shields.io/badge/lifecycle-maturing-blue.svg)

`rsample` contains a set of functions that can create different types of resamples and corresponding classes for their analysis. 
The goal is to have a modular set of methods that can be used across different R packages for:
 
 * traditional resampling techniques for estimating the sampling distribution of a statistic and
 * estimating model performance using a holdout set
 
The scope of `rsample` is to provide the basic building blocks for creating and analyzing resamples of a data set but does not include code for modeling or calculating statistics. The "Working with Resample Sets" vignette gives demonstrations of how `rsample` tools can be used.  

To install it, use:

```r
install.packages("rsample")

## For the devel version:
require(devtools)
install_github("tidymodels/rsample")
```

Note that resampled data sets created by `rsample` are directly accessible in a resampling object but do not contain much overhead in memory. Since the original data is not modified, R does not make an automatic copy. 
 
For example, creating 50 bootstraps of a data set does not create an object that is 50-fold larger in memory:

```r
> library(rsample)
> library(mlbench)
> library(pryr)
> 
> data(LetterRecognition)
> 
> object_size(LetterRecognition)
2.64 MB
> 
> set.seed(35222)
> boots <- bootstraps(LetterRecognition, times = 50)
> 
> object_size(boots)
6.69 MB
> 
> # Object size per resample
> object_size(boots)/nrow(boots)
134 kB
> 
> # Fold increase is <<< 50
> as.numeric(object_size(boots)/object_size(LetterRecognition))
[1] 2.528695
```

The memory usage for 50 boostrap samples is less than 3-fold more than the original data set. 

