
# rsample <a href='https://rsample.tidymodels.org/'><img src='man/figures/logo.png' align="right" height="139" alt="a boot on a green background" /></a>

<!-- badges: start -->

[![R-CMD-check](https://github.com/tidymodels/rsample/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/tidymodels/rsample/actions/workflows/R-CMD-check.yaml)
[![Codecov test
coverage](https://codecov.io/gh/tidymodels/rsample/graph/badge.svg)](https://app.codecov.io/gh/tidymodels/rsample)
[![CRAN_Status_Badge](https://www.r-pkg.org/badges/version/rsample)](https://cran.r-project.org/package=rsample)
[![Downloads](https://cranlogs.r-pkg.org/badges/rsample)](https://cran.r-project.org/package=rsample)
[![lifecycle](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html)
<!-- badges: end -->

## Overview

The rsample package provides functions to create different types of
resamples and corresponding classes for their analysis. The goal is to
have a modular set of methods that can be used for:

- resampling for estimating the sampling distribution of a statistic
- estimating model performance using a holdout set

The scope of rsample is to provide the basic building blocks for
creating and analyzing resamples of a data set, but this package does
not include code for modeling or calculating statistics. The [Working
with Resample
Sets](https://rsample.tidymodels.org/articles/Working_with_rsets.html)
vignette gives a demonstration of how rsample tools can be used when
building models.

Note that resampled data sets created by rsample are directly accessible
in a resampling object but do not contain much overhead in memory. Since
the original data is not modified, R does not make an automatic copy.

For example, creating 50 bootstraps of a data set does not create an
object that is 50-fold larger in memory:

``` r
library(rsample)
library(mlbench)

data(LetterRecognition)
lobstr::obj_size(LetterRecognition)
#> 2,644,640 B

set.seed(35222)
boots <- bootstraps(LetterRecognition, times = 50)
lobstr::obj_size(boots)
#> 6,686,776 B

# Object size per resample
lobstr::obj_size(boots)/nrow(boots)
#> 133,735.5 B

# Fold increase is <<< 50
as.numeric(lobstr::obj_size(boots)/lobstr::obj_size(LetterRecognition))
#> [1] 2.528426
```

<sup>Created on 2022-02-28 by the [reprex
package](https://reprex.tidyverse.org) (v2.0.1)</sup>

The memory usage for 50 bootstrap samples is less than 3-fold more than
the original data set.

## Installation

To install it, use:

``` r
install.packages("rsample")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("pak")
pak::pak("rsample")
```

## Contributing

This project is released with a [Contributor Code of
Conduct](https://contributor-covenant.org/version/2/1/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.

- For questions and discussions about tidymodels packages, modeling, and
  machine learning, please [post on Posit
  Community](https://forum.posit.co/new-topic?category_id=15&tags=tidymodels,question).

- If you think you have encountered a bug, please [submit an
  issue](https://github.com/tidymodels/rsample/issues).

- Either way, learn how to create and share a
  [reprex](https://reprex.tidyverse.org/articles/articles/learn-reprex.html)
  (a minimal, reproducible example), to clearly communicate about your
  code.

- Check out further details on [contributing guidelines for tidymodels
  packages](https://www.tidymodels.org/contribute/) and [how to get
  help](https://www.tidymodels.org/help/).
