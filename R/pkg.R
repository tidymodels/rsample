#' rsample: General Resampling Infrastructure for R
#'
#'\pkg{rsample} has functions to create variations of a data set
#'  that can be used to evaluate models or to estimate the
#'  sampling distribution of some statistic.
#'
#' @section Terminology:
#'\itemize{
#'  \item A **resample** is the result of a two-way split of a
#'  data set. For example, when bootstrapping, one part of the
#'  resample is a sample with replacement of the original data.
#'  The other part of the split contains the instances that were
#'  not contained in the bootstrap sample. The data structure
#'  `rsplit` is used to store a single resample. 
#'  \item When the data are split in two, the portion that is
#'  used to estimate the model or calculate the statistic is
#'  called the **analysis** set here. In machine learning this
#'  is sometimes called the "training set" but this would be
#'  poorly named since it might conflict with any initial split
#'  of the original data.
#'  \item Conversely, the other data in the split are called the
#'     **assessment** data. In bootstrapping, these data are
#'     often called the "out-of-bag" samples. 
#'  \item A collection of resamples is contained in an 
#'  `rset` object.
#'}
#'
#' @section Basic Functions:
#' The main resampling functions are: [vfold_cv()], 
#'   [bootstraps()], [mc_cv()], 
#'   [rolling_origin()], and [nested_cv()].
#' @docType package
#' @name rsample
NULL
