#' rsample: General Resampling Infrastructure for R
#'
#'\pkg{rsample} has functions to create variations of a data set
#'  that can be used to evaluate models or to estimate the
#'  sampling distribution of some statistic.
#'
#' @section Terminology:
#'\itemize{
#'  \item A \bold{resample} is a result of a two-way split of a
#'  data set. For example, when bootstrapping, one part of the
#'  resample is a sample with replacement of the original data.
#'  The other part of the split contains the instances that were
#'  not contained in the bootstrap sample. The data structure
#'  \code{rsplit} is used to store a single resample. 
#'  \item When the data are split in two, the portion that are
#'  used to estimate the model or calculate the statistic is
#'  called the \bold{analysis} set here. In machine learning this
#'  is sometimes called the "training set" but this would be
#'  poorly named since it might conflict with any initial split
#'  of the original data.
#'  \item Conversely, the other data in the split are called the
#'     \bold{assessment} data. In bootstrapping, these data are
#'     often called the "out-of-bag" samples. 
#'  \item A collection of resamples is contained in an 
#'  \code{rset} object.
#'}
#'
#' @section Basic Functions:
#' The main resampling functions are: \code{\link{vfold_cv}}, 
#'   \code{\link{bootstraps}}, \code{\link{mc_cv}}, 
#'   \code{\link{rolling_origin}}, and \code{\link{nested_cv}}.
#' @docType package
#' @name rsample
NULL
