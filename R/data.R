#' Job Attrition
#'
#' @details These data are from the IBM Watson Analytics Lab.
#'  The website describes the data with \dQuote{Uncover the
#'  factors that lead to employee attrition and explore important
#'  questions such as \sQuote{show me a breakdown of distance
#'  from home by job role and attrition} or \sQuote{compare
#'  average monthly income by education and attrition}. This is a
#'  fictional data set created by IBM data scientists.}. There
#'  are 1470 rows.
#'
#'  Several of the variables (\code{Education},
#'  \code{EnvironmentSatisfaction}, \code{JobInvolvement},
#'  \code{JobSatisfaction}, \code{PerformanceRating},
#'  \code{RelationshipSatisfaction}, and \code{WorkLifeBalance})
#'  are integer scores for ordinal values. These data have been
#'  kept in the original format. Consult the website listed
#'  below to obtain the translation between the integers and
#'  the values that they represent (e.g. low, medium, high, etc.).
#'
#' @name attrition
#' @aliases attrition
#' @docType data
#' @return \item{attrition}{a data frame}
#'
#' @source The IBM Watson Analytics Lab website \url{https://www.ibm.com/communities/analytics/watson-analytics-blog/hr-employee-attrition/}
#'
#'
#' @keywords datasets
#' @examples
#' data(attrition)
#' str(attrition)
NULL