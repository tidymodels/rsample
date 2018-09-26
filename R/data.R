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
#' @name attrition
#' @aliases attrition
#' @docType data
#' @return \item{attrition}{a data frame}
#'
#' @source The IBM Watson Analytics Lab website https://www.ibm.com/communities/analytics/watson-analytics-blog/hr-employee-attrition/
#'
#'
#' @keywords datasets
#' @examples
#' data(attrition)
#' str(attrition)
NULL


#' Two Class Data
#'
#' @details There are artifical data with two predictors (`A` and `B`) and
#'  a factor outcome variable (`Class`).
#'
#' @name two_class_dat
#' @aliases two_class_dat
#' @docType data
#' @return \item{two_class_dat}{a data frame}
#'
#' @keywords datasets
#' @examples
#' data(two_class_dat)
#' str(two_class_dat)
NULL

#' Sample Time Series Data
#'
#' @details Drink sales. The exact name of the series from FRED is:
#' "Merchant Wholesalers, Except Manufacturers' Sales Branches and Offices
#' Sales: Nondurable Goods: Beer, Wine, and Distilled Alcoholic Beverages Sales"
#'
#' @name drinks
#' @aliases drinks
#' @docType data
#' @return \item{drinks}{a data frame}
#'
#' @source The Federal Reserve Bank of St. Louis website https://fred.stlouisfed.org/series/S4248SM144NCEN
#'
#' @keywords datasets
#' @examples
#' data(drinks)
#' str(drinks)
NULL
