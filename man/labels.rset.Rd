% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/labels.R
\name{labels.rset}
\alias{labels.rset}
\alias{labels.vfold_cv}
\title{Find Labels from rset Object}
\usage{
\method{labels}{rset}(object, make_factor = FALSE, ...)

\method{labels}{vfold_cv}(object, make_factor = FALSE, ...)
}
\arguments{
\item{object}{An \code{rset} object.}

\item{make_factor}{A logical for whether the results should be
a character or a factor.}

\item{...}{Not currently used.}
}
\value{
A single character or factor vector.
}
\description{
Produce a vector of resampling labels (e.g. "Fold1") from
an \code{rset} object. Currently, \code{\link[=nested_cv]{nested_cv()}} is not supported.
}
\examples{
labels(vfold_cv(mtcars))
}
