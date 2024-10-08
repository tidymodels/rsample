% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fingerprint.R
\name{.get_fingerprint}
\alias{.get_fingerprint}
\alias{.get_fingerprint.default}
\alias{.get_fingerprint.rset}
\title{Obtain a identifier for the resamples}
\usage{
.get_fingerprint(x, ...)

\method{.get_fingerprint}{default}(x, ...)

\method{.get_fingerprint}{rset}(x, ...)
}
\arguments{
\item{x}{An \code{rset} or \code{tune_results} object.}

\item{...}{Not currently used.}
}
\value{
A character value or \code{NA_character_} if the object was created prior
to rsample version 0.1.0.
}
\description{
This function returns a hash (or NA) for an attribute that is created when
the \code{rset} was initially constructed. This can be used to compare with other
resampling objects to see if they are the same.
}
\examples{
set.seed(1)
.get_fingerprint(vfold_cv(mtcars))

set.seed(1)
.get_fingerprint(vfold_cv(mtcars))

set.seed(2)
.get_fingerprint(vfold_cv(mtcars))

set.seed(1)
.get_fingerprint(vfold_cv(mtcars, repeats = 2))
}
