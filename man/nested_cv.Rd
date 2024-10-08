% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/nested_cv.R
\name{nested_cv}
\alias{nested_cv}
\title{Nested or Double Resampling}
\usage{
nested_cv(data, outside, inside)
}
\arguments{
\item{data}{A data frame.}

\item{outside}{The initial resampling specification. This can be an already
created object or an expression of a new object (see the examples below).
If the latter is used, the \code{data} argument does not need to be
specified and, if it is given, will be ignored.}

\item{inside}{An expression for the type of resampling to be conducted
within the initial procedure.}
}
\value{
An tibble with \code{nested_cv} class and any other classes that
outer resampling process normally contains. The results include a
column for the outer data split objects, one or more \code{id} columns,
and a column of nested tibbles called \code{inner_resamples} with the
additional resamples.
}
\description{
\code{nested_cv()} can be used to take the results of one resampling procedure
and conduct further resamples within each split. Any type of resampling
used in rsample can be used.
}
\details{
It is a bad idea to use bootstrapping as the outer resampling procedure (see
the example below)
}
\examples{
## Using expressions for the resampling procedures:
nested_cv(mtcars, outside = vfold_cv(v = 3), inside = bootstraps(times = 5))

## Using an existing object:
folds <- vfold_cv(mtcars)
nested_cv(mtcars, folds, inside = bootstraps(times = 5))

## The dangers of outer bootstraps:
set.seed(2222)
bad_idea <- nested_cv(mtcars,
  outside = bootstraps(times = 5),
  inside = vfold_cv(v = 3)
)

first_outer_split <- get_rsplit(bad_idea, 1)
outer_analysis <- analysis(first_outer_split)
sum(grepl("Camaro Z28", rownames(outer_analysis)))

## For the 3-fold CV used inside of each bootstrap, how are the replicated
## `Camaro Z28` data partitioned?
first_inner_split <- get_rsplit(bad_idea$inner_resamples[[1]], 1)
inner_analysis <- analysis(first_inner_split)
inner_assess <- assessment(first_inner_split)

sum(grepl("Camaro Z28", rownames(inner_analysis)))
sum(grepl("Camaro Z28", rownames(inner_assess)))
}
