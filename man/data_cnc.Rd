% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_examples.R
\docType{data}
\name{data_cnc}
\alias{data_cnc}
\title{Example data for core versus non-core pair generation}
\format{
A data frame with 3 variables. Column headers are \code{media, core, score}
\describe{
  \item{media}{The label of the performance or media
  item.}
  \item{core}{1 for core, 0 for non-core.  Core is for making a new scale,
  non-core is for locating a performance on an existing scale.}
  \item{score}{Examples of student scores out of 100.}
  }
}
\usage{
data(data_cnc)
}
\description{
Use with \code{pairs_generate_cnc}.
Used in projects where non-core items are to be located on a predetermined
scale of the core items.
}
\keyword{datasets}
