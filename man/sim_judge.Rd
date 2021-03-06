% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/simulate.R
\name{sim_judge}
\alias{sim_judge}
\title{Simulate judgements of pairwise comparisons}
\usage{
sim_judge(performances, pairs, distribution = "uniform", min, max, mean, sd)
}
\arguments{
\item{performances}{Character vector of media or performance labels.}

\item{pairs}{data.frame returned from \code{pairs_generate}.}

\item{distribution}{Character string. \code{"uniform"} or \code{"normal"}.}

\item{min}{Numeric. Minimum value for the uniform distribution.}

\item{max}{Numeric. Maximum value for the uniform distribution.}

\item{mean}{Numeric.  Mean of the normal distribution.}

\item{sd}{Numeric.  Standard deviation for the normal distribution. or
\code{mead, sd} for \code{rnorm}.}
}
\description{
\code{sim_judge} is intended for investigating model properties at the
estimation stage, for research purposes.
}
