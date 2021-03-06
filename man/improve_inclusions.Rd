% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/swap3.R
\name{improve_inclusions}
\alias{improve_inclusions}
\title{Modify a data frame of pairs to improve the range of inclusions of
performances}
\usage{
improve_inclusions(
  gp,
  combinations,
  av_inclusions,
  inclusion_tolerance,
  animate = FALSE,
  max_iterations = 600,
  allow_repeats = FALSE
)
}
\arguments{
\item{gp}{Data frame of pairs, such as that returned from
\code{pairs_sample}.}

\item{combinations}{Data frame containing \code{left} veriable in column 1
and \code{right} variable in column 2.  All possible pairs of performances.
\code{combinations} can be created with \code{pairs_all}.}

\item{av_inclusions}{Integer. Average number of times performances are to be
included in the set of generated pairs.}

\item{inclusion_tolerance}{Integer.  Difference between the maximum or
minim number of times each performnace is included and the average
inclusions.}

\item{animate}{Logical.  Should plots showing the current number of
inclusions for each performance be displayed.}

\item{max_iterations}{Integer.  Maximum number of loops executed.}

\item{allow_repeats}{Logical.  Should duplicate pairs be generated.}
}
\value{
A data frame in the same form as \code{gp}, its first argument, but
  modified to meet specified inclusions of performances.  Messages
  indicating the iteration number, and the range of inclusions at the
  corresponding iteration are output to the console.  An inclusion plot
  updated at every iteration is optionally displayed with the \code{animate}
  argument.
}
\description{
\code{improve_inclusions} swaps pairs between the set of generated pairs
(\code{gp}) in the first argument with pairs in the set of all pairs
(\code{combinations}) in the second argument.  It does this in an iterative
fashion, trying to conform to the specified inclusion range.  Use this
function if \code{pairs_generate} is not able to return a data frame, or is
taking excessively long.
}
\examples{
set.seed(1) # for reproducability
sampled_pairs <- pairs_sample(letters, av_inclusions = 10)
pairs <- improve_inclusions(sampled_pairs,
                            combinations = pairs_all(letters)[ , 1:3],
                            av_inclusions = 10,
                            inclusion_tolerance = 1)
pairs_inclusion_range(pairs)

}
\seealso{
\code{\link{pairs_sample}}, \code{\link{chain}},
  \code{\link{switch_lr}}
}
