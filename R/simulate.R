#' Simulate judgements of pairwise comparisons
#'
#' \code{sim_judge} is intended for investigating model properties at the
#' estimation stage, for research purposes.
#'
#' @param performances Character vector of media or performance labels.
#' @param pairs data.frame returned from \code{pairs_generate}.
#' @param distribution Character string. \code{"uniform"} or \code{"normal"}.
#' @param min Numeric. Minimum value for the uniform distribution.
#' @param max Numeric. Maximum value for the uniform distribution.
#' @param mean Numeric.  Mean of the normal distribution.
#' @param sd Numeric.  Standard deviation for the normal distribution. or
#'   \code{mead, sd} for \code{rnorm}.
#' @export
sim_judge <- function(performances, pairs, distribution = "uniform", min, max, mean, sd) {
  stopifnot(any(distribution == c("uniform", "random")))
	x <- data.frame(media = performances)
  nrows_x <- dim(x)[1]

	if (distribution == "uniform") {
	  x$beta <- stats::runif(nrows_x, min, max)
	} else if(distribution == "random") {
		x$beta <- stats::rnorm(nrows_x, mean, sd)
	}

	judgements <- dplyr::left_join(pairs, x, by = c("left" = "media"))
	colnames(judgements)[6] <- "left_beta"
	judgements <- dplyr::left_join(judgements, x, by = c("right" = "media"))
	colnames(judgements)[7] <- "right_beta"

	P <- function(b1, b2) {
		exp(b1 - b2) / (1 + exp(b1 - b2))
	}

	judgements[,8] <- stats::rbinom(dim(judgements)[1], 1, P(judgements[,6], judgements[,7]))
	colnames(judgements)[8] <- "left_wins"

	judgements
}

