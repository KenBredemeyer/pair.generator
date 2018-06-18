# function ----------------------------------------------------------------
#' simulate judgements of pairwise comparisons
#'
#' @param performances Character vector of media or performance labels.
#' @param pairs data.frame returned from \code{pairs_generate}
#' @param distribution Character string. \code{"uniform"} or \code{"normal"}
#' @param ... args supplied to \code{fun}.  Use \code{min, max} for \code{runif}
#'   or \code{mead, sd} for \code{rnorm}.
#' @export
sim_judge <- function(performances, pairs, distribution = "uniform", ...) {
  stopifnot(any(distribution == c("uniform", "random")))
	x <- data.frame(media = performances)

	if (distribution == "uniform") {
	  x$beta <- runif(dim(x)[1], min, max)
	} else if(distribution == "random") {
		x$beta <- rnorm(length(x), mean, sd)
	}

	judgements <- left_join(pairs, x, by = c("left" = "media"))
	colnames(judgements)[6] <- "left_beta"
	judgements <- left_join(judgements, x, by = c("right" = "media"))
	colnames(judgements)[7] <- "right_beta"

	P <- function(b1, b2) {
		exp(b1 - b2) / (1 + exp(b1 - b2))
	}

	judgements[,8] <- rbinom(dim(judgements)[1], 1, P(judgements[,6], judgements[,7]))
	colnames(judgements)[8] <- "left_wins"

	judgements
}

sim_judge(letters, gp, min = 0, max = 100)
