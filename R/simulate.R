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
  stopifnot(any(distribution == c("uniform", "normal")))
	x <- data.frame(media = performances)
  nrows_x <- dim(x)[1]

	if (distribution == "uniform") {
	  x$beta <- stats::runif(nrows_x, min, max)
	} else if(distribution == "normal") {
		x$beta <- stats::rnorm(nrows_x, mean, sd)
	}

	judgements <- dplyr::left_join(pairs, x, by = c("left" = "media"))
	colnames(judgements)[dim(judgements)[2]] <- "left_beta"
	judgements <- dplyr::left_join(judgements, x, by = c("right" = "media"))
	colnames(judgements)[dim(judgements)[2]] <- "right_beta"

	P <- function(b1, b2) {
		exp(b1 - b2) / (1 + exp(b1 - b2))
	}

	left_wins <- stats::rbinom(dim(judgements)[1], 1,
	                           P(judgements[,"left_beta"], judgements[,"right_beta"]))
	Selected <- vector("character", length = length(left_wins))
	for (i in 1:length(left_wins)) {
		if (left_wins[i]) {
			Selected[i] <- judgements$left[i]
		} else if (!left_wins[i]) {
			Selected[i] <- judgements$right[i]
		}
	}

	sim_judgements <- cbind(judgements, Selected)
	list(sim_judgements, sim_betas = x)
}


#' Simulate judgements of pairwise comparisons
#'
#' \code{sim_judge} is intended for investigating model properties at the
#' estimation stage, for research purposes.
#'
#' @param performances Character vector of media or performance labels.
#' @param pairs data.frame returned from \code{pairs_generate}.
#' @param distribution Character string. Person distribution in
#'   logits\code{"uniform"} or \code{"normal"}.
#' @param min Numeric. Minimum value for the uniform distribution.
#' @param max Numeric. Maximum value for the uniform distribution.
#' @param mean Numeric.  Mean of the normal distribution.
#' @param sd Numeric.  Standard deviation for the normal distribution. or
#'   \code{mead, sd} for \code{rnorm}.
#' @export
sim_judge_LMF <- function(performances, pairs, alpha=1, distribution = "uniform", min, max, mean, sd) {
  stopifnot(any(distribution == c("uniform", "normal")))
	x <- data.frame(media = performances)
  nrows_x <- dim(x)[1]

	if (distribution == "uniform") {
	  x$beta <- stats::runif(nrows_x, min, max)
	} else if(distribution == "normal") {
		x$beta <- stats::rnorm(nrows_x, mean, sd)
	}

	judgements <- dplyr::left_join(pairs, x, by = c("left" = "media"))
	colnames(judgements)[dim(judgements)[2]] <- "left_beta"
	judgements <- dplyr::left_join(judgements, x, by = c("right" = "media"))
	colnames(judgements)[dim(judgements)[2]] <- "right_beta"

	P <- function(b1, b2) {
		exp(alpha*(b1 - b2)) / (1 + exp(alpha*(b1 - b2)))
	}

	left_wins <- stats::rbinom(dim(judgements)[1], 1,
	                           P(judgements[,"left_beta"], judgements[,"right_beta"]))
	Selected <- vector("character", length = length(left_wins))
	for (i in 1:length(left_wins)) {
		if (left_wins[i]) {
			Selected[i] <- judgements$left[i]
		} else if (!left_wins[i]) {
			Selected[i] <- judgements$right[i]
		}
	}

	sim_judgements <- cbind(judgements, Selected)
	list(sim_judgements, sim_betas = x)
}
