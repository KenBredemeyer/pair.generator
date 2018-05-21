#' Generate exhaustive pairs.
#'
#' \code{all_pairs} generates all unique pairs for pairwise comparisons.
#'
#' Exhaustive pairs (each media item paired with all others) can be created for
#' using any number of judges.  Each judge will view all pairs. Allocate
#' n*(n-1)/2 pairs to each judge.
#'
#' @param scripts Character vector of performances to compare.
#'
#' @param reps Integer value at least 1. Use this to make import file for
#'   multiple judges.  Use reps value at least equal to the number of judges.
#'
#' @return A data frame containing columns "left", "right" and "chain_number".
#'   "chain_number" can be read as judge number for the purposes of import into
#'   Pairwise Comparisons Application.
#'
#' @examples
#'   all_pairs(letters)
#'
#' @export
pairs_all <- function(scripts, reps = 1) {
	reps <- as.integer(reps)
	if(reps < 1)
		stop("second argument must be at least 1")
  pairs <- list()
		for (i in 1:reps) {
			pairs[[i]] <- t(combn(scripts, 2))
		}
	count <- choose(length(scripts), 2)
	pairs <- do.call(rbind, pairs)
	df_out <- cbind(pairs, rep(1:reps, each = count))
	colnames(df_out) <- c("left", "right", "chain_number")
	as.data.frame(df_out)
}
