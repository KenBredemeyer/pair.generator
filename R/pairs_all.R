# Generate all unique pairs
# allow for multiple judges
#
#         Ken Bredemeyer 18/05/2017
#
# `scripts` is a character vector of media labels.
# `reps` is the number of judges.
# "chain_number" represents judge number (i.e. "chain_number" = 1 for judge 1).


#' Generate exhaustive pairs.
#'
#' \code{pairs_all} generates all unique pairs for pairwise comparisons.
#'
#' Exhaustie pairs can be created for import into Pairwise Comparisons
#' Application using any number of judges.  Each judge will view all pairs.
#'
#' @param scripts Character vector of performances to compare.
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
	df_out
}
