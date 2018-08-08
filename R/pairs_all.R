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
#' @param n_judges Integer value at least 1. Use this to make import file for
#'   multiple judges.  Use reps value at least equal to the number of judges.
#'
#' @return A data frame containing columns "left", "right" and "judge". If
#'   you're importing into Pairwise Comparisons Application, change "judge" to
#'   "chain_number".
#'
#' @examples
#'   all_pairs(letters)
#'
#' @export
pairs_all <- function(media, n_judges = 1, separation_constraint = NULL) {
	if (!is.null(dim(media))) {
		if(!is.null(media$media)) {
		  scripts <- media$media
		}
	} else {
		scripts <- media
	}
	reps <- as.integer(n_judges)
	if(reps < 1)
		stop("second argument must be at least 1")
	combinations <- t(combn(scripts, 2))
	if (is.numeric(separation_constraint)) {
		stopifnot(!is.null(media$score), !any(is.na(media$score)), is.numeric(media$score))
  	combinations_scores <- data.frame(t(combn(media$score, 2)), stringsAsFactors = FALSE)
  	available_comparisons_i <- which(abs(combinations_scores[,1] - combinations_scores[,2]) <= separation_constraint)
  	combinations <- combinations[available_comparisons_i, ]
	}
  pairs <- list()
		for (i in 1:reps) {
			pairs[[i]] <- combinations
		}
	count <- dim(combinations)[1]
	pairs <- do.call(rbind, pairs)
	df_out <- cbind(pairs, rep(1:reps, each = count))
	colnames(df_out) <- c("left", "right", "judge")
	as.data.frame(df_out, stringsAsFactors = FALSE)
}
