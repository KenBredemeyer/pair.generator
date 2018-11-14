#' Generate exhaustive pairs.
#'
#' \code{all_pairs} generates all unique pairs for pairwise comparisons.
#'
#' Exhaustive pairs (each media item, or performance, paired with all others)
#' can be created for  any number of judges.  Each judge will view all
#' pairs. Allocate n.(n-1)/2 pairs to each judge, where n is the number of
#' performances.
#'
#' @param media Character vector of performances to compare, or data frame
#'   containting the variables \code{media, core, score}.
#'
#' @param n_judges Integer value at least 1. Use this to make import file for
#'   multiple judges.  Use reps value at least equal to the number of judges.
#'
#' @param separation_constraint Numeric. Maximum absolute difference in scores
#'   for performances in a pair.  If a separation constraint is specified, which
#'   is less than the maximum absolute difference between any two performances,
#'   then the resulting design will not be exhaustive.
#'
#' @return A data frame containing columns "left", "right" and "judge". If
#'   you're importing into Pairwise Comparisons Application, change "judge" to
#'   "chain_number".
#'
#' @examples
#'   pairs_all(letters[1:4])
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
	combinations <- t(utils::combn(scripts, 2))
	combinations <- cbind(combinations, 1:dim(combinations)[1])
	if (is.numeric(separation_constraint)) {
		stopifnot(!is.null(media$score), !any(is.na(media$score)), is.numeric(media$score))
  	combinations_scores <- data.frame(t(utils::combn(media$score, 2)), stringsAsFactors = FALSE)
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
	colnames(df_out) <- c("left", "right", "combination", "judge")
	as.data.frame(df_out, stringsAsFactors = FALSE)
}


# only call from pairs_generate
#
# combinations is one set of exhaustive pairs
#
# head_order is order of pairs from pairs_generate_.  Purpose is to allow
# allocations without duplicates for any judge.
# @param media Character vector representing the names of the performances, or
#   a data.frame containing the variables \code{media, core, score}.
# @param n_judges Integer specifying the number of sets of exhaustive pairs to
#   generate.
# @param separation_constraint Numeric.  Maximum absolute score difference
#   between performances in a pair.
# @param head_order Numeric vector specifying the order of generated pairs.
# @param chain_length Integer. The number of consecutive pairs containing a
#   common performance.
# @export
exhaustive_pairs <- function(media, n_judges = 1, separation_constraint = NULL,
	                           head_order = NULL, chain_length = 1) {
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
	combinations <- data.frame(t(utils::combn(scripts, 2)))
	# add 'combination' variable ([[3]])
  combinations <- cbind(combinations, 1:dim(combinations)[1])
	if (is.numeric(separation_constraint)) {
		stopifnot(!is.null(media$score), !any(is.na(media$score)), is.numeric(media$score))
  	combinations_scores <- data.frame(t(utils::combn(media$score, 2)), stringsAsFactors = FALSE)
  	available_comparisons_i <- which(abs(combinations_scores[,1] - combinations_scores[,2]) <= separation_constraint)
  	combinations <- combinations[available_comparisons_i, ]
	}
	if (!is.null(head_order)) {
		head <- length(head_order)
		n_combin <- dim(combinations)[1]
		combinations_ <- combinations
		combinations_[1:head, ] <- combinations[head_order, ]
		combinations_[(head+1):n_combin, ] <- chain(combinations[-head_order, ], chain_length = chain_length)[ , 1:3]
		# add comninations_[["chain_number]]
		combinations_[(head+1):n_combin, 4] <- chain(combinations[-head_order, ], chain_length = chain_length)[ , "chain_number"]
	} else {
		combinations_ <- chain(combinations, chain_length = chain_length)
	}
  pairs <- list()
		for (i in 1:reps) {
			pairs[[i]] <- combinations_
		}
	count <- dim(combinations)[1]
	pairs <- do.call(rbind, pairs)
	#df_out <- cbind(pairs, rep(1:reps, each = count))
	colnames(pairs) <- c("left", "right", "combination", "chain_number")
	as.data.frame(pairs, stringsAsFactors = FALSE)
}
