#' Combine two sets of pairs into one data frame, randomising the chains
#'
#' If \code{pairs1} and \code{pairs2} have any chain numbers in common,
#' \code{pairs2$chain_number} will be increased, so that chain numbers are
#' unique.
#'
#' @param pairs1 data frame containing the variable \code{chain_number}
#' @param pairs2 data frame containing the variable \code{chain_number}
#'
#' @return Data frame with rows from \code{pairs1} and \code{pairs2} randomised,
#'   with chains intact.
#' @export
combine_pairs <- function(pairs1, pairs2) {
	stopifnot(ncol(pairs1) == ncol(pairs2),
		colnames(pairs1) == colnames(pairs2),
		!is.null(pairs1$chain_number), !is.null(pairs2$chain_number))
	if(length(intersect(pairs1$chain_number, pairs2$chain_number)) != 0) {
		pairs2$chain_number <- as.numeric(pairs2$chain_number) + max(as.numeric(pairs1$chain_number))
	}
	randomised_chains <- sample(c(unique(pairs1$chain_number), unique(pairs2$chain_number)))
	combined_df <- rbind(pairs1, pairs2)

	row_indices <- list()
	for (i in seq_along(randomised_chains)) {
		row_indices[[i]] <- which(combined_df$chain_number == randomised_chains[i])
	}
	row_index <- do.call(c, row_indices)
	combined_df <- combined_df[row_index, ]

	combined_df
}
