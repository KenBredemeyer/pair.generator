#' Find Repeated Pairs
#'
#' @param pairs data.frame returned from \code{pairs_make}.
#' @return A data frame containing repeated pairs and their match.
#' @export
duplicates <- function(pairs) {
	dups <- pairs[which(duplicated(pairs[,1:2]) | duplicated(pairs[,1:2], fromLast = TRUE)), ]
	dups <- dups[order(dups[ , 1], dups[ , 2]), ]
	dups
}

#' Find repeated pairs in reverse left/right order
#'
#' @param gp data.frame returned from \code{pairs_make}.
#' @return A data frame containing repeated pairs and their match.
#' @export
reverse_duplicates <- function(gp) {
	revdups_i <- list()
	orig_i <- list()
	for (i in 1:dim(gp)[1]) {
	  revdups_i[[i]] <- which(paste0(gp[ ,1], gp[ ,2]) == paste0(gp[i, 2], gp[i, 1]))
	  if (length(revdups_i[[i]] > 0)) {
	    orig_i[[i]] <- rep(i, length(revdups_i[[i]]))
	  }
	}
	revdups_indicies <- do.call(c, revdups_i)
	orig_indicies <- do.call(c, orig_i)
	x <- data.frame(index = orig_indicies, rev_index = revdups_indicies)
	results <- cbind(orig_indicies, gp[orig_indicies, 1:2], revdups_indicies, gp[revdups_indicies, 1:2])
	if (!is.null(orig_indicies)) {
		colnames(results) <- c("row", "left", "right", "row_rev", "left_rev", "right_rev")
		results
	} else {
		name <- deparse(substitute(gp))
		message(paste0("no reverse duplicates found in ", name))
	}
}

#' Show all duplicates
#'
#' @export
duplicates2 <- function(pairs) {
	stopifnot(is.numeric(pairs$combination) && !any(is.na(pairs$combination)))

	dups_i <- which(duplicated(pairs$combination) | duplicated(pairs$combination, fromLast = TRUE))

	duplicates_df <- cbind(pairs[dups_i, ], pairs_i = dups_i)

	duplicates_df <- duplicates_df[order(duplicates_df$combination), ]
	duplicates_df <- transform(duplicates_df, freq= ave(seq(nrow(duplicates_df)), combination, FUN=length))
  duplicates_df <- duplicates_df[order(-duplicates_df$freq), ]
  duplicates_df
}
