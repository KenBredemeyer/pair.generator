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

#' Frequency plot of pairs
#'
#' @param pairs data.frame returned from \code{pairs_make}.
#' @export
plot_pairs <- function(pairs) {
	## plot pairs to detect duplicates
	# recover script labels
	scripts <- unique(unlist(pairs[,1:2]))
	scripts_ordered <- scripts[order(scripts)]

	# all possible pairs of combinations
	combinations <- t(combn(scripts_ordered, 2))

	# add 'combination' column to data to test for repetitions of pairs & distribution of pairs
	z <- vector("numeric", nrow(pairs))
	for (i in 1:nrow(pairs)) {
	  z[i] <- which((combinations[, 1] == pairs[i, 1] & combinations[ , 2] == pairs[i, 2]) |
	                 (combinations[, 2] == pairs[i, 1] & combinations[ , 1] == pairs[i, 2]) )
	}

	pairs <- cbind(pairs, z)

	# plot inclusion of pairs (`gpairs` = graph pairs)
	rle(sort(z)) -> gpairs
	plot(gpairs[[2]], gpairs[[1]],
	  main = "Inclusions of Pairs", xlab = "Unique Pair Combination", ylab = "Frequency",
	  sub = paste(nrow(pairs), "Pairs, ", nrow(combinations), "Combinations"))
}


