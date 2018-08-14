#' barchart to see how well each script is represented.
#'
#' @param x A data frame containing pairs of performances.
#' @export
pairs_plot_inclusions <- function(pairs) {
	sampling <- rle(sort(unlist(pairs[ , 1:2])))
	s.f. <- sampling[[1]]
	names(s.f.) <- sampling[[2]]
	uylim <- max(s.f.)

	barplot(s.f., ylim = c(0, uylim), main = "Inclusions of Performances",
	        sub = paste("number of performances =", length(unique(unlist(pairs[ ,1:2]))),
	        	           ", number of pairs = ", nrow(pairs), sep = ""),
		      xlab = "", ylab = "inclusions")
	abline(h = attributes(pairs)$user_inputs$min_c, lty = 2, col = "orange")
	abline(h = attributes(pairs)$user_inputs$max_c, lty = 2, col = "orange")
}

#' Range of inclusions per media/performance
#' @param pairs data.frame returned from \code{pairs_generate}
#' @export
pairs_inclusions_range <- function(pairs) {
	sampling <- rle(sort(unlist(pairs[ , 1:2])))
	s.f. <- sampling[[1]]
	names(s.f.) <- sampling[[2]]
	range(s.f.)
}

#' Plot the frequency of pairs
#'
#' @param pairs data.frame returned from \code{pairs_generate} or similar
#'   function.
#' @export
pairs_plot <- function(pairs) {
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
	z
	pairs <- cbind(pairs, z)

	# plot inclusion of pairs
	unique_pairs <- rle(sort(z))
	plot(unique_pairs[[2]], unique_pairs[[1]], pch = 20,
	  main = "Inclusions of Pairs", xlab = "Unique Pair Combination", ylab = "Frequency",
	  sub = paste(nrow(pairs), "Pairs, ", nrow(combinations), "Combinations"),
		font.sub = 3,
		yaxt="n", ylim = c(min(unique_pairs[[1]])-1, max(unique_pairs[[1]])))
	axis(side = 2, at = 0:max(unique_pairs[[1]]) )
}
