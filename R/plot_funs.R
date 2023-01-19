#' Barchart to see how well each performance is represented
#'
#' To test if \code{pairs_generate} produces pairs which conform to the
#' specified range of inclusions of performances, use
#' \code{pairs_plot_inclusions} together with \code{abline} (see the example
#' below), or use \code{\link{pairs_inclusion_range}}.
#'
#' @param pairs A data frame containing pairs of performances.
#' @examples
#' pairs <- pairs_generate(letters[1:10],
#'                          av_inclusions = 10,
#'                          inclusion_tolerance = 2,
#'                          chain_length = 1)
#' pairs_plot_inclusions(pairs)
#' abline(h = c(10-2, 10+2))
#' @export
pairs_plot_inclusions <- function(pairs) {
	sampling <- rle(sort(unlist(pairs[ , 1:2])))
	s.f. <- sampling[[1]]
	names(s.f.) <- sampling[[2]]
	uylim <- max(s.f.)

	graphics::barplot(s.f., ylim = c(0, uylim), main = "Inclusions of Performances",
	        sub = paste("number of performances =", length(unique(unlist(pairs[ ,1:2]))),
	        	           ", number of pairs = ", nrow(pairs), sep = ""),
		      xlab = "", ylab = "inclusions")
	graphics::abline(h = attributes(pairs)$user_inputs$min_c, lty = 2, col = "orange")
	graphics::abline(h = attributes(pairs)$user_inputs$max_c, lty = 2, col = "orange")
}

#' Range of inclusions per media/performance
#' @param pairs \code{data.frame} returned from \code{pairs_generate},
#'   \code{pairs_generate_cnc}, or similar function.
#'
#' @return Numeric vector length 2, reporting the minimum and maximum number of
#'   inclusions per performance in the set of pairs used as its first argument.
#'
#' @examples
#' pairs <- pairs_generate(letters[1:3],
#'                         av_inclusions = 2,
#'	                       inclusion_tolerance = 1,
#'	                       chain_length = 1)
#' pairs_inclusion_range(pairs)
#' @export
pairs_inclusion_range <- function(pairs) {
	sampling <- rle(sort(unlist(pairs[ , 1:2])))
	s.f. <- sampling[[1]]
	names(s.f.) <- sampling[[2]]
	range(s.f.)
}

#' Plot the frequency of unique pairs included in \code{pairs}
#'
#' @param pairs A data frame returned from \code{pairs_generate} or similar
#'   function.
#' @seealso \code{\link{pairs_plot_inclusions}}
#' @examples
#' gp <- pairs_generate(letters[1:7],
#'	                     av_inclusions = 15,
#'	                     inclusion_tolerance = 2,
#'	                     chain_length = 1)
#' pairs_plot(gp)
#' @export
pairs_plot <- function(pairs) {
	# recover script labels
	scripts <- unique(unlist(pairs[,1:2]))
	scripts_ordered <- scripts[order(scripts)]

	# all possible pairs of combinations
	combinations <- t(utils::combn(scripts_ordered, 2))

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
	graphics::plot(unique_pairs[[2]], unique_pairs[[1]], pch = 20,
	  main = "Inclusions of Pairs", xlab = "Unique Pair Combination", ylab = "Frequency",
	  sub = paste(nrow(pairs), "Pairs, ", nrow(combinations), "Combinations"),
		font.sub = 3,
		yaxt="n", ylim = c(min(unique_pairs[[1]])-1, max(unique_pairs[[1]])))
	graphics::axis(side = 2, at = 0:max(unique_pairs[[1]]) )
}



#' Plot the scores of performances in each pair
#'
#' This plot can be used to check that groups of performances are not isolated,
#' and provides an indication that the separation constraint used in
#' \code{pairs_generate} or similar function is working correctly.  Currently,
#' overplotting of performance labels occurs for performances which have close
#' scores.
#'
#' @param pairs Data frame returned from \code{pairs_generate} or similar
#'   function.
#' @param scores_df Data frame containing the variables \code{media, core,
#'   score}
#' @examples
#' pairs <- pairs_generate(data_standard[1:10, ],
#'	                      av_inclusions = 10,
#'	                      inclusion_tolerance = 2, chain_length = 1)
#' pairs_plot_scores(pairs, data_standard[1:10, ])
#' @export
pairs_plot_scores <- function(pairs, scores_df) {
  pairs_table <- merge(pairs, scores_df, by.x = "left", by.y = "media", sort = FALSE)
  pairs_table <- merge(pairs_table, scores_df, by.x = "right", by.y = "media", sort = FALSE)
	graphics::plot(-1, -1, xlim = c(0, 1.1), ylim = c(0, max(c(pairs_table$score.x, pairs_table$score.y))), xlab = "", ylab = "score", xaxt = "na",
	     main = "Scores of Performances in Each Pair",
	     sub = "Pairings")
	graphics::axis(side = 1, at = 0:1, labels = c("left", "right"))
	for(i in 1:nrow(pairs_table)) {
	  graphics::lines(c(0, 1), c(pairs_table[i, "score.x"], pairs_table[i, "score.y"]))
	  graphics::text(c(0, 1), c(pairs_table[i, "score.x"], pairs_table[i, "score.y"]),
	       labels = pairs_table[i, 1:2], pos = c(2, 4), offset = c(0.2, 0.2))
	}
}

