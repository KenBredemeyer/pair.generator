#' Allocate pairs to judges
#'
#' @param pairs data frame containing pairs in two columns, such as returned
#'   from \code{pairs_generate}
#' @param judges Single integer specifying the number of judges to allocate
#'   pairs evenly to, or a vector of integers specifying the number of pairs to
#'   allocate to each judge.
#' @return data frame with pairs and judge number.
#' @export
allocate <- function(pairs, judges) {
	# allocate pairs_each (equal pairs for each judge)
	if (length(judges) == 1) {
		pairs_each <- dim(pairs)[1] %/% judges
		left_over <- dim(pairs)[1] %% judges
		judge <- c(rep(1:judges, each = pairs_each), seq_len(left_over))
	} else if (length(judges) > 1) {
    judge <- rep(1:length(judges), times = judges)
	}
	pairs <- cbind(pairs, judge)
  pairs
}



#' Allocate pairs to judges, preventing judge duplicates
#'
#' @export
allocate2 <- function(pairs, n_judges) {
	## what if uneven allocations to judges??
	stopifnot()
	dups <- suppressMessages(duplicates2(pairs))
	if (!is.null(dups)) {
		stopifnot(n_judges >= dups[1, "freq"])    # "warning: delete some duplilcates or ..."
		# delete excessive duplicates?

		pairs_dups <- pairs[dups$pairs_i, ]
		pairs_no_dups <- pairs[-dups$pairs_i, ]

		# allocate dups
		dup_pairs_each <- dim(pairs_dups)[1] %/% n_judges
		dups_left_over <- dim(pairs_dups)[1] %% n_judges
		dups_judge <- c(rep(1:n_judges, times = dup_pairs_each), 1:dups_left_over)

    allocated_duplicates <- cbind(pairs_dups, judge = dups_judge)
		# allocate pairs_no_dups      # can allocate from last judge to first so that first judge won't have 2 more comparisons than the last
		nodup_pairs_each <- dim(pairs_no_dups)[1] %/% n_judges
		nodups_left_over <- dim(pairs_no_dups)[1] %% n_judges
		nodups_judge <- c(rep(1:n_judges, times = nodup_pairs_each), 1:nodups_left_over)

		allocated_nonduplicates <- cbind(pairs_no_dups, judge = nodups_judge)
		allocated_pairs <- rbind(allocated_duplicates, allocated_nonduplicates)
		allocated_pairs[order(allocated_pairs$judge), ]

	} else if (is.null(dups)) {
		allocate(pairs, n_judges)
	}
}
