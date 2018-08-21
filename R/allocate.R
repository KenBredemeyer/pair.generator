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
