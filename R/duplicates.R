#' Show all duplicates in a set of pairs
#'
#' Use this function on objects returned from \code{pairs_generate}, or similar
#' functions, to check if there are repeated comparisons for judges.
#'
#' @param pairs A data frame containing pairs, for example, an object returned
#'   from \code{pairs_generate}.  This data frame must include the
#'   \code{combinations} variable.
#' @param message Logical.  Should a message be returned if there are no
#'   repeated pairs.
#'
#' @return A data frame showing which pairs are repeated, if any, or \code{NULL}
#'   if there are no repeated pairs.  A message is also optionally returned if
#'   there are no repeated pairs.
#'
#' @examples
#' # check if there are any duplicates in a set of pairs
#' gp <- pairs_generate(data_standard,
#'                      av_inclusions = 10,
#'                      inclusion_tolerance = 2,
#'                      chain_length = 1)
#' duplicates(gp)
#'
#' # check if there are any duplicates for the judges
#' allocated_pairs <- pairs_allocate(gp, judges = 5)
#' judge_sets <- split(allocated_pairs, f = as.factor(allocated_pairs$judge))
#' judge_duplicates <- lapply(judge_sets, duplicates)
#'
#' @export
duplicates <- function(pairs, message = TRUE) {
	stopifnot(is.numeric(pairs$combination) && !any(is.na(pairs$combination)))

	dups_i <- which(duplicated(pairs$combination) | duplicated(pairs$combination, fromLast = TRUE))
  if (length(dups_i) > 0) {
		duplicates_df <- cbind(pairs[dups_i, ], pairs_i = dups_i)

		duplicates_df <- duplicates_df[order(duplicates_df$combination), ]
		duplicates_df <- transform(duplicates_df, freq= ave(seq(nrow(duplicates_df)), combination, FUN=length))
	  duplicates_df <- duplicates_df[order(-duplicates_df$freq), ]
	  rownames(duplicates_df) <- 1:dim(duplicates_df)[1]
	  duplicates_df
  } else if(message) {
  	message("no duplicates")
  }
}
