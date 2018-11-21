#'Show scores and score difference for a set of pairs
#'
#'Use this function to check that a separation constraint has been successfully
#'applied using \code{pairs_generate}, or similar function.
#'
#'@param pairs Data frame returned from \code{generate_pairs} or similar
#'  function
#'@param media Data frame containing the variables \code{media, core, score}.
#'  \code{score} must be numeric.
#'
#'@return Data frame similar to \code{pairs}, but with \code{left_score},
#'  \code{right_score}, and \code{score_difference} columns added.
#' @examples
#' # check separation constraint
#' pairs <- pairs_generate(data_standard,
#'	                        av_inclusions = 5,
#'	                        inclusion_tolerance = 2,
#'	                        chain_length = 1,
#'	                        separation_constraint = 30)
#' pairs_scores <- score_diff(pairs, data_standard)
#' max(pairs_scores$score_difference)
#'
#'@export
score_diff <- function(pairs, media) {
	stopifnot(class(pairs) == "data.frame", class(media) == "data.frame")
	stopifnot(!is.null(pairs$left), !is.null(pairs$right))
	stopifnot(!is.null(media$media), is.numeric(media$score))
	joined <- dplyr::left_join(pairs, media, by = c("left" = "media"))
	joined <- dplyr::left_join(joined, media, by = c("right" = "media"))
	joined <- joined[ , c("left", "right", "score.x", "score.y")]
	score_difference <- abs(joined$score.x - joined$score.y)
	joined <- cbind(joined, score_difference)
	colnames(joined)[3:4] <- c("left_score", "right_score")
	joined
}
