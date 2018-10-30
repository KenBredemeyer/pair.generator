#' Show scores and score difference for a set of pairs
#'
#' @param pairs data.frame returned from \code{generate_pairs} or similar
#'   function
#' @param media data.frame containing the variables \code{media, core, score}.
#'   \code{score} must be numeric.
#'
#' @return data.frame similar to \code{pairs}, but with \code{left_score,
#'   right_score, score_difference} added.
#' @export
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
