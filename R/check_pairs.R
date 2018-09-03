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


#' Test whether any judges have repeated comparisons
#'
#' @param allocated_pairs data.frame returned from \code{allocate}, or a list of
#'   pairwise comparisons with \code{judge} variable.
#' @return A message stating which if any judges have been allocated repeated
#'   pairs.
#' @seealso \code{\link{allocate}} \code{\link{duplicates}}
#' @export
any_judge_duplicates <- function(allocated_pairs) {
	judge_sets <- split(allocated_pairs, f = as.factor(allocated_pairs$judge))
	jdups <- lapply(judge_sets, duplicates)
	n_duplicates <- lapply(jdups, nrow)

	r_jdups <- lapply(judge_sets, reverse_duplicates)
	no_reverse_dups <- is.null(unlist(r_jdups))

	if (!no_reverse_dups) {
    xdups_rev <- lapply(x, reverse_duplicates)
		rows_revdups <- lapply(xdups_rev, nrow)
		judges_with_rdups <- unname(which(unlist(rows_revdups) != 0))
    message("Reverse duplicates found for the following judges: ")
    cat(judges_with_rdups)
	}

	if (any(n_duplicates != 0)) {
		judges_with_dups <- unname(which(unlist(n_duplicates) != 0))
	  message("The following judges have more than one unique comparison: ")
	  cat(judges_with_dups)
	  message("\nUse duplicates() to find repeated comparisons")
	} else if (all(n_duplicates == 0) && no_reverse_dups) {
		message("No repeated pairs for any judge")
	}
}

