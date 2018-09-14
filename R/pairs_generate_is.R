#' Sample pairs from the set of all possible pairs
#'
#' Similar to \code{pairs_generate} but allows finer control.  All performances
#' are included in the set of generated pairs, but unlike \code{pairs_generate}
#' no attempt is made to conform to a range of inclusions per performance.  Use
#' with \code{improve_inclusions} to improve the range of inclusions of
#' performances in the set of pairs.
#'
#' Combine \code{pairs_sample} in a workflow with \code{improve_inclusions},
#' \code{chain} and \code{switch_lr} to acheive the same result as
#' \code{pairs_generate}
#'
#' \code{improve_inclusions} optionally shows an animation of inclusions and
#' optionally allows repeated pairs.
#'
#' @param media Character vector representing the names of the performances, or
#'   a data frame containing the variables \code{media, core, score}.
#' @param av_inclusions Integer specifying the average number of times each
#'   performance should be included in the generated pairs.
#' @param separation_constraint Numeric.  Maximum absolute difference in scores
#'   between performances in a pair.
#' @seealso \code{\link{pairs_generate}}, \code{\link{improve_inclusions}},
#'   \code{\link{chain}} and  \code{\link{switch_lr}}.
#' @export
pairs_sample <- function(media, av_inclusions,
	                         separation_constraint = NULL) {
  stopifnot(av_inclusions %% 1 == 0)
	if (is.numeric(separation_constraint)) {
		stopifnot(!is.null(media$score), !any(is.na(media$score)), is.numeric(media$score))
	}

	stringsAsFactorsOption <- getOption("stringsAsFactors")
  options(stringsAsFactors = FALSE)

	# take vector of script labels or a data.frame
	if (!is.null(dim(media))) {
		if(!is.null(media$media)) {
		  scripts <- media$media
		}
	} else {
		scripts <- media
	}

  if ((length(scripts) * av_inclusions) %% 2 == 0) {
    pairs_length <- length(scripts) * av_inclusions / 2
  } else if ((length(scripts) * av_inclusions) %% 2 == 1) {
  	pairs_length <- (length(scripts) * av_inclusions + 1) / 2
  }

  # form all pairs
  combinations <- data.frame(t(utils::combn(scripts, 2)))   # allow for separation constraint
  combinations[,3] <- 1:dim(combinations)[1]
  if (is.numeric(separation_constraint)) {
  	combinations_scores <- data.frame(t(utils::combn(media$score, 2)))
  	available_comparisons_i <- which(abs(combinations_scores[,1] - combinations_scores[,2]) <= separation_constraint)
  	combinations <- combinations[available_comparisons_i, ]
  }

  if (dim(combinations)[1] < pairs_length) {
  	stop("Not enough available combinations to meet the required inclusions of performances. \n
          Check the separation constraint is large enough to accomodate the number of pairs required.")
  }

  pairs_i <- sample(dim(combinations)[1], pairs_length)  #! repeated pairs if pairs_length > dim(combinations)[1]  warn user of repeats
  gp <- combinations[pairs_i, ]
  colnames(gp) <- c("left", "right", "combination")

  options(stringsAsFactors = stringsAsFactorsOption)
  rownames(gp) <- 1:dim(gp)[1]
  gp
}
