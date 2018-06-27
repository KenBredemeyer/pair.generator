#' Form pairs from scripts
#'
#' Standard pairs generation.
#'
#' This function does not chain pairs.
#' @param media Character vector of media or performance labels, or a data.frame
#'   containing "media", "core", and "score" variables.
#' @param av_inclusions Integer specifying the average number of inclusions per
#'   media or performance.
#' @param inclusion_tolerance Integer specifying the range around the average
#'   number of incluisons.
#' @param seed Integer. Random number seed.
#' @param animate Logical or numeric. If logical, should inclusion plots be
#'   produced? If numeric, the delay between frames is seconds via
#'   \code{Sys.sleep}
#'
#' @export
pairs_generate <- function(media, av_inclusions, inclusion_tolerance,
	                         separation_constraint = NULL, seed = 1, animate = FALSE) {
  stopifnot(av_inclusions %% 1 == 0)
	if (is.numeric(separation_constraint)) {
		stopifnot(!is.null(media$score), !any(is.na(media$score)), is.numeric(media$score))
	}

	stringsAsFactorsOption <- getOption("stringsAsFactors")
  options(stringsAsFactors = FALSE)
  set.seed(seed)

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
  combinations <- data.frame(t(combn(scripts, 2)))   # allow for separation constraint
  combinations[,3] <- 1:dim(combinations)[1]
  if (is.numeric(separation_constraint)) {
  	combinations_scores <- data.frame(t(combn(media$score, 2)))
  	available_comparisons_i <- which(abs(combinations_scores[,1] - combinations_scores[,2]) <= separation_constraint)
  	combinations <- combinations[available_comparisons_i, ]
  }

  pairs_i <- sample(dim(combinations)[1], pairs_length)  #! repeated pairs if pairs_length > dim(combinations)[1]  warn user of repeats
  gp <- combinations[pairs_i, ]

  # generated pairs swapped
  gps <- swap2(gp = gp, combinations = combinations, av_inclusions = av_inclusions,
  	           inclusion_tolerance = inclusion_tolerance, animate = animate, seed = seed)
  options(stringsAsFactors = stringsAsFactorsOption)
  attr(gps, "initial_sampling") <- gp
  colnames(gps) <- c("left", "right", "combination")
  gps
}
