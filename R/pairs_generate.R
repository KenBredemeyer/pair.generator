# Form pairs from scripts
#
# no duplicates.  function used in pairs_generate
# @param media Character vector representing the names of the performances, or
#   a data frame containing the variables \code{media, core, score}.
# @param av_inclusions Integer specifying the average number of times each
#   performance should be included in the generated pairs.
# @param inclusion_tolerance Integer specifying the difference between the
#   average number of incusions and the minimum or maximum number of
#   inclusions.
# @param separation_constraint Numeric.  Maximum absolute difference in scores
#   between performances in a pair.
pairs_generate_ <- function(media, av_inclusions, inclusion_tolerance,
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

  pairs_i <- sample(dim(combinations)[1], pairs_length)
  gp <- combinations[pairs_i, ]
  colnames(gp) <- c("left", "right", "combination")
  # generated pairs swapped
  gps <- swap2(gp = gp, combinations = combinations, av_inclusions = av_inclusions,
  	           inclusion_tolerance = inclusion_tolerance)
  options(stringsAsFactors = stringsAsFactorsOption)
  attr(gps, "initial_sampling") <- gp

  gps
}



#'Form pairs of performances for pairwise comparisons
#'
#'Standard pairs generation.
#'
#'@param media Character vector of media or performance labels, or a data.frame
#'  containing \code{media}, \code{core}, and \code{score} variables.
#'@param av_inclusions Integer specifying the average number of inclusions per
#'  media or performance.
#'@param inclusion_tolerance Integer. What should the difference between average
#'  inclusions and minimum inclusions, or between maximum inclusions and average
#'  inclusions be.
#'@param separation_constraint Numeric. The maximum absolute score difference
#'  between media/performances in a pair.
#'@param chain_length Integer. How many successive comparisons should share a
#'  common performance.  Used for efficiency of judging.
#'
#'@return A data frame containing the variables \code{left, right,
#'  chain_number}.  \code{left} is the name of the performance to be presented
#'  on the left side for judging.  \code{chain_number} indicates how consecutive
#'  comparisons use common performances to increase the efficiency of judging.
#'
#' @examples
#' pairs_generate(letters[1:3],
#'	               av_inclusions = 2,
#'	               inclusion_tolerance = 1,
#'	               chain_length = 1)
#'
#'@export
pairs_generate <- function(media, av_inclusions, inclusion_tolerance,
	separation_constraint = NULL, chain_length) {
	# media can be vector or data.frame
	if (!is.null(dim(media))) {
    n_scripts <- dim(media)[1]
	} else {
		n_scripts <- length(media)
	}
	n_exhaustive_pairs <- n_scripts * (n_scripts - 1) / 2
	if(av_inclusions <= n_scripts - 1) {
		gp <- pairs_generate_(media = media, av_inclusions = av_inclusions,
			     inclusion_tolerance = inclusion_tolerance, separation_constraint = separation_constraint)
		gp <- chain(gp, chain_length = chain_length)
	} else if(av_inclusions > n_scripts - 1) {
		reps <- av_inclusions %/% (n_scripts - 1)
		rem <- av_inclusions %% (n_scripts - 1)
		# generate remaining pairs
		if (rem != 0) {
			gpr <- pairs_generate_(media, av_inclusions = rem, inclusion_tolerance = inclusion_tolerance,
				                    separation_constraint = separation_constraint)
			gpr <- chain(gpr, chain_length = chain_length)

			# generated pairs exhaustive
			gpe <- exhaustive_pairs(media, n_judges = reps, separation_constraint = separation_constraint,
				head_order = gpr$combination, chain_length = chain_length)
	    gpe[["chain_number"]] <- gpe[["chain_number"]] + utils::tail(gpr, 1)[["chain_number"]]
	    gpe[1:dim(gpr)[1], "chain_number"] <- gpr[["chain_number"]]
	    if (reps > 1) {
		    for (i in seq_len(reps-1)) {
		    	gpe[(1+i*n_exhaustive_pairs):(i*n_exhaustive_pairs + dim(gpr)[1]), 4] <- gpr[["chain_number"]]
		    }
	    }
		  gp <- rbind(gpe, gpr)
		} else {
			gp <- exhaustive_pairs(media, n_judges = reps, separation_constraint = separation_constraint,
				                     chain_length = chain_length)
		}
	}
  gp <- switch_lr(gp)
  rownames(gp) <- 1:dim(gp)[1]
  gp
}

