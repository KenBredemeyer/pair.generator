#' Form pairs from scripts
#'
#' Standard pairs generation.
#'
#' This function does not chain pairs.
#' @param media Character vector of media or performance labels, or a data.frame
#'   containing "media", "core", and "score" variables.
#' @param av_inclusions Integer specifying the average number of inclusions per
#'   media or performance.
#' @param inclusion_tolerance Integer. What should the difference between
#'   average inclusions and minimum inclusions, or between maximum inclusions
#'   and average inclusions be.
#' @param seed Integer. Random number seed.
#' @param separation_constraint Numeric. The maximum absolute score difference
#'   between media/performances in a pair.
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



#' generalized pair generator
#' @export
gen_pairs_g <- function(media, av_inclusions, inclusion_tolerance,
	separation_constraint = NULL, chain_length, seed = 1, animate = FALSE) {
	# media can be vector or data.frame
	if (!is.null(dim(media))) {
    n_scripts <- dim(media)[1]
	} else {
		n_scripts <- length(media)
	}
	n_exhaustive_pairs <- n_scripts * (n_scripts - 1) / 2
	if(av_inclusions <= n_scripts - 1) {
		gp <- pairs_generate(media = media, av_inclusions = av_inclusions,
			     inclusion_tolerance = inclusion_tolerance, separation_constraint = separation_constraint,
			     seed = 1, animate = FALSE)
		# gp <- chain(gp, )
	} else if(av_inclusions > n_scripts - 1) {
		reps <- av_inclusions %/% (n_scripts - 1)
		rem <- av_inclusions %% (n_scripts - 1)
		# generate remaining pairs
		if (rem != 0) {
		gpr <- pairs_generate(media, av_inclusions = rem, inclusion_tolerance = inclusion_tolerance,
			                    separation_constraint = separation_constraint, seed = 1, animate = FALSE)
		gpr <- chain(gpr, chain_length = chain_length)

		# generated pairs exhaustive
		gpe <- exhaustive_pairs(media, n_judges = reps, separation_constraint = separation_constraint,
			head_order = gpr$combination, chain_length = chain_length)
    gpe[ , 3] <- gpe[ , 3] + tail(gpr, 1)$chain
    gpe[1:dim(gpr)[1], 3] <- gpr$chain
    if (reps > 1) {
	    for (i in seq_len(reps-1)) {
	    	gpe[(1+i*n_exhaustive_pairs):(i*n_exhaustive_pairs + dim(gpr)[1]), 3] <- gpr$chain
	    }
    }
	  gp <- rbind(gpe[ , c("left", "right", "chain_number")], gpr[ , c("left", "right", "chain_number")])
		} else {
			gp <- exhaustive_pairs(media, n_judges = reps, separation_constraint = separation_constraint,
				                     chain_length = chain_length)
		}
	}
  gp
}

