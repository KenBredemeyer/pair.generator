#' Pair non-core performances with core performances
#'
#' This type of pair generation is for placing new performances on an existing
#' scale.  "Core" performances should have location estimates, and "non-core"
#' performances are to be placed on the scale formed using the "core"
#' performances.
#'
#' @param media A data frame containing the variables: \code{media}, a character
#'   vector of performance labels; \code{core}, a numeric vector, whether core
#'   (\code{0}) or non-core (\code{1}).
#'
#' @param nc_include Integer.  The number of core performances to pair with each
#'   non-core performance.
#'
#' @param chain_length Integer.  The number of successive pairs to contain a
#'   common performance (used to increase the efficiency of judging).
#'
#' @param separation_constraint Numeric, or \code{FALSE} for no separation
#'   constraint.  The maximum absolute difference between performance scores in
#'   a pair.
#'
#' @return A data frame containing the variables \code{left}, \code{right}, and
#'   \code{chain_number}.  Each \code{left, right} pair will contain one "core"
#'   and one "non-core" performance.
#'
#' @examples
#' pairs_generate_cnc(data_cnc[1:4, ], nc_include = 1, chain_length = 1)
#'
#' @export
pairs_generate_cnc <- function(media, nc_include, chain_length = 4, separation_constraint = FALSE) {

  stringsAsFactorsOption <- getOption("stringsAsFactors")
  options(stringsAsFactors = FALSE)

	# core scripts
	scripts <- media[media[,2] == 1, c("media", "score")]

	# non-core scripts
	nc_scripts <- media[media[,2] == 0, c("media", "score")]

	# for no separation constraint
	if (!separation_constraint) {
		# s1 is non-core sampling
		s1 <- sampler(nc_scripts[ , "media"], nrow(nc_scripts) * nc_include)
		# s2 for sampling of core scripts
		s2 <- list()  # probably not the best way.
		# nc_include must be less than the number of core scripts
		for (i in 1:nrow(nc_scripts))
			s2[[i]] <- sample(scripts[,"media"], nc_include)

		cnc_gp <- data.frame(non_core = I(s1), core = unlist(s2)[order(order(s1))])
	}

	if (is.numeric(separation_constraint)) {
		stopifnot(!any(is.na(scripts$score)), is.numeric(scripts$score))
		# make list of all possible pairings between core and non core
		cnc_combinations <- expand.grid(scripts[,1], nc_scripts[,1])
		# include scores
		cnc_scores <- expand.grid(scripts[,2], nc_scripts[,2])
		cnc_combinations <- cbind(cnc_combinations, cnc_scores)
		colnames(cnc_combinations) <- c("core_script", "non_core_script", "core_score", "non-core_score")

		# apply separation constraint to `cnc_combinations`
		cnc_combinations <- cbind(cnc_combinations, abs(cnc_combinations[,3] - cnc_combinations[,4]))
		colnames(cnc_combinations)[5] <- "diff"

		cnc_combinations <- cnc_combinations[cnc_combinations[,"diff"] <= separation_constraint, ]

		# for each nc_script (unique), sample `nc_include` core scripts from cnc_combinations.
		nonCore <- factor(cnc_combinations$non_core_script)

		paired_cores <- tapply(cnc_combinations$core_script, nonCore, function(x) sample(x, nc_include))
		cnc_gp <- data.frame(non_core = rep(unique(as.character(nonCore)), each = nc_include), core = as.character(unname(unlist(paired_cores))))
	}

	## apply chaining
	reps <- floor(nc_include / chain_length)
	rem <- nc_include %% chain_length

	index <- list()
	for (i in 1:length(nc_scripts[,"media"]))
		index[[i]] <- which(cnc_gp == unique(cnc_gp[,"non_core"])[i])

	thing <- list()
	index_vector <- list()
	for (j in 1:reps) {
		for (i in 1:length(index)) {
			thing[[i]] <- index[[i]][(chain_length*(j-1)+1):(j*chain_length)] }
		index_vector[[j]] <- do.call(c, thing)
	}

	# elements which cannot be chained completely
	if (rem != 0) {
		for (i in 1:length(index)) {
			thing[[i]] <- index[[i]][(j*chain_length+1):(j*chain_length+rem)]
		}
		index_vector[[j+1]] <- do.call(c, thing)
	}

	index_vector <- do.call(c, index_vector)

	cnc_gp <- cnc_gp[index_vector, ]

	# calculate chain number
	non_core_count <- length(unique(cnc_gp[,"non_core"]))
	chain_number <- c(rep(1 : (non_core_count * reps), each = chain_length),
              			rep((1 + (non_core_count * reps)) : (non_core_count + (non_core_count * reps)), each = rem))

	# randomize left/right presentation
	cnc_gp <- switch_lr(cnc_gp)
	colnames(cnc_gp) <- c("left", "right")

	# add chain number
	cnc_gp <- cbind(cnc_gp, chain_number)

  row.names(cnc_gp) <- 1:dim(cnc_gp)[1]
	options(stringsAsFactors = stringsAsFactorsOption)
	return(cnc_gp)
}
