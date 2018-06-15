#' Generate Pairs
#'
#' Pair performances or media items for constructing a pairwise comparison
#' design.
#'
#' @param x A data.frame containing 3 variables named "media", "core", "score".
#'   Variable types must be "character", "numeric", "numeric".
#' @param type A character string, either \code{"core_noncore"} or
#'   \code{"standard_pairs"}. Use \code{"standard_pairs"} to construct a new
#'   scale, or \code{"core_noncore"} to place non-core items on an existing
#'   scale.
#' @param chaining_constant Integer.  Specifies the number of consecutive pairs
#'   containg a common media item or performance.
#' @param separation_constraint Numeric specifying the maximum range of scores
#'   to use when pairing, or FALSE for no separation constraint.
#' @param nc_include Use only for \code{type = "core_noncore"}.  Integer
#'   specifying the number of times each non-core performance is included.
#' @param min_c Integer. The minimum number of times a performance should be
#'   included. Minimum and maximum inclusions are not guaranteed. \code{min_c}
#'   and \code{max_c} are relevant for \code{type = "standard_pairs"} only.
#' @param max_c Integer. The maximum number of times a performance should be
#'   included.
#' @return A data frame with paired performances, where "left" and "right" form
#'   the pair. "combination" is the row number corresponding to a data frame
#'   containing all possiblge pairs, used for checks. For importing into an
#'   interface for judging, use only "left", "right" and "chain_number".
#' @export
pairs_make <- function(x, type = "standard_pairs", chaining_constant = 4, separation_constraint = FALSE,
	                         nc_include = NA, min_c = 10, max_c = 20, seed = 1) {
  set.seed(seed)
  stopifnot(type == "core_noncore" | type == "standard_pairs")

  stringsAsFactorsOption <- getOption("stringsAsFactors")
  options(stringsAsFactors = FALSE)

	if (type == "core_noncore") {
		# core scripts
		scripts <- x[x[,2] == 1, c("media", "score")]
		# pass `scripts` to swap.R to make `gp`

		# non-core scripts
		nc_scripts <- x[x[,2] == 0, c("media", "score")]

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
			stopifnot(!any(is.na(scripts$score)))
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
		reps <- floor(nc_include / chaining_constant)
		rem <- nc_include %% chaining_constant

		index <- list()
		for (i in 1:length(nc_scripts[,"media"]))
			index[[i]] <- which(cnc_gp == unique(cnc_gp[,"non_core"])[i])

		thing <- list()
		index_vector <- list()
		for (j in 1:reps) {
			for (i in 1:length(index)) {
				thing[[i]] <- index[[i]][(chaining_constant*(j-1)+1):(j*chaining_constant)] }
			index_vector[[j]] <- do.call(c, thing)
		}

		# elements which cannot be chained completely
		if (rem != 0) {
			for (i in 1:length(index)) {
				thing[[i]] <- index[[i]][(j*chaining_constant+1):(j*chaining_constant+rem)]
			}
			index_vector[[j+1]] <- do.call(c, thing)
		}

		index_vector <- do.call(c, index_vector)

		cnc_gp <- cnc_gp[index_vector, ]

		# calculate chain number
		non_core_count <- length(unique(cnc_gp[,"non_core"]))
		chain_number <- c(rep(1 : (non_core_count * reps), each = chaining_constant),
                			rep((1 + (non_core_count * reps)) : (non_core_count + (non_core_count * reps)), each = rem))

		# randomize left/right presentation
		cnc_gp <- switch_lr(cnc_gp)
		colnames(cnc_gp) <- c("left", "right")

		# add chain number
		cnc_gp <- cbind(cnc_gp, chain_number)

		options(stringsAsFactors = stringsAsFactorsOption)
		return(cnc_gp)
	}

	if (type == "standard_pairs") {

		# run standard pairs only for performances marked as core.
		scripts <- x[x[,2] == 1, c("media", "score")]

		if (length(scripts$media) < 2) stop("must have at least 2 performances to compare")

		# create all possible combinations of pairs
		combination_labels <- t(combn(scripts$media, 2))
		combination_values <- (t(combn(scripts$score, 2)))
		combinations <- cbind(combination_labels, as.data.frame(combination_values))


		if (is.numeric(separation_constraint)) {

			# test minimum separation constraint that will *probably* not cause scale discontinuity
			#test_min_separation(scripts$score, separation_constraint)

			available_comparisons <- combinations[which(abs(combinations[,3] - combinations[,4]) < separation_constraint), ]

			n <- mean(min_c, max_c) * length(scripts$score)
			n <- n + n %% chaining_constant                             # so required number of comparisons is a multiple of chaining constant
			if (nrow(available_comparisons) <= n) {
				stop("separation_constraint is too small. Not enough comparisons available")
			}
			combinations <- available_comparisons[ , 1:2]
		}
		######## end of separation_constraint

		# n = total number of pairs generated (i.e. nrows(gp))
		n <- ceiling(mean(c(min_c, max_c)) * length(scripts$media) / 2)
		n <- n + n %% chaining_constant

		s1 <- sampler(scripts$media, n/chaining_constant)    # s1 = sample 1.  How many to sample from compared to n?
		if (!is.numeric(separation_constraint))
			combinations <- t(combn(scripts$media, 2))

		# s1_c = sample 1 combinations
		# gp = generated pairs (core)

		gp <- list()
		combinations <- data.frame(combinations)

		for (i in 1:length(s1)) {
			s1_c <- combinations[(combinations[ ,1] == s1[i] | combinations[ ,2] == s1[i]), ]
			gp[[i]] <- s1_c[sample(nrow(s1_c), chaining_constant), ]
			gp[[i]] <- cbind(gp[[i]], as.numeric(rownames(gp[[i]])))
		}
		# above and lines 36 & 37:  better to put `combinations` rows into gp[,5] rather than rownames(gp)

		# turn the list into a single matrix
		gp <- do.call(rbind, gp)
		rownames(gp) <- 1:nrow(gp)

		sampling <- rle(sort(unlist(gp[ , 1:2])))
		# s.f. = sampling frequencies
		s.f. <- sampling[[1]]
		names(s.f.) <- sampling[[2]]

		# chain columns provide a conveinient way of searching or excluding
		gp <- cbind(gp, rep(s1, each = chaining_constant), rep(1:length(s1), each = chaining_constant))
		colnames(gp) <- c("left", "right", "combination", "chain", "chain_number")

		# find scripts which are missing altogether in the sample (gp), and include
		missing_scripts <- setdiff(scripts$media, unlist(gp[ ,1:2]))
		if (length(missing_scripts) > 0) {
			s.f. <- c(s.f., rep(0, length(missing_scripts)))
			names(s.f.)[(length(s.f.) - length(missing_scripts) + 1):length(s.f.)] <- missing_scripts
		}

		# improve range of inclusions of performances
	  gp <- swap(gp = gp, s.f. = s.f., combinations = combinations, min_c = min_c, max_c = max_c)

		gp[,1:2] <- switch_lr(gp[,1:2])
		attr(gp, "user_inputs") <- list(type = type, chaining_constant = chaining_constant,
			                              separation_constraint = separation_constraint,
	                                   nc_include = nc_include, min_c = min_c, max_c = max_c, seed = seed)
		options(stringsAsFactors = stringsAsFactorsOption)
		return(gp)
	}

}
