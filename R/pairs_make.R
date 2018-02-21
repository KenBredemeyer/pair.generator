# generate pairs fun def
pairs_make <- function(x, type = "standard_pairs", chaining_constant = 4, separation_constraint = FALSE,
	                         nc_include = NA, min_c = 10, max_c = 15, seed = 1) {
  set.seed(seed)

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

		return(cnc_gp)
	}

	if (type == "standard_pairs") {

		# run standard pairs only for performances marked as core.
		scripts <- x[x[,2] == 1, c("media", "score")]

		if (length(scripts$media) < 2) stop("must have at least 2 performances to compare")

		# create all possible combinations of pairs
		options(stringsAsFactors = FALSE)
		combination_labels <- t(combn(scripts$media, 2))
		combination_values <- (t(combn(scripts$score, 2)))
		combinations <- cbind(combination_labels, as.data.frame(combination_values))


		if (is.numeric(separation_constraint)) {

			# sort to ascending order (needed for diff calculation below)
			ordered_scores <- round(sort(scripts$score), 3)

			# calculate minimum possible valid separation constraint for scale continuity
			MDL <- which(diff(ordered_scores) == max(diff(ordered_scores)))
			min_sep <- sum(diff(ordered_scores)[(MDL-1):(MDL+1)])

			# warning if separation constraint will cause scale discontinuity
			if (separation_constraint < min_sep) {
				warning("separation_constraint is too small and will cause discontinuity in the scale. \n
                 Increase separation_constraint to ", min_sep, " or more")
			}

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

		# find scripts which do not conform to min & max inclusions
		above_max <- names(s.f.[which(s.f. > max_c)])
		below_min <- names(s.f.[which(s.f. < min_c)])
		equal_min <- names(s.f.[which(s.f. == min_c)])


		while (length(above_max) > 0 | length(below_min) > 0) {
			min_s.f. <- names(which(s.f. == min(s.f.)))    # can have length more than 1
			not_min_chain <- as.numeric(rownames(gp[gp[,"chain"]!=min_s.f.[1], ]))
			## stuff to swap out: ##

			highs <- names(sort(s.f., decreasing = TRUE)[sort(s.f., decreasing = TRUE) >= max_c])
			for (i in 1:length(highs)) {

				most_sampled <- which(gp[,1] == highs[i] | gp[,2] == highs[i])   # gives row indices for max(s.f.)

				# not max chain
				nmc <- which(gp[ , "chain"] != highs[i])

				# not below_min comparisons:  (for any given above_max to swap out, must not swap out _any_ below_min
				nbmin <- list()
				if (length(below_min) > 0) {
					for (i in 1:length(below_min)) {
						nbmin[[i]] <- which(gp[,1] != below_min[i] & gp[,2] != below_min[i])
					}
					nbmin_all <- Reduce(intersect, nbmin)
				} else nbmin_all <- 1:nrow(gp)
				# not equal_min
				nemin <- list()
				if (length(equal_min) > 0) {
					for (i in 1:length(equal_min)) {
						nemin[[i]] <- which(gp[,1] != equal_min[i] & gp[,2] != equal_min[i])
					}
					nemin_all <- Reduce(intersect, nemin)
				} else nemin_all <- 1:nrow(gp)
				nmin <- union(nbmin_all, nemin_all)

				# pair to swap out is intersection of max_s.f., not max_s.f. chain, not below or equal to min_c, not_min_chain
				#  if the max is only involved out of chain with equal or below min - then throw out the equal min, and move to the next max
				swap_out <- Reduce(intersect, list(most_sampled, nmc, nmin, not_min_chain))
				if (length(swap_out) == 0) {
					swap_out <- Reduce(intersect, list(most_sampled, nmc, nbmin_all))
				}
				if (length(swap_out) > 0) {
					swap_out1 <- sample(swap_out, 1)
					swap_out_chain <- gp[swap_out1, "chain"] # a character string
				}
			}
      if (length(swap_out) == 0) {
      	message("swap method stopped: could not find a pair to swap out")
      	break
      }

			swap_chain_number <- gp[swap_out1, "chain_number"]
			swap_chain_pairs <- gp[gp$chain_number == swap_chain_number, "combination"]
			# do not swap in a pair that already exists in that chain (avoid repetition of pairs in-chain)
			# pair to swap in:
			for (i in 1:length(min_s.f.)) {
				swap_in <- combinations[(combinations[-swap_chain_pairs, 1] == min_s.f.[i] & combinations[-swap_chain_pairs, 2] == swap_out_chain) |
	                              (combinations[-swap_chain_pairs, 2] == min_s.f.[i] & combinations[-swap_chain_pairs, 1] == swap_out_chain), ]

				if (nrow(swap_in) > 0) {
					gp[swap_out1, 1:2] <- swap_in
					gp[swap_out1, 3] <- as.numeric(rownames(swap_in))
				}

				# update s.f., above_max, below_min, equal_min
				sampling <- rle(sort(unlist(gp[ , 1:2])))
				s.f. <- sampling[[1]]
				names(s.f.) <- sampling[[2]]
				above_max <- names(s.f.[which(s.f. > max_c)])
				below_min <- names(s.f.[which(s.f. < min_c)])
				equal_min <- names(s.f.[which(s.f. == min_c)])

			}
			if (nrow(swap_in) == 0) {
				message("pairs generated, but could not meet specified inclusions")
				break
			}
		}
		gp[,1:2] <- switch_lr(gp[,1:2])
		return(gp)
	}
}
