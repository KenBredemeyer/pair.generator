# swap pairs to improve inclusion range for performances
# this function preserves chains (at the expense of inclusions)
swap <- function(gp, s.f., combinations, min_c, max_c) {
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
				for (j in 1:length(below_min)) {
					nbmin[[j]] <- which(gp[,1] != below_min[j] & gp[,2] != below_min[j])
				}
				nbmin_all <- Reduce(intersect, nbmin)
			} else nbmin_all <- 1:nrow(gp)
			# not equal_min
			nemin <- list()
			if (length(equal_min) > 0) {
				for (k in 1:length(equal_min)) {
					nemin[[k]] <- which(gp[,1] != equal_min[k] & gp[,2] != equal_min[k])
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

		swap_in <- combinations[(combinations[-swap_chain_pairs, 1] == min_s.f.[1] & combinations[-swap_chain_pairs, 2] == swap_out_chain) |
					(combinations[-swap_chain_pairs, 2] == min_s.f.[1] & combinations[-swap_chain_pairs, 1] == swap_out_chain), ]

		if (nrow(swap_in) == 0) {
			message("pairs generated, but could not meet specified inclusions")
			break
		} else if (nrow(swap_in) == 1) {
	  	gp[swap_out1, 1:2] <- swap_in
			gp[swap_out1, 3] <- as.numeric(rownames(swap_in))
		} else if (nrow(swap_in) > 1) {
		  swap_in <- swap_in[1,]   #or sample a row.
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
	gp
}
