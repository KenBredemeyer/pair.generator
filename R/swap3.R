#' Swap pairs to improve inclusion range for performances
#' @param gp Data frame of left/right comparisons
#' @param combinations Data frame containing \code{left} veriable in column 1
#'   and \code{right} variable in column 2.  All possible pais of performances.
#'   \code{combinations} can be created will \code{pairs_all}.
#' @param av_inclusions Integer. Average number of times performances are to be
#'   included in the set of generated pairs.
#' @param inclusion_tolerance Integer.  Difference between the maximum or
#'   minimnumber of times each performnace is included and the average
#'   inclusions.
#' @param animate Logical.  Should plots showing the current number of
#'   inclusions for each performance be displayed.
#' @param max_iterations Integer.  Maximum number of loops executed.
#' @param allow_repeats Logical.  Should duplicate pairs be generated.
#' @export
improve_inclusions <- function(gp, combinations, av_inclusions, inclusion_tolerance,
	                animate = FALSE, max_iterations = 600, allow_repeats = FALSE) {

	min_c <- av_inclusions - inclusion_tolerance
	max_c <- av_inclusions + inclusion_tolerance
	sampling <- rle(sort(unlist(gp[ , 1:2])))
	# s.f. = sampling frequencies
	s.f. <- sampling[[1]]
	names(s.f.) <- sampling[[2]]
	ulim <- max(s.f.)
	# find scripts which do not conform to min & max inclusions
	above_max <- names(s.f.[which(s.f. > max_c)])
	below_min <- names(s.f.[which(s.f. < min_c)])
	equal_min <- names(s.f.[which(s.f. == min_c)])

	loop_index <- 1

	while ((length(above_max) > 0 | length(below_min) > 0) & loop_index < max_iterations) {
		min_s.f. <- names(which(s.f. == min(s.f.)))
		highs <- names(sort(s.f., decreasing = TRUE)[sort(s.f., decreasing = TRUE) >= max_c])

		most_sampled <- which(gp[,1] == highs[1] | gp[,2] == highs[1])   # gives row indices for max(s.f.)
		# not below_min comparisons:  (for any given above_max to swap out, must not swap out any below_min
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

		# pair to swap out is intersection of max_s.f. & not below or equal to min_c
		#  if the max is only involved out of chain with equal or below min - then throw out the equal min, and move to the next max
		swap_out <- Reduce(intersect, list(most_sampled, nmin))
		if (length(swap_out) == 0) {
			swap_out <- Reduce(intersect, list(most_sampled, nbmin_all))
		}
		if (length(swap_out) > 0) {
			swap_out1 <- sample(swap_out, 1)
		}

		if (length(swap_out) == 0) {
			message("swap method stopped: could not find a pair to swap out")
			break
		}

		if (!allow_repeats) {
			# no repeated comparisons
	    sampled_pairs_i <- gp[ , "combination"]
	    sampled_rows <- match(sampled_pairs_i, combinations[ , 3])
	    combinations_unsampled <- combinations[-sampled_rows, ]

			swap_in <- combinations_unsampled[combinations_unsampled[ , 1] == min_s.f.[1] |
						combinations_unsampled[ , 2] == min_s.f.[1], ]
		} else if (allow_repeats) {
			swap_in <- combinations[combinations[ , 1] == min_s.f.[1] |
						combinations[ , 2] == min_s.f.[1], ]
		}

		if (nrow(swap_in) == 0) {
			message("pairs generated, but could not meet specified inclusions")
			break
		} else if (nrow(swap_in) == 1) {
	  	gp[swap_out1, ] <- swap_in
			#gp[swap_out1, 3] <- as.numeric(rownames(swap_in))
		} else if (nrow(swap_in) > 1) {
		  swap_in <- swap_in[1,]   #or sample a row.
	  	gp[swap_out1, ] <- swap_in
			#gp[swap_out1, 3] <- as.numeric(rownames(swap_in))
    }
		# update s.f., above_max, below_min, equal_min
		sampling <- rle(sort(unlist(gp[ , 1:2])))
		s.f. <- sampling[[1]]
		names(s.f.) <- sampling[[2]]
		above_max <- names(s.f.[which(s.f. > max_c)])
		below_min <- names(s.f.[which(s.f. < min_c)])
		equal_min <- names(s.f.[which(s.f. == min_c)])

		if (is.numeric(animate)) {
			speed <- animate
			animate <- TRUE
		} else if (!is.numeric(animate)) speed = 0.2

		if (animate) {
			Sys.sleep(speed)
	    graphics::barplot(s.f., main = "swap method",
	        xlab = "scripts", ylab = "inclusions",
	    	  ylim = c(0, ulim))
		  graphics::abline(h = min_c, lty = 2, col = "orange")
			graphics::abline(h = max_c, lty = 2, col = "orange")
		}

		if (loop_index %% 20 == 0) {
			message(paste0("iteration number  ", loop_index))
			print(pairs_inclusions_range(gp))
		}
	loop_index <- loop_index + 1
	}
	gp
}
