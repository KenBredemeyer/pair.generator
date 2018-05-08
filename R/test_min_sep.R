#' test minimum separation constraint
#'
#' @param scores Numeric vector of scores from the scores file
#' @param separation_constraint Numeric value to be used in \code{pairs_make}
#'
#' @return message suggesting the minimum separation constraint to use in
#'   \code{pairs_make}.  Using a separation constraint below this value will
#'   almost certainly cause problems during estimation of item parameters.  In
#'   particular, this function tests for the potential for discontinuity in the
#'   scale.  It is recommended to use a separation constraint (if at all) that
#'   is well above the advised minimum.
#' @export
test_min_separation <- function(scores, separation_constraint) {   # could call sep_const. "max_score_range"
	ordered_scores <- round(sort(scores), 3)
  MDL <- which(diff(ordered_scores) == max(diff(ordered_scores)))
	if (all(MDL > 1 & MDL < length(ordered_scores)-1)) {
	  min_sep <- sum(diff(ordered_scores)[(MDL[1]-1):(MDL[1]+1)])
	} else if(any(MDL == 1) & all(MDL != length(ordered_scores)-1 )) {
		min_sep <- sum(diff(ordered_scores)[(MDL[1]):(MDL[1]+2)])
	} else if(any(MDL == length(ordered_scores)-1 & all(MDL != 1))) {
		min_sep <- sum(diff(ordered_scores)[(MDL[1]-2):(MDL[1])])
	} else if(any(MDL == 1) & any(MDL == length(ordered_scores)-1)) {
    min_sep <- sum(diff(ordered_scores)[(MDL[1]):(MDL[1]+2)])
	}
  #cat(MDL, "  ", ordered_scores)
 	if (separation_constraint < min_sep) {
		message("separation_constraint is too small and will cause discontinuity in the scale. \n",
            "Increase separation_constraint to ", min_sep, " or more. \n")
	}
}
