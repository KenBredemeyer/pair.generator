#' barchart to see how well each script is represented.
#'
#' @param x A data frame containing pairs of performances.
#' @export
pairs_plot_inclusions <- function(gp) {
	sampling <- rle(sort(unlist(gp[ , 1:2])))
	s.f. <- sampling[[1]]
	names(s.f.) <- sampling[[2]]
	uylim <- max(s.f.)

	barplot(s.f., ylim = c(0, uylim), main = "Inclusions of Performances",
	        sub = paste("number of performances =", length(unique(unlist(gp[ ,1:2]))),
	        	           ", number of pairs = ", nrow(gp), sep = ""),
		      xlab = "", ylab = "inclusions")
	abline(h = attributes(gp)$user_inputs$min_c, lty = 2, col = "orange")
	abline(h = attributes(gp)$user_inputs$max_c, lty = 2, col = "orange")
}
