# barchart to see how well each script is represented.
pairs_plot_inclusions <- function(gp, min_c = NULL, max_c = NULL) {
	sampling <- rle(sort(unlist(gp[ , 1:2])))
	s.f. <- sampling[[1]]
	names(s.f.) <- sampling[[2]]
	uylim <- max(s.f.)

	barplot(s.f., ylim = c(0, uylim), main = "Inclusions of Performances",
	        sub = paste("number of performances =", length(unique(unlist(gp[ ,1:2]))),
	        	           ", number of pairs = ", nrow(gp), sep = ""),
		      xlab = "", ylab = "inclusions")
	abline(h = min_c, lty = 2, col = "orange")
	abline(h = max_c, lty = 2, col = "orange")
}
