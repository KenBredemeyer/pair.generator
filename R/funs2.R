#' Tabulate inclusions for each performance
#'
#' @param pairs A data frame containing pairs of performances.
#'
#' @param return A named vector of performance inclusions
#' @export
inclusions <- function(pairs) {
	attach(pairs)
	stopifnot((exists("left") | exists("Left.Media")) & (exists("right") | exists("Right.Media")))
	if (exists("left") && exists("right")) {
		sampling <- rle(sort(unlist(pairs[ , c("left", "right")])))
	} else if (exists("Left.Media") && exists("Right.Media")) {
		sampling <- rle(sort(unlist(pairs[ , c("Left.Media", "Right.Media")])))
	}
	detach(pairs)
	s.f. <- sampling[[1]]
	names(s.f.) <- sampling[[2]]
	data.frame(inclusions = s.f.[order(s.f.)])
}
