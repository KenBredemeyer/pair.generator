#' Form pairs from scripts
#'
#' Standard pairs generation
#'
#' @export
pairs_generate <- function(media, av_inclusions, inclusion_tolerance, seed = 1) {
  #stopifnot(  )

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

  if ((length(scripts) * av_inclusions %% 2) == 0) {
    pairs_length <- length(scripts) * av_inclusions / 2
  } else if ((length(scripts) * av_inclusions %% 2) == 1) {
  	pairs_length <- (length(scripts) * av_inclusions + 1) / 2
  }

  # form all pairs
  combinations <- data.frame(t(combn(scripts, 2)))   # allow for separation constraint
  combinations[,3] <- 1:dim(combinations)[1]
  pairs_i <- sample(dim(combinations)[1], pairs_length)  #! repeated pairs if pairs_length > dim(combinations)[1]  warn user of repeats
  gp <- combinations[pairs_i, ]

  # generated pairs swapped
  #gps <- swap2(gp = gp, combinations = combinations, av_inclusions = av_inclusions, inclusion_tolerance = inclusion_tolerance)
  #options(stringsAsFactors = stringsAsFactorsOption)
  #attr(gps, "initial_sampling") <- gp
  #gps
}
