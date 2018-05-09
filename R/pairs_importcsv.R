#' read in data
#'
#' \code{pairs_import.csv} reads in perforance data, and makes the data available for
#' other pairwise analysis functions.
#'
#' \code{pairs_import.csv} uses \code{read.csv} to read a .csv file, and issues warnings
#' if it detects that the format of the input file is not suitable for
#' subsequent analysis.
#'
#' Warnings will be issued if: 1) the headers in \code{scores_file} are not
#' "media", "core", "score", or 2) if \code{choose_coreNcore = TRUE} and the
#' "core" column does not contain only 1s and 0s, or 3) if
#' \code{separtion_constraint} is specified but the "score" column is not
#' numeric or not complete.
#'
#' @param scores_file a path and .csv file containing the label of the
#'   performance ("media"), whether the performance is core (1) or non-core (2),
#'   and the score of the performance, if available.
#'
#' @param choose_core logical.  Is the core column to be used?
#' @export
pairs_import.csv <- function(scores_file, separation_constraint = NULL) {
	input_data <<- read.csv(scores_file, stringsAsFactors = FALSE)
	# if no score column, or if any missing in score column, cannot use separation constraint.

	# check headers are correct
	if (colnames(input_data)[1] != "media" |
	    colnames(input_data)[2] != "core" |
	    colnames(input_data)[3] != "score")
	  stop("header for input data must be: 'media', 'core', 'score'.  Please check the input file")

	# set 'core non-core' to true or false  ## just check cnc is ok??
	if (all(input_data[,2] == 0 | input_data[,2] == 1) & choose_coreNcore) {
		coreNcore <- TRUE
	} else {
		coreNcore <- FALSE
		warning("core column not correct")
	}

	# check data o.k. for separation constraint
	if (is.numeric(separation_constraint) & all(!is.na(input_data[,3])) & is.numeric(input_data[,3])) {
	  apply_separation_constraint <- TRUE
	} else apply_separation_constraint <- FALSE     ## please include warning if is.numeric(separation_constraint), but input_data not o.k.
}


#' Export pairs to .csv
#'
#' \code{pairs_export} selects the variables 'left', 'right', 'chain_number' and
#' writes to file using \code{write.csv}.
#'
#' @param gp data.frame returned from \code{pairs_make}
#' @param file Character string naming a file for writing
#' @export
pairs_export <- function(gp, file) {
	x <- gp[ , c("left", "right", "chain_number")]
	write.csv(x, file, row.names = FALSE)
}
