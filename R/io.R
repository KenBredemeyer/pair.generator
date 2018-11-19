#' Read in data from file
#'
#' \code{pairs_import.csv} reads in perforance data, and makes the data
#' available for other pair generation functions, such as \code{pairs_generate}.
#'
#' \code{pairs_import.csv} uses \code{read.csv} to read a .csv file, and issues
#' warnings if it detects that the format of the input file is not suitable for
#' subsequent analysis.
#'
#' Warnings will be issued if: 1) the headers in \code{scores_file} are not
#' \code{media}, \code{core}, \code{score}, or 2) the \code{core} column does
#' not contain only 1s and 0s, or 3) if \code{separtion_constraint} is specified
#' but the \code{score} column is not numeric or not complete.
#'
#' \code{pairs_import.csv} ensures that data are not read in as factors, which
#' can cause problems for other functions in the \code{pair.generator} package.
#'
#' @param scores_file Path and ".csv" file name containing the label of the
#'   performance (\code{media}), whether the performance is core (1) or non-core
#'   (0), and the score of the performance, if available.
#'
#' @param separation_constraint Logical.  Are score differences in a pair to be
#'   restricted in pair generation.
#'
#' @return A data frame corresponding to the data read in. An error will be
#'   returned if the headers are not correct in \code{scores_file}, if there are
#'   missing media labels, if \code{core} is not all 0 or 1.  An error will also be
#'   returned if \code{separation_constraint = TRUE} and there are missing or
#'   non-numeric scores.
#'
#' @export
pairs_import.csv <- function(scores_file, separation_constraint = FALSE) {
	input_data <- utils::read.csv(scores_file, stringsAsFactors = FALSE)

  stopifnot(colnames(input_data) == c("media", "core", "score"),
  	        !any(is.na(input_data$media)),
  	        input_data$core == 0 | input_data$core == 1)

  if (separation_constraint) {
  	stopifnot(!any(is.na(input_data$score)),
  		        is.numeric(input_data$score))
  }

  input_data
}


#' Export pairs to .csv
#'
#' \code{pairs_export} selects the variables \code{left}, \code{right} and
#' \code{chain_number} from \code{gp}, and writes to file using
#' \code{write.csv}.
#'
#' @param gp A data frame returned from \code{pairs_generate}, or other pair
#'   generating function.
#' @param file Character string naming a file for writing to.
#'
#' @seealso \code{\link{pairs_generate}}
#' @export
pairs_export <- function(gp, file) {
	x <- gp[ , c("left", "right", "chain_number")]
	utils::write.csv(x, file, row.names = FALSE)
}
