chain_once <- function(pairs, chain_length, times, n_performances) {
	stringsAsFactorsOption <- getOption("stringsAsFactors")
  options(stringsAsFactors = FALSE)
	pairs_copy <- data.frame(pairs[ , 1:2])
	pairs_copy[ , 3] <- 1:dim(pairs)[1]
	performance <- unique(unlist(pairs_copy[,1:2]))
	performance <- sample(performance)

	match_i <- list()
	chain_i <- list()
	chain_number <- list()

	for (i in seq_along(performance)) {
		x <- which(pairs_copy[ , 1] == performance[i] | pairs_copy[ , 2] == performance[i])
		match_i[[i]] <- pairs_copy[x , 3]
		if (length(match_i[[i]]) > chain_length) {
			chain_i[[i]] <- sample(match_i[[i]], chain_length, replace = FALSE)
		} else if (length(match_i[[i]]) <= chain_length && length(match_i[[i]]) != 0) {
			chain_i[[i]] <- match_i[[i]]
		} else if (length(match_i[[i]]) == 0) {
			next
		}
		chain_number[[i]] <- rep(i + (times - 1)*n_performances, length(chain_i[[i]]))
		delete_rows <- match(chain_i[[i]], pairs_copy[,3])
		pairs_copy <- pairs_copy[-delete_rows, ]
	}
	options(stringsAsFactors = stringsAsFactorsOption)
	chain_numbers <- unlist(chain_number)
	chain_indicies <- unlist(chain_i)
	data.frame(chain_indicies, chain_numbers)
}




#' Chain pairs
#'
#' Re-orders rows of a data frame containing \code{left} and \code{right} pairs,
#' so that a common performance (or cell value) is included in consecutive
#' pairs.
#'
#' @param pairs A data frame containing pairs in columns 1 and 2, such as that
#'   returned from \code{pairs_generate}.  Other columns may also be present.
#' @param chain_length Integer, specifying how many consecutive pairs of
#'   performances/media are to have a common performance.
#'
#' @examples
#' sampled_pairs <- pairs_sample(letters, av_inclusions = 5)
#' chained_pairs <- chain(sampled_pairs, chain_length = 4)
#'
#' @seealso \code{\link{switch_lr}}
#'
#' @export
chain <- function(pairs, chain_length = 4) {
	labels <- unique(unlist(pairs[ , 1:2]))
	n_performances <- length(labels)
	pairs.table <- cbind(pairs[ , 1:2], 1:nrow(pairs))
	chain_rows <- list()
	chain_n <- list()
  j = 1
	while (dim(pairs.table)[1] > 0) {
		# chain row indicies
		chained <- chain_once(pairs.table, chain_length, times = j, n_performances)
		chain_i <- chained[ , 1]
		chain_n[[j]] <- chained[ , 2]
		chain_rows[[j]] <- pairs.table[chain_i, 3]
		pairs.table <- pairs.table[-chain_i, ]
		j <- j + 1
	}
	chains_i <- unlist(chain_rows)
	chain_number <- unlist(chain_n)
	cbind(pairs[chains_i, ], chain_number)
}
