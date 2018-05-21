chain_once <- function(pairs, chain_length) {
	set.seed(1)
	options(stringsAsFactors = FALSE)

	pairs_copy <- data.frame(pairs[ , 1:2])
	pairs_copy[ , 3] <- 1:dim(pairs)[1]
	performance <- unique(unlist(pairs_copy[,1:2]))
	performance <- sample(performance)

	match_i <- list()
	chain_i <- list()

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
		delete_rows <- match(chain_i[[i]], pairs_copy[,3])
		pairs_copy <- pairs_copy[-delete_rows, ]
	}
	chain_indicies <- unlist(chain_i)
}




#' chain pairs
#' @param pairs data.frame containing pairs in columns 1 and 2
#' @param chain_length Integer.
#' @export
chain <- function(pairs, chain_length = 4) {
	labels <- unique(unlist(pairs[ , 1:2]))
	pairs.table <- cbind(pairs[ , 1:2], 1:nrow(pairs))
	chain_rows <- list()
  i = 1
	while (dim(pairs.table)[1] > 0) {
		# chain row indicies
		chain_i <- chain_once(pairs.table, chain_length)
		chain_rows[[i]] <- pairs.table[chain_i, 3]
		pairs.table <- pairs.table[-chain_i, ]
		i <- i + 1
	}
	chains_i <- unlist(chain_rows)
	pairs[chains_i, ]
}
