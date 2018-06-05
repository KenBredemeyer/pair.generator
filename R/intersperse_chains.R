#' intersperse incomplete chains with full chains
#'
#' @param pairs data.frame containing chained pairs, returned from \code{chain}.
#' @param chain_length Integer.
#' @export
intersperse_chains <- function(pairs, chain_length, seed = 1) {
  set.seed(seed)
  rles <- rle(pairs$chain_number)
  rles <- data.frame(chain_number = rles$values, len = rles$lengths)
  incomplete_i <- which(rles[,2] < chain_length)
  complete_i <- which(rles[,2] == chain_length)
  put_i <- sample(sample(1:dim(rles)[1]), length(incomplete_i))
  # move incomplete_i to put_i
  rles_copy <- rles     # rles_new
  rles_copy[put_i, ] <- rles[incomplete_i, ]
  not_put_i <- setdiff(1:dim(rles)[1], put_i)
  rles_copy[not_put_i, ] <- rles[complete_i, ]

  # rles to pais
  gp <- list()
  for (i in seq_along(pairs$chain_number)) {
  	gp[[i]] <- pairs[pairs$chain_number == rles_copy$chain_number[i], ]
  }
  newpairs <- do.call(rbind, gp)

  #newpairs <- merge(rles_copy, pairs, by = "chain_number", sort = FALSE)  # order not quite right
  newpairs
}
