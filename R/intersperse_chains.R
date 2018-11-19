#' Intersperse incomplete chains with full chains.  Use this function if it is
#' desirable to avoid incomplete chains being allocated unevenly among judges
#'
#' Here, \emph{chaining} refers to consecutive pairs having a common performance or
#' item.  Chain length is the specified number of consecutive pairs which
#' contain a common item or performance, and is specified as an argument to the
#' \code{pairs_generate} and \code{chain} functions.  Complete chains are where
#' the number of consecutive pairs which have a common performance is equal to
#' that specified, and incomplete chains have a performance common to a number
#' of consecutive pairs which is less than \code{chain_length}.
#'
#' @param pairs A data frame containing chained pairs, returned from \code{chain}.
#' @param chain_length Integer specifying the number of consecutive pairs to
#'   include a common performance.
#'
#' @return A data frame which is the same as first argument of
#'   \code{intersperse_chains}, but with re-ordered rows.
#'
#' @seealso \code{\link{pairs_generate}}, \code{\link{chain}},
#'   \code{\link{allocate}}
#' @export
intersperse_chains <- function(pairs, chain_length) {
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
  for (i in 1:length(unique(rles_copy$chain_number))) {
  	gp[[i]] <- pairs[pairs$chain_number == rles_copy$chain_number[i], ]
  }
  newpairs <- do.call(rbind, gp)

  #newpairs <- merge(rles_copy, pairs, by = "chain_number", sort = FALSE)  # order not quite right
  newpairs
}
