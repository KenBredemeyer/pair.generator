# sample without replacement multiple times and combine
sampler <- function(scripts, s1_size) {
  if (s1_size > length(scripts)) {
    s1_reps <- floor(s1_size / length(scripts))
    s1_rem <- s1_size %% length(scripts)
    s1 <- list()
    for (i in 1:s1_reps) {
      s1[[i]] <- sample(scripts, size = length(scripts), replace = FALSE)
    }
    s1[[i+1]] <- sample(scripts, size = s1_rem, replace = FALSE)
  s1 <- do.call(c, s1)
  } else s1 <- sample(scripts, s1_size, replace = FALSE)
}

#' Randomize left/right presentation
#'
#' This function switches the value of the column 1 with the value of column 2,
#' for random row numbers.
#'
#' @param pairs Data frame containing paired performances in columns 1 and 2.
#' @examples
#' pairs <- data.frame(left = letters[1:4], right = 1:4, stringsAsFactors = FALSE)
#' switch_lr(pairs)
#' @export
switch_lr <- function(pairs) {
  switch_index <- sort(sample(1:nrow(pairs), nrow(pairs) / 2))
  temp_copy <- pairs
  temp_copy[switch_index, 1] <- pairs[switch_index, 2]
  temp_copy[switch_index, 2] <- pairs[switch_index, 1]
  pairs <- temp_copy
  pairs
}



#' add combination number to a set of pairs
#'
#' for using pairs_generate_cnc with duplicates and pairs_plot
combination <- function(pairs, media) {
 	if ("media" %in% names(media)) {
 		media_names <- media["media"]
 	} else if (is.vector(media)) {
  	media_names <- media
  } else {
  	stop("media arg must be a vector of names, or contain the 'media' variable as a vector of names")
  }
	pairs_lr <- pairs[ , c("left", "right")]
  combinations <- data.frame(t(utils::combn(media_names, 2)))   # allow for separation constraint
  colnames(combinations) <- c("left", "right")
  # forwards match
  match1 <- plyr::match_df(combinations, pairs_lr)
  # backwards match
  colnames(pairs_lr) <- colnames(pairs_lr)[2:1]
  match2 <- plyr::match_df(combinations, pairs_lr)
  matches <- rbind(match1, match2)
  c_index <- as.numeric(rownames(matches))
  # put c_index in the right order   *!
  rev_i_i <- as.numeric(rownames(plyr::match_df(pairs_lr, combinations)))
  colnames(pairs_lr) <- colnames(pairs_lr)[2:1]
  i_i <- as.numeric(rownames(plyr::match_df(pairs_lr, combinations)))

  cbind(pairs, c_index)
}

