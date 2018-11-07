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
#' @param x data frame containing paired performances in columns 1 and 2.
#' @examples
#' pairs <- data.frame(left = letters[1:4], right = 1:4, stringsAsFactors = FALSE)
#' switch_lr(pairs)
#' @export
switch_lr <- function(x) {
  switch_index <- sort(sample(1:nrow(x), nrow(x) / 2))
  temp_copy <- x
  temp_copy[switch_index, 1] <- x[switch_index, 2]
  temp_copy[switch_index, 2] <- x[switch_index, 1]
  x <- temp_copy
  x
}
