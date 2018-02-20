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

# randomize left/right presentation
switch_lr <- function(x) {
  switch_index <- sort(sample(1:nrow(x), nrow(x) / 2))
  temp_copy <- x
  temp_copy[switch_index, 1] <- x[switch_index, 2]
  temp_copy[switch_index, 2] <- x[switch_index, 1]
  x <- temp_copy
}
