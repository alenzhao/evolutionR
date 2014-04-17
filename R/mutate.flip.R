##### Evolutionary Optimization with R (evolutionR)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.evolutionR.org/

mutate.flip <- function(input, n=1, min=NA, max=NA) {
  if (is.na(min)) { min <- min(input) }
  if (is.na(max)) { max <- max(input) }
  
  if (is.na(n)) { n <- sample(length(input), 1) }
  if (n < 1) { n <- 1 }
  if (n > length(input)) { n <- length(input) }

  pos <- sample(length(input), n)
  output <- input
  output[pos] <- (max-min) - abs(min - input[pos])

  return(list(TRUE, output))
}
