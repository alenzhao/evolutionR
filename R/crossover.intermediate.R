##### Evolutionary Optimization with R (evolutionR)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.evolutionR.org/

crossover.intermediate <- function(parent1, parent2, prob=0.5) {
  # verify input
  if ( (prob < 0) || (prob > 1) ) { prob <- NA }
  if (length(parent1) != length(parent2)) { return(list(FALSE, "Length of parent chromosomes differs")) }
  
  # compute offsprings
  if (is.na(prob)) { p <- runif(length(parent1)) }
  else { p <- rep(prob, length(parent1)) }
  offspring1 <- p * parent1 + (1-p) * parent2
  offspring2 <- p * parent2 + (1-p) * parent1

  return(list(TRUE, list(offspring1, offspring2)))
}
