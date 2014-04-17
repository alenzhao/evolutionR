##### Evolutionary Optimization with R (evolutionR)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.evolutionR.org/

crossover.npoint <- function(parent1, parent2, n=1) {
  # verify input
  if (n < 1) { n <- 1 }
  if (length(parent1) != length(parent2)) { return(list(FALSE, "Length of parent chromosomes differs")) }

  # compute offspring
  crossover <- c(1, sort(sample(2:(length(parent1)-1), n)), length(parent1)+1)

#   # MatLab coverted code (legacy)
#   cross <- rep(1, crossover[2]) 
#   if (n > 1) for(i in 2:n) cross <- c(cross, rep(i, crossover[i+1]-crossover[i]))  
#   if (length(cross) < length(parent1)) cross <- c(cross, rep(n+1, length(parent1)-length(cross)))
  
  # R code
  cross <- unlist(apply(matrix(c(crossover[2:(n+2)]-crossover[1:(n+1)], seq(1:(n+1))), nrow=n+1, byrow=FALSE), 1, function(x) rep(x[2],x[1])))
  
  p <- (cross %% 2)
  offspring1 <- p * parent1 + (1-p) * parent2
  offspring2 <- p * parent2 + (1-p) * parent1

  return(list(TRUE, offspring1, offspring2))
}
