##### Evolutionary Optimization with R (evolutionR)
##### (c)2013-2014 Ronald Hochreiter <ron@hochreiter.net>
##### http://www.evolutionR.org/

mutate.random.uniform <- function(input, n=1, min=0, max=1) {
  output <- input
  output[sample(length(output), n)] <- runif(n, min, max)
  return(list(TRUE, output))
}
