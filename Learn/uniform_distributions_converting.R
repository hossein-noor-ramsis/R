r_bernouli <- function(n, p) {
  result <- ifelse(runif(n) < 1 - p, 0, 1)

  return(result)
}

binomial_sample <- r_bernouli(20, 0.5)
print(binomial_sample)
