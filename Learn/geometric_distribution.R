# Geometric Distribution: prob of 1 success after k experiment
# X: probability of see the success for the first time after repeating a Bernoulli experiment for k times;

p <- 0.3

# p: success prob, k: after k time experiment;
PMF <- function(p, k) {
  return((1 - p)^(k - 1) * p)
}

k <- 1:20
plot(k, PMF(p, k))

k <- 1:20000
expected_x <- sum(k * PMF(p, k)) # 1 / p
print(expected_x)

variance_x <- sum((k - 1 / p)^2 * PMF(p, k)) # (1 - p) / p^2
print(variance_x)

k <- 1000
sample <- rgeom(k, p) + 1
print(head(sample))
print(mean(sample))
print(var(sample))
hist(sample)
plot(1:k, sample)
