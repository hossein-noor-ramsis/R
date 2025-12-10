# Poisson Distribution: events in fixed interval with average rate lambda
# Poisson distribution counts how many events happen in a fixed interval
# X: probability of see k event in interval H with average rate event lambda

lambda <- 4.2
k_vals <- 0:15 # adjust max k based on lambda

# PMF for Poisson
pois_pmf <- function(lambda, k) {
  (lambda^k * exp(-lambda)) / factorial(k)
}

# Compute PMF for all k
pmf_vals <- pois_pmf(lambda, k_vals)

# Plot PMF
plot(k_vals, pmf_vals,
  type = "h", lwd = 5, col = "salmon",
  xlab = "Number of events k",
  ylab = "P(X = k)",
  main = paste("Poisson PMF (λ =", lambda, ")")
)
points(k_vals, pmf_vals, col = "red", pch = 19)

# Theoretical mean and variance (equal for Poisson)
expected_x <- lambda
variance_x <- lambda

cat("Expected value:", expected_x, "\n")
cat("Variance:", variance_x, "\n")

# Simulation
set.seed(123)
sim_samples <- rpois(n = 1000, lambda = lambda)

# Get frequency distribution
sim_counts <- table(factor(sim_samples, levels = k_vals))
sim_probs <- as.numeric(sim_counts) / length(sim_samples)

# Simulation plot
plot(k_vals, sim_probs,
  type = "h", lwd = 5, col = "lightblue",
  xlab = "Number of events k",
  ylab = "P(X = k)",
  main = paste("Poisson Simulation (λ =", lambda, ")")
)
points(k_vals, sim_probs, col = "blue", pch = 19)

cat("\nSimulation results (1000 samples):\n")
cat("Sample mean:", mean(sim_samples), "\n")
cat("Sample variance:", var(sim_samples), "\n")
