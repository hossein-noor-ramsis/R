# Bernoulli Distribution: single trial with success probability p
# X: probability of see success event with prob p after 1 experiment

p <- 0.6
k_vals <- 0:1

# PMF for Bernoulli
bern_pmf <- function(p, k) {
  ifelse(k == 1, p, 1 - p)
}

# Compute PMF for all k
pmf_vals <- bern_pmf(p, k_vals)

# Plot PMF
plot(k_vals, pmf_vals,
  type = "h", lwd = 10, col = "salmon",
  xlab = "Success (1) or Failure (0)",
  ylab = "P(X = k)",
  main = paste("Bernoulli PMF (p =", p, ")"),
  xlim = c(-0.5, 1.5),
  ylim = c(0, 1)
)
points(k_vals, pmf_vals, col = "red", pch = 19, cex = 1.5)

# Theoretical mean and variance
expected_x <- p
variance_x <- p * (1 - p)

cat("Expected value:", expected_x, "\n")
cat("Variance:", variance_x, "\n")

# Simulation
set.seed(123)
sim_samples <- rbinom(n = 1000, size = 1, prob = p)

# Get frequency distribution
sim_counts <- table(factor(sim_samples, levels = k_vals))
sim_probs <- as.numeric(sim_counts) / length(sim_samples)

# Simulation plot
plot(k_vals, sim_probs,
  type = "h", lwd = 10, col = "lightblue",
  xlab = "Success (1) or Failure (0)",
  ylab = "P(X = k)",
  main = paste("Bernoulli Simulation (p =", p, ")"),
  xlim = c(-0.5, 1.5),
  ylim = c(0, 1)
)
points(k_vals, sim_probs, col = "blue", pch = 19, cex = 1.5)

cat("\nSimulation results (1000 samples):\n")
cat("Sample mean:", mean(sim_samples), "\n")
cat("Sample variance:", var(sim_samples), "\n")
