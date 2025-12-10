# Binomial Distribution: n independent Bernoulli trials
# X: probability of see success for k times when repeat a Bernoulli experiment for n times

n <- 20
p <- 0.4
k_vals <- 0:n

# PMF for Binomial
binom_pmf <- function(n, p, k) {
  choose(n, k) * p^k * (1 - p)^(n - k)
}

# Compute PMF for all k
pmf_vals <- binom_pmf(n, p, k_vals)

# Plot PMF
plot(k_vals, pmf_vals,
  type = "h", lwd = 5, col = "salmon",
  xlab = "Number of successes k",
  ylab = "P(X = k)",
  main = paste("Binomial PMF (n =", n, ", p =", p, ")")
)
points(k_vals, pmf_vals, col = "red", pch = 19)

# Theoretical mean and variance
expected_x <- n * p
variance_x <- n * p * (1 - p)

cat("Expected value:", expected_x, "\n")
cat("Variance:", variance_x, "\n")

# Simulation
set.seed(123)
sim_samples <- rbinom(n = 1000, size = n, prob = p)

# Get frequency distribution
sim_counts <- table(factor(sim_samples, levels = k_vals))
sim_probs <- as.numeric(sim_counts) / length(sim_samples)

# Simulation plot
plot(k_vals, sim_probs,
  type = "h", lwd = 5, col = "lightblue",
  xlab = "Number of successes k",
  ylab = "P(X = k)",
  main = paste("Binomial Simulation (n =", n, ", p =", p, ")")
)
points(k_vals, sim_probs, col = "blue", pch = 19)

cat("\nSimulation results (1000 samples):\n")
cat("Sample mean:", mean(sim_samples), "\n")
cat("Sample variance:", var(sim_samples), "\n")

# CDF Plot (Cumulative Distribution Function)

# Theoretical CDF
cdf_vals <- cumsum(pmf_vals)

# Empirical CDF from simulation
ecdf_vals <- ecdf(sim_samples)

# Create CDF plot
plot(k_vals, cdf_vals,
  type = "s", lwd = 3, col = "darkgreen",
  xlab = "Number of successes k",
  ylab = "Cumulative Probability F(k)",
  main = paste("Binomial CDF (n =", n, ", p =", p, ")"),
  ylim = c(0, 1)
)

# Add empirical CDF (step function)
plot(ecdf_vals,
  col = "orange", lwd = 2,
  add = TRUE, verticals = TRUE, do.points = FALSE
)

# Add legend
# legend("topleft",
#   legend = c("Theoretical CDF", "Empirical CDF (simulation)"),
#   col = c("darkgreen", "orange"),
#   lwd = 3
# )

# Mark key probabilities
# abline(h = c(0.25, 0.5, 0.75), lty = 2, col = "gray")
# abline(v = expected_x, lty = 2, col = "red")
# text(expected_x, 0.1, paste("Mean =", round(expected_x, 2)),
#   col = "red", pos = 4
# )

# Print some CDF values
cat("\nCDF values at selected points:\n")
for (k in c(5, 8, 10, 12)) {
  cat("P(X ≤", k, ") =", round(pbinom(k, n, p), 4), "\n")
}
