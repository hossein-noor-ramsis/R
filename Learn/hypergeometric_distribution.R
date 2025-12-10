# Hypergeometric Distribution: prob of k success after n time experiment
# X: probability of see white balls for k times when takeout n balls from N balls (N: m white ball + N - m black ball)
# experiment n depends on experiment n - 1

# Hypergeometric Distribution

N <- 100 # total balls
m <- 20 # red balls (success states)
n <- 10 # sample size drawn
k_vals <- 0:n # CHANGED: 0:n not 0:m

# PMF for hypergeometric
hyper_pmf <- function(N, m, n, k) {
  choose(m, k) * choose(N - m, n - k) / choose(N, n)
}

# Compute PMF for all k
pmf_vals <- hyper_pmf(N = N, m = m, n = n, k = k_vals)

# Plot PMF
plot(k_vals, pmf_vals,
  type = "h", lwd = 5, col = "salmon",
  xlab = "Number of successes k", ylab = "P(X = k)"
)

points(k_vals, pmf_vals, col = "red", pch = 19)

# Theoretical mean and variance
expected_x <- n * m / N
variance_x <- n * (m / N) * ((N - m) / N) * ((N - n) / (N - 1))

cat("Expected value:", expected_x, "\n")
cat("Variance:", variance_x, "\n")

# Simulation
set.seed(123)
sim_samples <- rhyper(nn = 1000, m = m, n = N - m, k = n)

# Get frequency distribution
sim_counts <- table(factor(sim_samples, levels = k_vals))
sim_probs <- as.numeric(sim_counts) / length(sim_samples)

# Add simulation to existing plot (same style)
plot(k_vals, sim_probs,
  type = "h", lwd = 5, col = "lightblue",
  xlab = "Number of successes k", ylab = "P(X = k)",
)
points(k_vals, sim_probs, col = "blue", pch = 19)

cat("\nSimulation results (1000 samples):\n")
cat("Sample mean:", mean(sim_samples), "\n")
cat("Sample variance:", var(sim_samples), "\n")
