# Exponential Distribution: continuous distribution with rate lambda
# X: probability of see an event with average rate event lambda after minimum x time

lambda <- 0.5
x_vals <- seq(0, 10 / lambda, length.out = 100) # Go up to 10 times the mean

# PDF for Exponential
exp_pdf <- function(lambda, x) {
  ifelse(x >= 0, lambda * exp(-lambda * x), 0)
}

# Compute PDF for all x
pdf_vals <- exp_pdf(lambda, x_vals)

# Plot PDF
plot(x_vals, pdf_vals,
  type = "l", lwd = 3, col = "salmon",
  xlab = "x",
  ylab = "f(x)",
  main = paste("Exponential PDF (λ =", lambda, ")")
)
abline(h = 0, col = "gray")

# Theoretical mean and variance
expected_x <- 1 / lambda
variance_x <- 1 / (lambda^2)

cat("Expected value:", expected_x, "\n")
cat("Variance:", variance_x, "\n")

# Simulation
set.seed(123)
sim_samples <- rexp(n = 1000, rate = lambda)

# Get density estimation (instead of frequency for continuous)
sim_density <- density(sim_samples)

# Simulation plot
hist(sim_samples,
  freq = FALSE,
  col = "lightblue",
  xlab = "x",
  ylab = "Density",
  main = paste("Exponential Simulation (λ =", lambda, ")"),
  xlim = c(0, max(sim_samples) * 0.8) # Trim extreme tail for better visualization
)

lines(sim_density, col = "blue", lwd = 2)
curve(dexp(x, rate = lambda),
  add = TRUE, col = "red", lwd = 2, lty = 2
)

cat("\nSimulation results (1000 samples):\n")
cat("Sample mean:", mean(sim_samples), "\n")
cat("Sample variance:", var(sim_samples), "\n")

# CDF Plot (Cumulative Distribution Function)

# Theoretical CDF
exp_cdf <- function(lambda, x) {
  ifelse(x >= 0, 1 - exp(-lambda * x), 0)
}
cdf_vals <- exp_cdf(lambda, x_vals)

# Empirical CDF from simulation
ecdf_vals <- ecdf(sim_samples)

# Create CDF plot
plot(x_vals, cdf_vals,
  type = "l", lwd = 3, col = "darkgreen",
  xlab = "x",
  ylab = "Cumulative Probability F(x)",
  main = paste("Exponential CDF (λ =", lambda, ")"),
  ylim = c(0, 1)
)

# Add empirical CDF (step function)
plot(ecdf_vals,
  col = "orange", lwd = 2,
  add = TRUE, verticals = TRUE, do.points = FALSE
)

# Print some CDF values
cat("\nCDF values at selected points:\n")
selected_x <- c(0.5, 1, 2, 3, 5) # Common points for exponential
for (x in selected_x) {
  cat("P(X ≤", round(x, 2), ") =", round(pexp(x, rate = lambda), 4), "\n")
}

# Additional: Show memoryless property example
cat("\nMemoryless property example:\n")
t <- 2
s <- 3
cat("P(X >", t + s, " | X >", t, ") = P(X >", s, ")\n")
cat("Left side:", (1 - pexp(t + s, rate = lambda)) / (1 - pexp(t, rate = lambda)), "\n")
cat("Right side:", 1 - pexp(s, rate = lambda), "\n")
