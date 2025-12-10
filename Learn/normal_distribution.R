# Normal Distribution: continuous distribution with mean mu and standard deviation sigma
# X: probablity of see x in a noraml distribution

mu <- 0
sigma <- 1
x_vals <- seq(mu - 4 * sigma, mu + 4 * sigma, length.out = 100)

# PDF for Normal
norm_pdf <- function(mu, sigma, x) {
  (1 / (sigma * sqrt(2 * pi))) * exp(-(1 / 2) * ((x - mu) / sigma)^2)
}

# Compute PDF for all x
pdf_vals <- norm_pdf(mu, sigma, x_vals)
pdf_vals <- dnorm(x_vals, mu, sigma)
ح

# Plot PDF
plot(x_vals, pdf_vals,
  type = "l", lwd = 3, col = "salmon",
  xlab = "x",
  ylab = "f(x)",
  main = paste("Normal PDF (μ =", mu, ", σ =", sigma, ")")
)
abline(h = 0, col = "gray")

# Theoretical mean and variance
expected_x <- mu
variance_x <- sigma^2

cat("Expected value:", expected_x, "\n")
cat("Variance:", variance_x, "\n")

# Simulation
set.seed(123)
sim_samples <- rnorm(n = 1000, mean = mu, sd = sigma)

# Get density estimation (instead of frequency for continuous)
sim_density <- density(sim_samples)

# Simulation plot
hist(sim_samples,
  freq = FALSE,
  col = "lightblue",
  xlab = "x",
  ylab = "Density",
  main = paste("Normal Simulation (μ =", mu, ", σ =", sigma, ")")
)
lines(sim_density, col = "blue", lwd = 2)
curve(dnorm(x, mean = mu, sd = sigma),
  add = TRUE, col = "red", lwd = 2, lty = 2
)

cat("\nSimulation results (1000 samples):\n")
cat("Sample mean:", mean(sim_samples), "\n")
cat("Sample variance:", var(sim_samples), "\n")

# CDF Plot (Cumulative Distribution Function)

# Theoretical CDF
norm_cdf <- function(mu, sigma, x) {
  pnorm(x, mean = mu, sd = sigma)
}

cdf_vals <- norm_cdf(mu, sigma, x_vals)

# Empirical CDF from simulation
ecdf_vals <- ecdf(sim_samples)

# Create CDF plot
plot(x_vals, cdf_vals,
  type = "l", lwd = 3, col = "darkgreen",
  xlab = "x",
  ylab = "Cumulative Probability F(x)",
  main = paste("Normal CDF (μ =", mu, ", σ =", sigma, ")"),
  ylim = c(0, 1)
)

# Add empirical CDF (step function)
plot(ecdf_vals,
  col = "orange", lwd = 2,
  add = TRUE, verticals = TRUE, do.points = FALSE
)

# Print some CDF values
cat("\nCDF values at selected points:\n")
selected_x <- seq(mu - 2 * sigma, mu + 2 * sigma, length.out = 5)
for (x in selected_x) {
  cat("P(X ≤", round(x, 2), ") =", round(pnorm(x, mu, sigma), 4), "\n")
}
