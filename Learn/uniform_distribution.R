# Uniform Distribution: continuous distribution between a and b
# X: probability of see x in a uniform distribution

a <- 2
b <- 8
x_vals <- seq(a, b, length.out = 100)

# PDF for Uniform
unif_pdf <- function(a, b, x) {
  ifelse(x >= a & x <= b, 1 / (b - a), 0)
}

# Compute PDF for all x
pdf_vals <- unif_pdf(a, b, x_vals)

# Plot PDF
plot(x_vals, pdf_vals,
  type = "l", lwd = 3, col = "salmon",
  xlab = "x",
  ylab = "f(x)",
  main = paste("Uniform PDF (a =", a, ", b =", b, ")")
)
abline(h = 0, col = "gray")

# Theoretical mean and variance
expected_x <- (a + b) / 2
variance_x <- (b - a)^2 / 12

cat("Expected value:", expected_x, "\n")
cat("Variance:", variance_x, "\n")

# Simulation
set.seed(123)
sim_samples <- runif(n = 1000, min = a, max = b)

# Get density estimation (instead of frequency for continuous)
sim_density <- density(sim_samples)

# Simulation plot
hist(sim_samples,
  freq = FALSE,
  col = "lightblue",
  xlab = "x",
  ylab = "Density",
  main = paste("Uniform Simulation (a =", a, ", b =", b, ")")
)
lines(sim_density, col = "blue", lwd = 2)

cat("\nSimulation results (1000 samples):\n")
cat("Sample mean:", mean(sim_samples), "\n")
cat("Sample variance:", var(sim_samples), "\n")

# CDF Plot (Cumulative Distribution Function)

# Theoretical CDF
unif_cdf <- function(a, b, x) {
  ifelse(x < a, 0,
    ifelse(x > b, 1,
      (x - a) / (b - a)
    )
  )
}
cdf_vals <- unif_cdf(a, b, x_vals)

# Empirical CDF from simulation
ecdf_vals <- ecdf(sim_samples)

# Create CDF plot
plot(x_vals, cdf_vals,
  type = "l", lwd = 3, col = "darkgreen",
  xlab = "x",
  ylab = "Cumulative Probability F(x)",
  main = paste("Uniform CDF (a =", a, ", b =", b, ")"),
  ylim = c(0, 1)
)

# Add empirical CDF (step function)
plot(ecdf_vals,
  col = "orange", lwd = 2,
  add = TRUE, verticals = TRUE, do.points = FALSE
)

# Print some CDF values
cat("\nCDF values at selected points:\n")
selected_x <- seq(a, b, length.out = 5)
for (x in selected_x) {
  cat("P(X ≤", round(x, 2), ") =", round(punif(x, a, b), 4), "\n")
}
