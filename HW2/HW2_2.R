Ns <- c(1000, 5000, 10000, 50000, 100000, 200000)
repeats <- 30
mean_vals <- numeric(length(Ns))
sd_vals <- numeric(length(Ns))

for (i in seq_along(Ns)) {
  n <- Ns[i]
  probs <- numeric(repeats)
  for (r in 1:repeats) {
    df_i <- simulate_dataset(n)
    subset_i <- subset(df_i, OddCountry == 1 & LargeAmount == 1)
    probs[r] <- ifelse(nrow(subset_i) > 0, mean(subset_i$Fraud), NA)
  }
  mean_vals[i] <- mean(probs, na.rm = TRUE)
  sd_vals[i] <- sd(probs, na.rm = TRUE)
}

# Base R plot
plot(Ns, mean_vals,
  type = "b",
  log = "x",
  pch = 19,
  col = "blue",
  ylim = range(mean_vals - sd_vals, mean_vals + sd_vals),
  main = "Convergence of Empirical P(Fraud | Odd & Large)",
  xlab = "Sample Size (log scale)", ylab = "Empirical Probability"
)
arrows(Ns, mean_vals - sd_vals, Ns, mean_vals + sd_vals,
  angle = 90, code = 3, length = 0.05, col = "gray40"
)
abline(h = p_fraud_given_odd_large, col = "red", lty = 2)
legend("bottomright",
  legend = c("Empirical mean ± SD", "Theoretical"),
  col = c("blue", "red"), lty = c(1, 2), pch = c(19, NA), bty = "n"
)
