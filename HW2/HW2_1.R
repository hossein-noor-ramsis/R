# -----------------------------
# 1) Parameters
# -----------------------------
p_object <- 0.05
p_not_object <- 1 - p_object
p_alarm_given_object <- 0.90
p_alarm_given_not_object <- 0.08

# -----------------------------
# 2) Analytical results
# -----------------------------
p_alarm <- p_object * p_alarm_given_object + p_not_object * p_alarm_given_not_object
print(p_alarm)

p_object_given_alarm <- (p_object * p_alarm_given_object) / (p_object * p_alarm_given_object + p_not_object * p_alarm_given_not_object)
print(p_object_given_alarm)

# -----------------------------
# 3) Simulation (Empirical)
# -----------------------------
N <- 100000

# Step 1: simulate whether each passenger has the object
object_vec <- rbinom(N, 1, p_object)

# Step 2: simulate the alarm conditional on object presence
u <- runif(N)

alarm_vec <- integer(N)
for (i in 1:N) {
  if (object_vec[i] == 1) {
    if (u[i] < p_alarm_given_object) {
      alarm_vec[i] <- 1
    } else {
      alarm_vec[i] <- 0
    }
  } else {
    if (u[i] < p_alarm_given_not_object) {
      alarm_vec[i] <- 1
    } else {
      alarm_vec[i] <- 0
    }
  }
}

# Step 3: empirical estimates
num_object_and_alarm <- sum(object_vec == 1 & alarm_vec == 1)
num_alarm <- sum(alarm_vec == 1)

p_alarm_empirical <- mean(alarm_vec)
p_object_given_alarm_empirical <- num_object_and_alarm / num_alarm

cat("Empirical results (N =", N, "):\n")
cat("  P(Alarm) =", p_alarm_empirical, "\n")
cat("  P(Object | Alarm) =", p_object_given_alarm_empirical, "\n\n")

# -----------------------------
# 4) Bar chart: Analytical vs Empirical
# ----------------------------
values <- rbind(
  c(p_alarm, p_object_given_alarm),
  c(p_alarm_empirical, p_object_given_alarm_empirical)
)

rownames(values) <- c("Analytical", "Empirical")
colnames(values) <- c("P(Alarm)", "P(Object|Alarm)")

barplot(values,
  beside = TRUE,
  col = c("skyblue", "salmon"),
  ylim = c(0, 1),
  main = "Analytical vs Empirical",
  ylab = "Probability",
  legend.text = TRUE
)

# -----------------------------
# 5) Bar chart: Analytical vs Empirical
# ----------------------------
sample_sizes <- c(10, 100, 500, 1000, 5000, 10000, 50000, 100000)
convergence_data <- c()

for (i in 1:length(sample_sizes)) {
  n <- sample_sizes[i]

  object_vec <- rbinom(n, 1, p_object)

  u <- runif(n)

  alarm_vec <- integer(n)
  for (j in 1:n) {
    if (object_vec[j] == 1) {
      if (u[j] < p_alarm_given_object) {
        alarm_vec[j] <- 1
      } else {
        alarm_vec[j] <- 0
      }
    } else {
      if (u[j] < p_alarm_given_not_object) {
        alarm_vec[j] <- 1
      } else {
        alarm_vec[j] <- 0
      }
    }
  }

  num_object_and_alarm <- sum(object_vec == 1 & alarm_vec == 1)
  num_alarm <- sum(alarm_vec == 1)

  p_alarm_empirical <- mean(alarm_vec)
  p_object_given_alarm_empirical <- num_object_and_alarm / num_alarm

  if (num_alarm > 0) {
    convergence_data[i] <- num_object_and_alarm / num_alarm
  } else {
    convergence_data[i] <- NA
  }
}

# Plot convergence
plot(sample_sizes, convergence_data,
  type = "l",
  log = "x",
  col = "blue",
  pch = 16,
  lwd = 2,
  xlab = "Sample Size",
  ylab = "Empirical P(Object|Alarm)",
  main = "Convergence to Theoretical Value"
)

abline(h = p_object_given_alarm, col = "red", lty = 2, lwd = 2)

legend("bottomright",
  legend = c("Empirical", paste("Theoretical =", round(p_object_given_alarm, 4))),
  col = c("blue", "red"), lty = c(1, 2), lwd = 2
)
