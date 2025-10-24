roll_dice <- function(n) {
  # TODO: simulate rolling a dice n times.################
  dice <- seq(1, 6)
  propOfDice <- rep(1 / 6, 6)

  simulateResult <- sample(dice, n, replace = TRUE, propOfDice)

  return(simulateResult)
}

# print(roll_dice(10))

# You should create a data frame and store the probabilities of the desired sums in it.
# Each row of this data frame should represent the probabilities for one sample size.
# Print the data frame.
# TODO#############################################################
sum_of_n_dices <- function(n, k) {
  # This function returns a list of k values obtained by summing the rolls of n dices
  # TODO###########################################################
  x <- replicate(k, roll_dice(n), simplify = F)
  result <- list()

  for (i in 1:length(x)) {
    result[i] <- sum(unlist(x[i]))
  }

  return(result)
}

# print(sum_of_n_dices(3, 10))
#

sample_sizes <- 10^(2:6)

simulate1 <- function(sampleSizes) {
  probabilities <- c()

  result <- list()
  col <- list()
  df <- data.frame()

  for (size in sampleSizes) {
    simData <- unlist(sum_of_n_dices(3, size))

    for (num in 9:12) {
      filteredData <- simData[simData == num]
      count <- length(filteredData)

      probability <- count / size

      col[paste(num)] <- probability
      result[[paste(size)]] <- col
    }
  }

  df <- as.data.frame(result)
  return(result)
}

simResult <- simulate1(sample_sizes)

##################################################################
plot_probabilities <- function(names, probs) {
  # This function takes a list of labels and their corresponding probabilities
  # and creates a bar chart.
  # TODO###########################################################
  barplot(probs, names)
  ################################################################
}

plot_probabilities(c(9:12), unlist(simResult$`1e+06`))
# TODO: plot the probabilities for sample size of 10^6 in results df
##################################################################
# plot_probabilities(c(9:12), unlist(simResult$`1e+06`))
##################################################################

sample_sizes <- c(
  seq(100, 1000, 200),
  seq(1000, 10000, 2000),
  seq(10000, 100000, 20000),
  seq(100000, 1000000, 200000)
)

# TODO: Calculate probabilities for different sample sizes.#######
simulate2 <- function(sampleSizes) {
  probabilities <- c()

  result <- list()
  df <- data.frame()

  p9 <- c()
  p10 <- c()

  for (size in sampleSizes) {
    simData <- unlist(sum_of_n_dices(3, size))

    filteredData <- simData[simData == 9]
    count <- length(filteredData)

    probability <- count / size

    p9 <- c(p9, probability)


    filteredData <- simData[simData == 10]
    count <- length(filteredData)

    probability <- count / size

    p10 <- c(p10, probability)
  }

  result$`9` <- p9
  result$`10` <- p10

  df <- as.data.frame(result)
  return(result)
}

simResult <- simulate2(sample_sizes)
##############################################################

# TODO: Create a line chart based on probabilities in the previous cell.
##################################################################
plot(sample_sizes, unlist(simResult$`9`),
  type = "l", # Line type
  col = "blue",
  lwd = 2,
  xlab = "Sample Size",
  ylab = "Probability",
) # Set y-axis to fit both lines

# Add second line
lines(sample_sizes, unlist(simResult$`10`),
  type = "l",
  col = "red",
  lwd = 2,
)

# Add legend
legend("topright",
  legend = c("9", "10"),
  col = c("blue", "red"),
  lwd = 2,
  lty = c(1, 1), # Solid and dashed lines
  bty = "n"
) # No box around legend
##################################################################
sum_of_n_dices <- function(n, k) {
  # This function returns a list of k values obtained by summing the rolls of n dices
  # TODO###########################################################
  x <- replicate(k, roll_dice(n), simplify = F)
  result <- list()

  for (i in 1:length(x)) {
    result[i] <- sum(unlist(x[i]))
  }

  return(result)
}

plot_distribution_of_sum_of_n_dices <- function(n, k) {
  # This function simulates rolling n dice for k experiments and plots the
  # resulting probability distribution of their sums using a bar chart.
  # TODO#########################################################
  probabilities <- c()
  result <- c()
  df <- data.frame()

  simData <- unlist(sum_of_n_dices(n, k))

  for (num in n:(n * 6)) {
    filteredData <- simData[simData == num]
    count <- length(filteredData)

    probability <- count / k

    result[paste(num)] <- probability
    # result[paste(num)] <- as.data.frame(col)
  }

  plot_probabilities(c(n:(n * 6)), result)

  df <- as.data.frame(result)
  return(result)
}

simResult <- plot_distribution_of_sum_of_n_dices(3, 1000)


dice_counts <- c(3, 5, 10, 15, 20, 50, 100)
# TODO############################################################
for (num in dice_counts) {
  plot_distribution_of_sum_of_n_dices(num, 10**5)
}

# TODO############################################################
random_sitting_order <- function(n) {
  # TODO: n = number of students.##########################
  return(sample(c(1:n), n))
}

res <- random_sitting_order(10)

# TODO############################################################
init_order <- c(1:10)
new_order <- random_sitting_order(10)

has_similar_neighbors <- function(students_count) {
  new_order <- random_sitting_order(students_count)
  has_sim <- FALSE

  i <- 1
  for (std in init_order) {
    if (i < length(init_order)) {
      nextBy <- init_order[i + 1]
    }

    j <- 1
    for (s in new_order) {
      if (j == length(new_order)) {
        next
      }

      if (s != std) {
        j <- j + 1
        next
      }

      if (new_order[j + 1] == nextBy) {
        return(TRUE)
      }

      break
    }

    if (i > 1) {
      prevBy <- init_order[i - 1]
    }

    j <- 1
    for (s in new_order) {
      if (j == 1) {
        next
      }

      if (s != std) {
        j <- j + 1
        next
      }

      if (new_order[j - 1] == prevBy) {
        return(TRUE)
      }

      break
    }

    i <- i + 1
  }

  return(FALSE)
  # This function generates a new order of students and return true if two
  # neighbors also appeared in the original order.
  # TODO###########################################################
}

print(has_similar_neighbors(10))

similar_neighbors_probability <- function(N, students_count) {
  # TODO##########################################################
  init_order <- c(1:students_count)
  TCount <- 0

  for (i in c(1:N)) {
    if (has_similar_neighbors(students_count)) {
      TCount <- TCount + 1
    }
  }

  return(TCount / N)
}


sample_sizes <- c(seq(100, 1000, 200), seq(1000, 10000, 2000), seq(10000, 100000, 20000), seq(100000, 1000000, 200000))
probs <- c()
# TODO#############################################################
for (size in sample_sizes) {
  probs <- c(probs, similar_neighbors_probability(size, 5))
}

plot(sample_sizes, probs,
  type = "l", # Line type
  col = "blue",
  lwd = 2,
  xlab = "Sample Size",
  ylab = "Probability",
) # Set y-axis to fit both lines

# TODO#############################################################
probs <- c()
for (n in c(3:20)) {
  probs <- c(probs, similar_neighbors_probability(10**3, n))
}

plot(c(3:20), probs,
  type = "l", # Line type
  col = "blue",
  lwd = 2,
  xlab = "Students",
  ylab = "Probability",
) # Set y-axis to fit both lines
