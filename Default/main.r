# cat("\014") = cls


printSumOfDivisors <- function(n) {
  sum <- 0

  for (i in 1:n) {
    if (n %% i == 0) {
      sum <- sum + i
      print(i)
    }
  }

  print(paste("sum:", sum))
}

printSumOfDivisors(100)

# vector
v1 <- c(1, 2, 3, 4, 5)
v2 <- c(5, 4, 3, 2, 1)
v3 <- c(2, 4, 8)

print(v1 < v2)
print(v1 == v2)
print(v1 + v2)
print(typeof(v3))
# print(v1 < v3) -> Error

# seq
print(v1 == 5)
print(1:9)
print(seq(from = 1, to = 10, by = 3))

# sample
print(sample(1:10))
print(sample(1:10, 3))

# Example
for (i in sample(1:100, 10)) {
  if (i %% 2 == 0) {
    print(i)
  }
}

# list
l1 <- list(1, 4, "hello", FALSE, c(1, 2, 3))
print(l1)
print(typeof(l1))

for (i in l1) {
  print(typeof(i))
}

# matrix
m <- matrix(data = 1:30, nrow = 5, ncol = 6, byrow = T)
print(m)

# csv
questions <- read.csv("./Default/questions.csv")
print(questions)
print(typeof(questions))
print(questions[1])
print(questions[-1])
print(questions[1, 2])
print(questions[1, 1:5])
print(questions["subject"])

# order
c4 <- sample(20:11)
print(c4)
print(order(c4))
print(c4[order(c4)])
