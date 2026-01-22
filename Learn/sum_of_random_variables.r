#### Poisson ####
x <- rpois(100000, 5)
y <- rpois(100000, 8)

z <- x + y
hist(z, breaks = 50)
hist(rpois(100000, 13), breaks = 50)

#### Uniform ####
x <- runif(100000)
y <- runif(100000)

z <- x + y
hist(z, breaks = 50)
hist(runif(100000), breaks = 50)

hist(z + runif(10000), breaks = 50)

#### Noraml ####
x <- rnorm(100000)
y <- rnorm(100000)

z <- x + y
hist(z, breaks = 50)
hist(rnorm(100000), breaks = 50)

hist(z + rnorm(10000), breaks = 50)
