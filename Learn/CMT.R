#### Poisson ####
x <- rpois(100000, 5)

z <- x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x
hist(z, breaks = 50)

#### Uniform ####
x <- runif(100000)

z <- x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x
hist(z, breaks = 50)

#### Noraml ####
x <- rnorm(100000)

z <- x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x + x
hist(z, breaks = 50)
