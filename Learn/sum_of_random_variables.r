x <- rpois(10000, 5)
y <- rpois(10000, 8)

z <- x + y
hist(z, breaks = 50)
hist(rpois(10000, 13), breaks = 50)
