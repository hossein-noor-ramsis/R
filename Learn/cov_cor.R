# data("iris")

plot(iris$Sepal.Length, iris$Sepal.Width)

library(ggplot2)

ggplot(iris, aes(Sepal.Length, Sepal.Width, color = Species)) +
  geom_point() + theme_bw()

# Cov:
# Cor:
