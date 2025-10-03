library(ggplot2)

# Ex 1
df <- data.frame(
  x = 1:10,
  y = c(2, 5, 3, 6, 7, 8, 9, 12, 11, 15)
)

ggplot(df, aes(x = x, y = y)) +
  geom_point()

ggplot(df, aes(x = x, y = y)) +
  geom_line()

# Ex 2
df2 <- data.frame(
  fruit = c("apple", "banana", "cherry"),
  count = c(10, 5, 8)
)

ggplot(df2, aes(x = fruit, y = count)) +
  geom_bar(stat = "identity", fill = "skyblue")

# Ex 3
ggplot(df, aes(x = x, y = y)) +
  geom_point(color = "red", size = 3) +
  labs(
    title = "My First ggplot",
    x = "X values",
    y = "Y values"
  ) +
  theme_minimal()

# Ex 4
ggplot(data = mpg, aes(y = hwy, x = displ)) +
  geom_point()

ggplot(data = mpg, aes(y = hwy, x = displ, colour = class)) +
  geom_point()
