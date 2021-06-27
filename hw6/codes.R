library(openintro)
library(ggplot2)
library(ggfortify)


# a
p_a <- ggplot(starbucks, aes(x = calories, y = carb)) +
  geom_point() +
  labs(x="calories",
       y="carbohydrate grams") +
  theme(plot.title = element_text(hjust = 0.5)) +
  theme_classic()

show(p_a)


# c
model <- lm(formula = carb ~ calories, starbucks)
p_c <- p_a + stat_smooth(method = lm, se = FALSE)
show(p_c)

#e
summary(model)

# f
p_f <- autoplot(model)
show(p_f)
