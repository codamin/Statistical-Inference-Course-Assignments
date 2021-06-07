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
p_c <- p_a + stat_smooth(method = lm, se = FALSE)

show(p_c)
  

lreg <- lm(calories ~ carb, data=starbucks)

autoplot(fit)
# ggplot(lreg, aes(x = .fitted, y = .resid)) +
#   geom_point()
# 
# 
# lreg$fitted
