#Problem 5

#here we create samples
men <- c(128.35, 160.34, 133.74, 138.12, 91.00, 97.43, 128.58, 148.78, 150.65, 110.96,
             135.7, 118.77, 147.1, 107.2, 122.46, 129.36, 158.14, 102.72, 136.59, 146.02, 105.88,
             111.24, 131.22, 124.6, 137.85, 136.46, 145.31, 166.71, 158.66, 108.63, 103.11, 149.29)
women <- c(116.62, 137.15, 106.07, 172.58, 151.33, 98.73, 136.11, 149.9, 140.8, 98.58,
             158.4, 97.97, 117.99, 126.53, 128.67, 126.57, 124.3, 120.39, 150.08, 143.05, 130.18,
             108.04, 136.39, 124.94, 136.86, 143.03, 128.58, 142.51, 151.68, 120.94)

#this function calculates variance of sample:
miu_var <- function(sample) {
  return(var(sample) / length(sample))
}

#this function prints the interval:
print_interval <- function(lower, upper) {
  cat("95% confidence interval is: (")
  cat(lower_bound, upper_bound)
  cat(')')
}

#here we find std of the difference of the means:
sigma <- sqrt(miu_var(men) + miu_var(women))

#finding point estimate:
point_estimate <- mean(men) - mean(women)

#finding the right and left decision boundaries:
lower_bound <- point_estimate - sigma * qnorm(0.975)
upper_bound <- point_estimate + sigma * qnorm(0.975)
print_interval(lower_bound, upper_bound)

#################### Output #############################

# 95% confidence interval is: (-10.07359 9.094549)

################################################################################################
################################################################################################

#Problem 7

#a

miu_0 <- 28
miu_a <- c(22, 23, 24, 25, 26, 27)
s <- 5.6

calc_power <- function(alpha, n) {
  se <- s / sqrt(n)
  left_boundary <- miu_0 - qnorm(1-alpha)*se
  powers <- pnorm((left_boundary - miu_a) / se)
}

powers_a <- calc_power(0.05, 50)

plot(miu_a, powers_a, type = "b", col='blue', pch=15,
     xlab="true mean", ylab="power")

legend("topright", legend=c("alpha=0.05"),
       col=c("blue"), pch = 15)
##################################################################
#b
powers_a <- calc_power(0.05, 50)
plot(miu_a, powers_a, type = "b", col='blue', pch=15, 
     xlab="true mean", ylab="power")
powers_b <- calc_power(0.01, 50)
lines(miu_a, powers_b, col='red', lw=2, pch=17, type = "b")
legend("topright", legend=c("alpha=0.05", "alpha=0.01"),
       col=c("blue", "red"), pch = c(15,17))
##################################################################
#c
powers_a <- calc_power(0.05, 50)
plot(miu_a, powers_a, type = "b", col='blue', pch=15,
     xlab="true mean", ylab="power")
powers_a <- calc_power(0.05, 20)
lines(miu_a, powers_a, col='green', lw=2, pch=19, type = "b")
legend("topright", legend=c("n=50", "n=20"), col=c("blue", "green"),
       pch = c(15, 19))
