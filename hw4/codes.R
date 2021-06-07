# 810196410
# Problem 3
###################################################################################
#a
num_reps = 1000
data = read.csv('Chick.csv')
s1 <- data[data$feed == 'casein', 'weight']
s2 <- data[data$feed == 'meatmeal', 'weight']

n1 <- length(s1)
n2 <- length(s2)

boot_dist = c()
boot_stats = c()
for(i in (1:num_reps)) {
  smp1 = sample(s1, n1, rep=T)
  smp2 = sample(s2, n2, rep=T)
  diff_of_means = mean(smp1)-mean(smp2)
  smp_SE = sqrt(var(smp1)/n1 + var(smp2)/n2)
  boot_dist = c(boot_dist, diff_of_means)
  boot_stats = c(boot_stats, diff_of_means / smp_SE)
}

hist(boot_dist, breaks = 20)

#b
#calculating the pvalue with original sample
p.value.original = t.test(s1, s2)$p.value
cat("p-value with original sample =", p.value.original, '\n')

#calculating the pvalue with bootstrap samples
orig.statistic <- t.test(s1, s2)$statistic
p.value.bootstrap = mean(boot_stats - mean(boot_stats) > orig.statistic)
cat("p-value with bootstrap samples =", p.value.bootstrap, '\n')

#c
cat("confidence interval with original sample =", t.test(s1, s2, conf.int=95)$conf.int, '\n')
cat("confidence interval with bootstrap sample =", quantile(boot_dist, c(.025,.975)), '\n')


# Problem 8
###################################################################################
data = read.csv('Diet.csv')
data["weight_loss"] = data$weight6weeks - data$pre.weight

# a
boxplot(data$weight_loss ~ data$Diet,
        col="orange",
        main="Boxplot of the weight loss across the groups",
        ylab="Weight Loss",
        xlab="Diet Type") 

# b
anova.result <- aov(weight_loss ~ as.factor(Diet), data=data)

# c
summary(anova.result)

# d
TukeyHSD(anova.result)
