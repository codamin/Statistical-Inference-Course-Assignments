################################################################################
#a
positions <- c('UR Developers', 'Back-end Developers', 'magangement', 'HR', 'HSE')
counts <- c(8, 12, 4, 3, 3)
################################################################################
#b

barplot(counts,
        names.arg = positions,
        main = "Bar chart of distribution of employees in different positions",
        xlab = "position name",
        ylab = "count",
        col.main = "red",
        col.lab = "blue")
################################################################################
#c

uid_salaries <- c(75000, 25000, 48000, 42000, 35200, 45000, 23000, 45500)
bed_salaries <- c(20000, 80000, 36000, 46300, 41000, 43000, 22000, 37000,
                  39000, 43500, 69000, 5000)
mng_salaries <- c(80000, 67000, 56000, 82000)
hr_salaries <- c(45000, 39000, 30000)
hse_salareis <- c(12000, 25000, 31500)

boxplot(uid_salaries, bed_salaries, mng_salaries, hr_salaries, hse_salareis,
        main = "Box plot of salaries of each position",
        xlab = "position name",
        ylab = "salary",
        col.main = "red",
        col.lab = "blue",
        names = positions)
################################################################################
#d

print_spread_measures <- function(pos, salaries) {
  quantiles = quantile(salaries, names=FALSE);
  min = min(salaries)
  Q1 = quantiles[2]
  mean = quantiles[3]
  Q3 = quantiles[4]
  max = max(salaries)
  iqr = Q3 - Q1
  upper_whisker = min(Q3 + 1.5*iqr, max)
  lower_whisker = max(Q1 - 1.5*iqr, min)
  outliers = salaries[salaries > upper_whisker | salaries < lower_whisker]
  cat('\t min value:', min, '\n')
  cat('\t first quartile:', Q1, '\n')
  cat('\t second quartile(mean):', mean, '\n')
  cat('\t third quartile:', Q3, '\n')
  cat('\t IQR:', iqr, '\n')
  cat('\t max value:', max, '\n')
  if(length(outliers) > 0)
    cat('\t outliers:', outliers, '\n')
  else
    cat('\t outliers: no outliers \n')
  cat("------------------------------------------------------------------\n")
}

all_position_salaries = list(uid_salaries, bed_salaries, mng_salaries, hr_salaries, hse_salareis)
for (i in 1:5) {
  cat('Quartiles for', positions[i], ' salaries:\n')
  print_spread_measures(positions[i], all_position_salaries[[i]])
}
################################################################################
#e

plot_hist_density <- function(pos, salaries) {
  hist(uid_salaries,
       main=paste("Histogram and Density For", pos),
       xlab=paste(pos, "Salaries"),
       border="black",
       prob=TRUE)
  lines(density(uid_salaries), col="blue", lwd=2)
}
for (i in 1:5) {
  plot_hist_density(positions[i], all_position_salaries[[i]])
}
################################################################################
#f

all_salaries = c(uid_salaries, bed_salaries, mng_salaries, hr_salaries, hse_salareis)
sal_group1 <- sum(all_salaries > 50000)
sal_group2 <- sum(all_salaries > 40000 & all_salaries < 50000)
sal_group3 <- sum(all_salaries > 30000 & all_salaries < 40000)
sal_group4 <- sum(all_salaries > 20000 & all_salaries < 30000)
sal_group5 <- sum(all_salaries <= 20000)
colors <- c("azure", "yellow1", "tomato", "orange1", "blue")
labels <- c("very high", "high", "middle", "low", "very low")
slices <- c(sal_group1, sal_group2, sal_group3, sal_group4, sal_group5)
percents <- round(slices/sum(slices)*100)
percents <- paste(percents,"%", sep="")
pie(slices, col=colors, labels=percents, radius=0.6, main="Pie Chart of Salaries")
legend("topleft", labels, fill=colors)
################################################################################
#g

print('center and spread measures for Backend Developers salaries:')
sprintf('mean: %.2f', mean(bed_salaries))
sprintf('median: %.2f', median(bed_salaries))
sprintf('variance: %.2f', var(bed_salaries))
sprintf('standard deviation: %.2f', sd(bed_salaries))
################################################################################


