positions <- c('UR Developers', 'Back-end Developers',
               'magangement', 'HR', 'HSE')
counts <- c(8, 12, 4, 3, 3)
################################################################################

barplot(counts, names.arg = positions)
################################################################################
uid_salaries <- c(75000, 25000, 48000, 42000, 35200, 45000, 23000, 45500)
bed_salaries <- c(20000, 80000, 36000, 46300, 41000, 43000, 22000, 37000,
                  39000, 43500, 69000, 5000)
mng_salaries <- c(80000, 67000, 56000, 82000)
hr_salaries <- c(45000, 39000, 30000)
hse_salareis <- c(12000, 25000, 31500)

boxplot(uid_salaries, bed_salaries, mng_salaries, hr_salaries, hse_salareis,
        names = positions)
################################################################################
#d
print_spread_measures <- function(pos, salaries) {
  quantiles = quantile(salaries, names=FALSE);
  Q1 = quantiles[2]
  mean = quantiles[3]
  Q3 = quantiles[4]
  iqr = Q3 - Q1
  cat('first quartile of', pos, 'salaries:', Q1, '\n')
  cat('second quartile(mean) of', pos, 'salaries:', mean, '\n')
  cat('first quartile of', pos, 'salaries:', Q3, '\n')
  cat('IQR of', pos, 'salaries:', Q3, '\n')
  cat("############################################\n")
}

all_position_salaries = list(uid_salaries, bed_salaries, mng_salaries, hr_salaries, hse_salareis)
for (i in 1:5) {
  print_spread_measures(positions[i], all_position_salaries[[i]])
}
################################################################################
plot_hist_density <- function(pos, salaries) {
  hist(uid_salaries, main=paste("Histogram and Density For", pos), border="black", prob=TRUE)
  lines(density(uid_salaries))
}
for (i in 1:5) {
  plot_hist_density(positions[i], all_position_salaries[[i]])
}
################################################################################
all_salaries = c(uid_salaries, bed_salaries, mng_salaries, hr_salaries, hse_salareis)
sal_group1 <- sum(all_salaries > 50000)
sal_group2 <- sum(all_salaries > 40000 & all_salaries < 50000)
sal_group3 <- sum(all_salaries > 30000 & all_salaries < 40000)
sal_group4 <- sum(all_salaries > 20000 & all_salaries < 30000)
sal_group5 <- sum(all_salaries <= 20000)
labels <- c("A", "B", "C", "D", "E")
pie(c(sal_group1, sal_group2, sal_group3, sal_group4, sal_group5), labels)
################################################################################


