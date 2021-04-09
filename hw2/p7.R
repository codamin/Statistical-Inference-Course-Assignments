library(ggplot2)

#a
exam_mean <- 462;
exam_std <- 119;
project_mean <-584;
project_std <- 151
# Shorthand for exams = N(462, 119)
# Shorthand for projects = N(584, 151)

exam_score <- 620;
project_score <- 670;

#b    
exam_z_score = (exam_score - exam_mean) / exam_std;
project_z_score = (project_score - project_mean) / project_std;

cat('exam z-score is:', exam_z_score, '\n')
cat('project z-score is', project_z_score, '\n')

#c
p1 <- ggplot(data.frame(x = seq(-4, 4, length=100)), aes(x = x)) +
        stat_function(fun = dnorm) +
        geom_vline(xintercept = c(exam_z_score, project_z_score),
                                     colour=c('blue', 'red'),
                                     linetype = "dashed") + 
        geom_text(aes(x = c(exam_z_score, project_z_score),
                        y = 0,
                        label = c('exam', 'project')),
                  data=data.frame(c(exam_z_score, project_z_score))) +
        labs(title=paste("Displaying Z-Scores on Normal Distribution")) +
        theme_classic()
show(p1)

#f
exam_percentile_score <- pnorm(exam_z_score, 0, 1)
project_percentile_score <- pnorm(project_z_score, 0, 1)
cat('percentile score of exam:', exam_percentile_score , '\n')
cat('percentile score of project ', project_percentile_score, '\n')

#g
cat(100 - exam_percentile_score * 100, " % of students performed better that her in exam\n")

#h
cat(100 - project_percentile_score * 100, "% of students performed better that her in project\n")

#i
scores <- c(57, 66, 69, 71, 72, 73, 74, 77, 78, 78, 79, 79, 81, 81, 82, 83, 83, 88, 89, 94)

## QQplot
qqnorm(scores)
qqline(scores, col='red', lwd=2, lty=2)

## Histogram
h <- hist(scores)
xfit <- seq(min(scores), max(scores), by=0.01)
yfit <- dnorm(xfit, mean = mean(scores), sd = sd(scores)) * 5 * length(scores)
lines(xfit, yfit, col="blue")

## Box Plot
boxplot(scores)
title(main = "scores box plot", ylab = "score")



