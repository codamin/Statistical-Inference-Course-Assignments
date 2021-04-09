library(ggplot2)

data <- read.csv('StudentsPerformance.csv')

# a

males = data[data["gender"]=='male', ]
females = data[data["gender"]=='female', ]

means <- c(mean(males$writing_score), mean(females$writing_score));

p_a <- ggplot(data,
              aes(x=writing_score,
                  color=gender)) +
  geom_histogram(binwidth=1,
                 fill='white',
                 alpha=0.5,
                 position="identity") +

  geom_vline(data=data.frame(gender=c('male', 'female') , means=means),
             aes(xintercept=means, color=gender),
             linetype="dashed",
             size=0.8) +

  scale_color_brewer(palette="Dark2") +
  labs(title="Histogram of Writing Score Based on Gender", x="Writing Score", y = "Count")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5)) +
  scale_x_continuous(breaks=seq(0, 100, 40))

show(p_a)

# ################################################################################
# b
for (subj in c('reading_score', 'writing_score', 'math_score')) {
    p_b <- ggplot(data,
                aes_string(sample=subj, shape='gender',
                    colour='gender')) +
    stat_qq() +
    stat_qq_line() +
    scale_shape_manual(values=c(4,13)) +
    scale_color_brewer(palette="Dark2") +
    theme_classic() +
    labs(title=paste("QQ-Plot for", subj)) +
    theme(plot.title = element_text(hjust = 0.5))
    show(p_b)
}
################################################################################
# c

reading_score_mean <- mean(data$reading_score)
writing_score_mean <- mean(data$writing_score)
math_score_mean <- mean(data$math_score)

reading_score_sd <- sd(data$reading_score)
writing_score_sd <- sd(data$writing_score)
math_score_sd <- sd(data$math_score)

bin_width = 4;
num_samples = nrow(data);

scaled_dnorm = function(x, mean, sd, n) {
  bin_width * num_samples * dnorm(x, mean, sd)
}

for (subj in c('reading_score', 'writing_score', 'math_score')) {
  p_c <- ggplot(data,
              aes_string(x=subj)) +

  geom_histogram(binwidth=bin_width,
                 color='black',
                 fill='yellow',
                 position="identity",
                 alpha=0.5) +
  stat_function(fun=scaled_dnorm,
                args=list(data[[subj]],
                          mean=mean(data[[subj]]),
                          sd = sd(data[[subj]])),
                colour = "darkred", size = 1) +
  geom_vline(xintercept=unname(quantile(data[[subj]], c(0.025, 0.975))),
           linetype="dashed",
           size=0.8) +
  labs(title=paste("Probability Density of", subj), x=subj, y = "Count")+
  theme_classic() +
  theme(plot.title = element_text(hjust = 0.5))
  show(p_c)
}
################################################################################
#d
p_d <- ggplot(data, aes(race, math_score)) +
  geom_boxplot(aes(colour=gender)) +
  labs(title="Box Plot of Math Score based on race and genders", x="Race", y = "Math score") +
  theme(plot.title = element_text(hjust = 0.5))
show(p_d)
################################################################################
# e
data['avg_score'] <- round(rowMeans(data[, c('math_score', 'reading_score', 'writing_score')]), 2);

p_e <- ggplot(data, aes(x=parents_education, y=avg_score, fill=parents_education)) +
  geom_bar(stat="summary", fun="mean") +
  scale_fill_brewer(palette="Paired") +
  theme(axis.text.x=element_blank(),
      axis.ticks.x=element_blank()) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, 100)) +
theme(axis.line.x = element_line(size=1.5, colour="grey")) +
  labs(title="Bar plot of average score w.r.t. Parents Education Level", x="Parent's education level",
       y = "Average total score") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))
show(p_e)


