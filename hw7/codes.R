# P3

X = c(315, 317, 316, 316, 295, 318, 317, 316, 269, 314)
Y = c(321, 319, 267, 242, 324, 323, 284, 258, 257, 322)
n = length(X)
m = length(Y)

U = sum(rank(c(X, Y)[20:1])[1:10])

mean = n * (n + m + 1) / 2

sd = sqrt(n * m * (n + m + 1) / 12)

2 * pnorm(abs((U-0.5 - mean) / sd), lower.tail=F)
################################################################################
# P7

# 7.A
data1 <- data.frame(status.died=c(1, 1, 0, 0),
  hospital.A=c(1, 0, 1, 0), freq=c(63, 16, 2037, 784))

m1 <- glm(status.died ~ hospital.A, weights=freq, data=data1, family=binomial)
print(summary(m1))

coefs1 = summary(m1)$coefficients
PE = coefs1[2]
SE = coefs1[4]
ME = SE*qnorm(0.975)
CI1 = exp(PE + c(-ME, ME))
cat("CI part A=", CI1)

cat("Odds Ratio for Hospital A vs Hospital B in part 1= ", exp(PE))



# 7.B
data2 <- data.frame(condition=c(rep("Good", 4), rep("Poor", 4)),
                    status.died=rep(c(1, 1, 0, 0), 2),
                   hospital.A=rep(c(1, 0, 1, 0), 2), freq=c(6, 8, 594, 592, 57, 8, 1443, 192))

m2 <- glm(status.died ~ hospital.A + condition, weights=freq, data=data2, family=binomial)
print(summary(m2))

coefs2 = summary(m2)$coefficients
PE = coefs2[2]
SE = coefs2[5]
ME = SE*qnorm(0.975)
CI2 = exp(PE + c(-ME, ME))
cat("CI part B=", CI2)

cat("Odds Ratio for Hospital A vs Hospital B in part 1= ", exp(PE))



Status <- c(rep(c(0),79), rep(c(1), 2821))
Hospital <- c(rep(c(0),63), rep(c(1),16), rep(c(0),2037), rep(c(1),784))
data <- data.frame(Hospital, Status)
m1 <- glm(Status ~ Hospital, data=data, family=binomial)

Condition <- c(rep(c(0), 1200), rep(c(1), 1700))
Status <- c(rep(c(0),14), rep(c(1), 1186), rep(c(0),65), rep(c(1), 1635))
Hospital <- c(rep(c(0),6), rep(c(1),8), rep(c(0),594), rep(c(1),592),
              rep(c(0),57), rep(c(1),8), rep(c(0),1443), rep(c(1),192))
data <- data.frame(Hospital, Status)
m2 <- glm(Status ~  Hospital + Condition, data=data, family=binomial)

################################################################################
# P8

# 8.A
set.seed(42)
data <- read.csv("Data.csv")

train.size <- floor(2/3 * nrow(data))
train.ind <- sample(seq_len(nrow(data)), size = train.size)
train.data <- data[train.ind, ]
test.data <- data[-train.ind, ]

full.model <- glm(Response ~ ., data = train.data, family = binomial)
summary(full.model)

################################################################################

#8.C
nullmod <- glm(Response~1, data=data, family="binomial")

r2.adj <- function (null.model, model) {
  1-logLik(model)/logLik(null.model)
}

vars <- c("Adhes", "BNucl", "Chrom", "Epith", "Mitos", "NNucl", "Thick", "UShap", "USize")
selected_vars <- c()
max_adj_r2 <- c(0)

while(T) {
  rem_vars <- setdiff(vars, selected_vars)
  # print(rem_vars)
  if(length(rem_vars)==0) {
    break
  }
  step_vars <- c()
  for(j in length(rem_vars)) {
    step_max_adj_r2 <- 0
    current_var <- rem_vars[j]
    # print(current_var)
    step_vars <- c(selected_vars, current_var)
    mod <- glm(as.formula(paste("Response",
                                paste(step_vars, collapse=" + "), sep=" ~ ")),
               data=train.data,
               family="binomial")
    adjr2 <- r2.adj(nullmod, mod)
    if(adjr2 > step_max_adj_r2) {
      step_max_adj_r2 <- adjr2
      step_best_model <- mod
      step_best_var <- current_var
    }
  }
  if(step_max_adj_r2 >= max_adj_r2[length(max_adj_r2)]) {
    max_adj_r2 <- c(max_adj_r2, step_max_adj_r2)
    best_model <- step_best_model
    selected_vars <- c(selected_vars, step_best_var)
  }
  else {
    print('here')
    break
  }
}

plot(max_adj_r2[seq(2, length(max_adj_r2))],
     main = "adjusted R squared",
     xlab = "step",
     ylab = "adj. R^2")


print("selected variables are:",)
print(selected_vars)



# 8.D

library(plotROC)
library(ggplot2)

train.data$pred=predict(best_model, newdata=train.data)

roc_curve_train <- ggplot(train.data,
                    aes(m = pred,
                        d = Response)) +
  geom_roc(n.cuts=20,
           labels=F) + 
  theme_classic()

show(roc_curve_train + annotate("text", x = .75, y = .25 , label =
                          paste("AUC For Train =",
                                round(calc_auc(roc_curve_train)["AUC"], 3))))



test.data$pred=predict(best_model, newdata=test.data)

roc_curve_test <- ggplot(test.data,
                  aes(m = pred,
                      d = Response)) +
  geom_roc(n.cuts=20,
           labels=F) + 
  theme_classic()

show(roc_curve_test + annotate("text", x = .75, y = .25 , label =
                            paste("AUC For Test =",
                                  round(calc_auc(roc_curve_test)["AUC"], 3))))


#8.E
library(dplyr)
library(tidyverse)
probabilities <- predict(best_model, type = "response")
predictors <- colnames(mydata)
train.data$logit <- log(probabilities/(1-probabilities))
pairs <- gather(train.data[, !(names(train.data) %in% c("pred", "Response"))],
                key = "predictors", value = "predictor.value", -logit)


ggplot(pairs, aes(logit, predictor.value))+
  geom_point(size = 0.5, alpha = 0.5) +
  geom_smooth(method = "loess") + 
  theme_bw() + 
  facet_wrap(~predictors, scales = "free_y")



