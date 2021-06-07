#####################################
# P7
observed <- c(125, 175)
p <- c(1/24, 23/24)
chisq.test(observed, p=p)

#####################################
# P8

library(MASS)

t <- table(survey$Exer, survey$Smoke)

chisq <- chisq.test(t)

print(chisq$expected)
print(chisq$observed)
print(chisq)
#####################################