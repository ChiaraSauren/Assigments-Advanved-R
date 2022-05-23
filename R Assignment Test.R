test <- 1
library(MASS)
data(Boston)
reg<-lm(crim~., data=Boston)
summary(reg)
