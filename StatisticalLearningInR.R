library(MASS) # Built-in dataset library
library(ISLR) # Data library for Introduction to Statistial Learning in R, Hastie Tibshirani

### Simple Linear regression
names(Boston) # The Boston dataframe
?Boston
plot(medv~lstat, Boston)
fit1 = lm(medv~lstat, data=Boston)
fit1
summary(fit1)
abline(fit1, col="red")
names(fit1)
hist(fit1$residuals) # See if residual are normally distributed
Boston
confint(fit1)
predict(fit1, data.frame(lstat=c(5,10,15)), interval="confidence") # See lower and upper confidence band

### Multiple Linear regression
