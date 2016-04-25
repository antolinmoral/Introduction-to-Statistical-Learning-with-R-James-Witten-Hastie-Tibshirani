require(ISLR)
require(boot) # Bootstrap resampling package
?boot
?cv.glm # Get estimated k-fold cross-validation prediction error for genaralized linear models
plot(mpg~horsepower, data= Auto)

View(Auto)

## Use cross validation to determine hyperparameter degree d of polynomial to fit the data
## using glm
## LOOVC - Leave one out cross validation
## K-fold cross validation
glm.fit = glm(mpg~horsepower, data=Auto) # If family is not specified, fits a linear model
cv.glm(Auto, glm.fit)$delta # Brute force fitting. Slow!!! Fit each time.  


## LOOCV - fast algorithm hand coded
loocv = function(fit){
  res = residuals(fit)
  h = lm.influence(fit)$h
  mean((res/(1-h))^2)  
}

loocv(glm.fit)

# Find the best polynomial fit of degree d - going beyond linear fit here
cv.error = rep(0,5)
degree = 1:5
for(d in degree){
  glm.fit = glm(mpg~poly(horsepower,d), data=Auto) # Fit a polynom of degree d
  cv.error[d] = loocv(glm.fit)
}
plot(degree, cv.error, type="b") # Second order seems to be quite a good fit

## 10-fold cross validation CV on the same polynomial fit w/ hyperparemeter of degree d of the polynomial
cv.error10=rep(0,5)
for(d in degree){
  glm.fit = glm(mpg~poly(horsepower, d), data=Auto)
  cv.error10[d] = cv.glm(Auto, glm.fit, K=10)$delta[1]
}
lines(degree, cv.error10, type="b", col="red")


## Bootstrap
## Minimum risk investment - Section 5.2
## Calculate the best combination of x and y stocks: \alpha x + (1 - \alpha) y
## Use the bootstrap to calculate standard error on the best \alpha

View(Portfolio)
alpha = function(x,y){
  vx = var(x)
  vy = var(y)
  cxy = cov(x,y)
  (vy - cxy)/(vx+vy-2*cxy)
}
alpha(Portfolio$X, Portfolio$Y)

## What is the standard error on alpha?
alpha.fn = function(data, index){
  with(data[index,], alpha(X, Y)) # Take dataframe, here data[index,], and use its columns in functions, here alpha.
}

alpha.fn(Portfolio, 1:100)

set.seed(1)
alpha.fn(Portfolio, sample(1:100, 100, replace=TRUE))

boot.out = boot(Portfolio, alpha.fn, R=1000)
boot.out
plot(boot.out)

