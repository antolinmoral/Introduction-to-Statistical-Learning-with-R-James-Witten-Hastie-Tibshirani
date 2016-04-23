require(ISLR)
require(boot) # Bootstrap resampling package
?boot
?cv.glm # Get estimated k-fold cross-validation prediction error for genaralized linear models
plot(mpg~horsepower, data= Auto)

View(Auto)

## LOOVC - Leave one out cross validation
glm.fit = glm(mpg~horsepower, data=Auto) # If family is specified, fits a linear model
cv.glm(Auto, glm.fit)$delta # Brute force fitting; Slow!!!

## LOOCV - fast algorithm hand coded
fit = glm.fit
loocv = function(fit){
  h = lm.influence(fit)$h
  mean((residuals(fit)/(1-h))^2)  
}

loocv(fit)





