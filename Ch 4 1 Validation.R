require(ISLR)
require(boot) # Bootstrap resampling package
?boot
?cv.glm # Get estimated k-fold cross-validation prediction error for genaralized linear models
plot(mpg~horsepower, data= Auto)
