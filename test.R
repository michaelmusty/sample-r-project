library(ISLR)
library(boot)
plot(mpg~horsepower,data=Auto)
## The Validation Set (Train/Test Split) Approach
set.seed(88)
attach(Auto) # ?ISLR::Auto
# 392/2 = 196
# "2-fold" crossvalidation i.e. Train/Test Split
train<-sample(392,196) # bootstrapping vs. crossvalidation <-> replacement vs. without replacement
cvv.error<-rep(0,10) # error for each model there are 10 different models to test
degree <- 1:10 # degree of polynomial in Lagrange interpolation
for(d in degree){ # glm = general linear model, can also use lm for this one
  glm.fit <- glm(mpg~poly(horsepower,d),data=Auto, subset=train)
  cvv.error[d]<-mean((mpg-predict(glm.fit,Auto))[-train]^2) # MSE
}
plot(degree,cvv.error,type="b", col="black", xlab = "Degree of Polynomial", ylab="CV Error", ylim=c(18,25))
which(cvv.error==min(cvv.error))
## LOOCV
glm.fit<-glm(mpg~horsepower, data=Auto)
# cv.glm is package to do crossvalidation
cv.glm(Auto,glm.fit)$delta # "delta" is the cross-validation prediction error
## the first # is the raw cross-validation result;
## and the second one is a bias-corrected version of it.
## Define a LOOCV function
loocv <- function(fit){ # input is the fit
  h<-lm.influence(fit)$h # h is the leverage
  mean((residuals(fit)/(1-h))^2)
}
loocv(glm.fit)
cv.error<-rep(0,10)
for(d in degree){
  glm.fit <- glm(mpg~poly(horsepower,d),data=Auto)
  cv.error[d]<-loocv(glm.fit)
}
lines(degree, cv.error, type="b", col="blue")
which(cv.error==min(cv.error))
## 10-Fold Cross-Validation
cv.error10<-rep(0,10)
for(d in degree){
  glm.fit <- glm(mpg~poly(horsepower,d),data=Auto)
  cv.error10[d]<-cv.glm(Auto,glm.fit, K=10)$delta[1]
}
lines(degree,cv.error10,type="b", col="red")
legend(7, 24, c("Validation-Set", "LOOCV", "10-Fold CV"), lty=c(1,1,1), lwd = c(2.5, 2.5,2.5), col = c("black", "blue", "red"))
which(cv.error10==min(cv.error10))
