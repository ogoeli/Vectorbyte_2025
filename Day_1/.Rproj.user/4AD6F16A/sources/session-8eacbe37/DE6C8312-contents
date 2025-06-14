## load the dataset

data <- read.csv("transforms.csv")




# portion for X1 and Y1
#-----------------------------------------------------------------------------------------------

## fit models
lm1 <- lm(data = data, Y1 ~ X1)

## plot points and fitted lines
plot(data$X1, data$Y1, col=1, main="I");
abline(lm1, col=2)


par(mfrow=c(1,3), mar=c(4,4,2,0.5))   

## studentized residuals vs fitted
plot(lm1$fitted, rstudent(lm1), col=1,
     xlab="Fitted Values", 
     ylab="Studentized Residuals", 
     pch=20, main="I")

## qq plot of studentized residuals
qqnorm(rstudent(lm1), pch=20, col=1, main="" )
abline(a=0,b=1,lty=2, col=2)

## histogram of studentized residuals: the data isnt normally distributed
hist(rstudent(lm1), col=1, 
     xlab="Studentized Residuals", 
     main="", border=8)


### the fix is as follows:
logX1<- log(data$X1)
logY1 <- log(data$Y1)

### re-run the regressions and residual plots to show this worked
lm1 <- lm(logY1 ~ logX1)

## plot points and lines
plot(logX1, logY1, col=1, main="I"); abline(lm1, col=2)



## studentized residuals vs fitted

par(mfrow=c(1,3), mar=c(4,4,2,0.5))  
plot(lm1$fitted, rstudent(lm1), col=1,
     xlab="Fitted Values", 
     ylab="Studentized Residuals", 
     pch=20, main="I")

## Q-Q plots
qqnorm(rstudent(lm1), pch=20, col=1, main="" )
abline(a=0,b=1,lty=2, col=2)

## histograms of studentized residuals: now the data is normally distributed. it was normalized.
hist(rstudent(lm1), col=1, 
     xlab="Studentized Residuals", 
     main="", border=8)

#------------------------------------------------------------------------------------------




# portion for X2 and Y2
#-----------------------------------------------------------------------------------------------

## fit models
lm1 <- lm(data = data, Y2 ~ X2)

## plot points and fitted lines
plot(data$X2, data$Y2, col=1, main="I");
abline(lm1, col=2)


par(mfrow=c(1,3), mar=c(4,4,2,0.5))   

## studentized residuals vs fitted
plot(lm1$fitted, rstudent(lm1), col=1,
     xlab="Fitted Values", 
     ylab="Studentized Residuals", 
     pch=20, main="I")

## qq plot of studentized residuals
qqnorm(rstudent(lm1), pch=20, col=1, main="" )
abline(a=0,b=1,lty=2, col=2)

## histogram of studentized residuals: the data isnt normally distributed
hist(rstudent(lm1), col=1, 
     xlab="Studentized Residuals", 
     main="", border=8)


### the fix is as follows:
logX1<- log(data$X2)
logY1 <- log(data$Y2)

### re-run the regressions and residual plots to show this worked
lm1 <- lm(logY1 ~ logX1)

## plot points and lines
plot(logX1, logY1, col=1, main="I"); abline(lm1, col=2)



## studentized residuals vs fitted

par(mfrow=c(1,3), mar=c(4,4,2,0.5))  
plot(lm1$fitted, rstudent(lm1), col=1,
     xlab="Fitted Values", 
     ylab="Studentized Residuals", 
     pch=20, main="I")

## Q-Q plots
qqnorm(rstudent(lm1), pch=20, col=1, main="" )
abline(a=0,b=1,lty=2, col=2)

## histograms of studentized residuals: now the data is normally distributed. 
# it was normalized.
hist(rstudent(lm1), col=1, 
     xlab="Studentized Residuals", 
     main="", border=8)

#------------------------------------------------------------------------------------------
