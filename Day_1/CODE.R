## load the dataset

data <- read.csv("transforms.csv")




# portion for X1 and Y1: did log of x1 and y1
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




# portion for X2 and Y2: did log of y2 and original value of x2
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
logX2<- log(data$X2)
logY2 <- log(data$Y2)

### re-run the regressions and residual plots to show this worked
lm1 <- lm(logY2 ~ data$X2)

## plot points and lines
plot(data$X2, logY2, col=1, main="I"); abline(lm1, col=2)



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




# portion for X3 and Y3: did sqrt of x3 and y3
#-----------------------------------------------------------------------------------------------

## fit models
lm1 <- lm(data = data, Y3 ~ X3)

## plot points and fitted lines
par(mfrow = c(1,2))
plot(data$X3, data$Y3, col=1, main="I");
plot(abs(data$X3), abs(data$Y3), col=1, main="I");
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
logX3<- sqrt(abs(data$X3))
logY3 <- sqrt(abs(data$Y3))

### re-run the regressions and residual plots to show this worked
lm1 <- lm(logY3 ~ logX3)

## plot points and lines
plot(logX3, logY3, col=1, main="I"); abline(lm1, col=2)



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




# portion for X4 and Y4: did exp () on x4 and made it negative. maybe to match negative y4 
#-----------------------------------------------------------------------------------------------

par(mfrow = c(1,1))
## fit models
lm1 <- lm(data = data, Y4 ~ X4)

## plot points and fitted lines
plot(data$X4, data$Y4, col=1, main="I");
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


par(mfrow = c(1,1))
### the fix is as follows:
logX1<- exp(-data$X4)
logY1 <- exp(-data$Y4)

### re-run the regressions and residual plots to show this worked
lm1 <- lm(logX1 ~ data$Y4)

## plot points and lines
plot(data$Y4, logX1, col=1, main="I"); abline(lm1, col=2)



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



# time dependet analysis



#-----------------------------------------------------------------------------------------

## load your data
mozData<-read.csv("Culex_erraticus_walton_covariates_aggregated.csv")
summary(mozData)


#plot the data
months<-dim(mozData)[1]
t<-1:months ## counter for months in the data set
par(mfrow=c(3,1))
plot(t, mozData$sample_value, type="l", lwd=2, 
     main="Average Monthly Abundance", 
     xlab ="Time (months)", 
     ylab = "Average Count")
plot(t, mozData$MaxTemp, type="l",
     col = 2, lwd=2, 
     main="Average Maximum Temp", 
     xlab ="Time (months)", 
     ylab = "Temperature (C)")
plot(t, mozData$Precip, type="l",
     col="dodgerblue", lwd=2,
     main="Average Monthly Precip", 
     xlab ="Time (months)", 
     ylab = "Precipitation (in)")


par(mfrow=c(1,1)) # rows, cols
## get the sqrt() of our sample value and plot it again
months<-dim(mozData)[1]
t<-1:months ## counter for months in the data set
plot(t, sqrt(mozData$sample_value), type="l", lwd=2, 
     main="Sqrt Average Monthly Abundance", 
     xlab ="Time (months)", 
     ylab = "Average Count")

## build a dataframe you use to build your model
t <- 2:months ## to make building the AR1 predictors easier

mozTS <- data.frame(
  Y=sqrt(mozData$sample_value[t]), # transformed response
  Yl1=sqrt(mozData$sample_value[t-1]), # AR1 predictor: lag by 1 day
  t=t, # trend predictor
  sin12=sin(2*pi*t/12), 
  cos12=cos(2*pi*t/12) # periodic predictors
)


mozTS$MaxTemp<-mozData$MaxTemp[t] ## current temps
mozTS$MaxTempl1<-mozData$MaxTemp[t-1] ## previous temps
mozTS$Precip<-mozData$Precip[t] ## current precip
mozTS$Precipl1<-mozData$Precip[t-1] ## previous precip


# build first model
mod1<-lm(Y ~ t, data=mozTS)
summary(mod1)

## plot points and fitted lines
plot(Y~t, data=mozTS, col=1, type="l")
lines(t, mod1$fitted, col="dodgerblue", lwd=2)

par(mfrow=c(1,3), mar=c(4,4,2,0.5))   

## studentized residuals vs fitted
plot(mod1$fitted, rstudent(mod1), col=1,
     xlab="Fitted Values", 
     ylab="Studentized Residuals", 
     pch=20, main="AR 1 only model")

## qq plot of studentized residuals
qqnorm(rstudent(mod1), pch=20, col=1, main="" )
abline(a=0,b=1,lty=2, col=2)

## histogram of studentized residuals
hist(rstudent(mod1), col=1, 
     xlab="Studentized Residuals", 
     main="", border=8)


par(mfrow=c(1,1))
# acf for mod1, if the lines are above the dotted blue lines,
# its a good lag space
acf(mod1$residuals)
# extract BIC to run model comparison later
n<-length(t)
extractAIC(mod1, k=log(n))[2]
plot(Y ~ t, data = mozTS, type = 'l', lty = 2, col = 1, lwd = 2)  # Dashed line
lines(mozTS$t, predict(mod1), col = 2, lwd = 2)  # Solid line
legend("topright", legend = c("Observed", "Predicted"), 
       col = c(1, 2), lty = c(2, 1), lwd = 2)
#-----------------------------------------------------------------------------------------




#------------------------------------------------------------------------------------------
#one that contains an AR term
# this is an autoregressive model of order one: you are decreasing by 1 row
mod2 <- lm(mozTS$Y[2:37] ~ mozTS$Y[1:36])
summary(mod2)

par(mfrow=c(1,1))
# acf for mod1, if the lines are above the dotted blue lines,
# its a good lag space
acf(mod2$residuals)
# extract BIC to run model comparison later
n<-length(t)
extractAIC(mod2, k=log(n))[2]
#------------------------------------------------------------------------------------------- 



#------------------------------------------------------------------------------------------
#AR one with the sine/cosine terms

mod3 <- lm(mozTS$Y ~ mozTS$Yl1 + sin12 + cos12, data=mozTS)
summary(mod3)

par(mfrow=c(1,1))
# acf for mod1, if the lines are above the dotted blue lines,
# its a good lag space
acf(mod3$residuals)
# extract BIC to run model comparison later
n<-length(t)
extractAIC(mod3, k=log(n))[2]
plot(Y ~ t, data = mozTS, type = 'l', lty = 2, col = 1, lwd = 2)  # Dashed line
lines(mozTS$t, predict(mod3), col = 2, lwd = 2)  # Solid line
legend("topright", legend = c("Observed", "Predicted"), 
       col = c(1, 2), lty = c(2, 1), lwd = 2)

#--------------------------------------------------------------------------------------------



#-----------------------------------------------------------------------------------------
#one with the environmental predictors

ccf(mozTS$MaxTemp, mozTS$Y)


mod4 <- lm(Y ~ Yl1+ MaxTempl1, data=mozTS) 
## adding the Yl1 cancels the AR of Y, 
#this makes it easy to focus on the effect of temp on the data
summary(mod4)

par(mfrow=c(1,1))
# acf for mod1, if the lines are above the dotted blue lines,
# its a good lag space
acf(mod4$residuals)
# extract BIC to run model comparison later
n<-length(t)
extractAIC(mod4, k=log(n))[2]
plot(Y ~ t, data = mozTS, type = 'l', lty = 2, col = 1, lwd = 2)  # Dashed line
lines(mozTS$t, predict(mod4), col = 2, lwd = 2)  # Solid line
legend("topright", legend = c("Observed", "Predicted"), 
       col = c(1, 2), lty = c(2, 1), lwd = 2)

#-----------------------------------------------------------------------------------------




#-----------------------------------------------------------------------------------------
#one with a combination
mod5 <- lm(Y ~ Yl1+ MaxTempl1 + sin12 + cos12 + t, data=mozTS) 
## adding the Yl1 cancels the AR of Y, 
#this makes it easy to focus on the effect of temp on the data
summary(mod5)

par(mfrow=c(1,1))
# acf for mod1, if the lines are above the dotted blue lines,
# its a good lag space
acf(mod5$residuals)
# extract BIC to run model comparison later
n<-length(t)
extractAIC(mod5, k=log(n))[2]
plot(Y ~ t, data = mozTS, type = 'l', lty = 2, col = 1, lwd = 2)  # Dashed line
lines(mozTS$t, predict(mod5), col = 2, lwd = 2)  # Solid line
legend("topright", legend = c("Observed", "Predicted"), 
       col = c(1, 2), lty = c(2, 1), lwd = 2)

#------------------------------------------------------------------------------------------
