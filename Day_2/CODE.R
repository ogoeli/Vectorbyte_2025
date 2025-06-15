## load packages
library(tidyverse)
library(forecast)
library(Metrics)

# Read in the clean data set
phen <- read.csv("NEONphenologyClean.csv")

ggplot(data = phen, aes(x = DATE, y = observation))+   
  geom_point() 

ggplot(data = phen, aes(x = DATE, y = meanTemp))+   
  geom_point() 

##turn our dataset into time series
phenTS <- ts(phen$observation, frequency = 365) 

##decompose our data
phenDecomp <- decompose(phenTS) 
plot(phenDecomp)

##look at our ACF lag
acf(phen$observation, lag.max = 365)

##partition our dataset for modeling
n <- nrow(phen)

# The length of the data set minus the most recent 30 days
trainN <- n-30
testN <- trainN+1

# Index the earlier rows for training
train <- phen[1:trainN,] 

# Index the later 30 for testing
test <- phen[testN:n,] ## or phen[-1:trainN,]
nrow(test) # Should be 30

#Simple modeling with ARIMA
ts.train <- ts(train$observation, frequency = 365)

#fit ARIMA
fit.arima<-auto.arima(ts.train)
summary(fit.arima)

par(mfrow=c(3,1), bty="n", mar = c(4,4,1,1))
plot(fit.arima$residuals)
acf(fit.arima$residuals, main="")
hist(fit.arima$residuals, main="")

# make a forecast 30 days out from the train date
arima.fc<-forecast(fit.arima, h=30)
plot(arima.fc)

#look at the fit and predict
plot(as.vector(phen$observation), xlab="day", ylab="pheno", type="l", xlim=c(0, 1560),
     col=1, lwd=2, ylim=c(0.25, 0.55))
lines(as.vector(fit.arima$fitted), col=2, lty=2, lwd=2)
lines(1528+1:30, arima.fc$mean, col="dodgerblue", lwd=2)
lines(1528+1:30, arima.fc$lower[,2], col="dodgerblue", lwd=2, lty=3)
lines(1528+1:30, arima.fc$upper[,2], col="dodgerblue", lwd=2, lty=3)
legend("topleft", legend=c("data", "fit", "FC"), col=c(1:2, "dodgerblue"), 
       lty=c(1,2,1), lwd=2)

## zoom into the lazt part
plot(as.vector(phen$observation), xlab="day", ylab="pheno", type="l", xlim=c(1160, 1560),
     col=1, lwd=2, ylim=c(0.3, 0.525))
lines(as.vector(fit.arima$fitted), col=2, lty=2, lwd=2)
lines(1528+1:30, arima.fc$mean, col="dodgerblue", lwd=2)
lines(1528+1:30, arima.fc$lower[,2], col="dodgerblue", lwd=2, lty=3)
lines(1528+1:30, arima.fc$upper[,2], col="dodgerblue", lwd=2, lty=3)
legend("topleft", legend=c("data", "fit", "FC"), col=c(1:2, "dodgerblue"), lty=c(1,2,1), lwd=2)

# There are several options that we can customize in the stl()
#   function, but the one we have to specify is the s.window. 
#   Check out the documentation and play around with some of the
#   other options if you would like.
stl.fit.arima <- stlm(ts.train, 
                      s.window = "periodic",
                      method = "arima")

summary(stl.fit.arima$model)

# look at the residuals to see if there is anything left unaccounted for.
par(mfrow=c(3,1), bty="n", mar = c(4,4,1,1))
plot(stl.fit.arima$residuals)
acf(stl.fit.arima$residuals)
hist(stl.fit.arima$residuals)

# We can generate forecasts with the forecast() function in the
#    forecast package
stl.arima.forecasts <- forecast(stl.fit.arima, h = 30)

# The forecast function gives us point forecasts, as well as
#    prediction intervals
stl.arima.forecasts

# Let's look at the forecasts!
plot(stl.arima.forecasts)


## COMPARE forecasts
# First make a data frame with both in there
compare <- data.frame(time = seq(1:30),
                      observed = test$observation,
                      forecast1 = arima.fc$mean,
                      forecast2 = stl.arima.forecasts$mean)

# What do you think??
ggplot(data = compare, aes(x = time, y = observed))+
  geom_line(color = "blue")+
  geom_point(color = "blue")+
  geom_line(aes(y = forecast1), color = "red", lty=2)+
  geom_point(aes(y = forecast1), color = "red", pch=1)+
  geom_line(aes(y = forecast2), color = "darkred", lty=2)+
  geom_point(aes(y = forecast2), color = "darkred", pch=1)


# Let's get the RMSE and MAE out and save them # for later.
arima.comps <- c(rmse = rmse(compare$observed,
                             compare$forecast1),
                 mae = mae(compare$observed,
                           compare$forecast1))


stl.comps <- c(rmse = rmse(compare$observed,
                           compare$forecast2),
               mae = mae(compare$observed,
                         compare$forecast2))
comps.DF<-as.data.frame(rbind(arima.comps, stl.comps))
comps.DF

#try out exponential smoothing, you will use method = "ETS"
#and you can specify different model types using etsmodel = "ANN"
stl.fit.ets <- stlm(ts.train, s.window = "periodic", method = "ets",
                    etsmodel = "ANN")
summary(stl.fit.ets$model)

#Extra predictors with Linear Regression
plot(train$meanTempLag1, type="l")
# We will start with just our predictors
lm.fit <- lm(observation ~ meanTempLag1, 
             data = train)
summary(lm.fit)
acf(lm.fit$residuals, 365) 

#filtering the temperature to smooth it out
## 7 day moving average, not including today
#leaves are out on trees might depend on not just the temperature today, 
#but maybe over the last week.
train$meanTempMA7 <- as.vector(stats::filter(ts(train$meanTemp), 
                                             filter = c(0,rep(1/7, 7)),
                                             sides = 1))
plot(train$meanTempMA7, type="l")

# Add to the training set
train$sinSeason <- sin((2*pi*train$X)/365)
train$cosSeason <- cos((2*pi*train$X)/365)

# Add to the testing set
test$sinSeason <- sin((2*pi*test$X)/365)
test$cosSeason <- cos((2*pi*test$X)/365)

#build a model for estimating average temperature over the previous 7 days
#using an ARIMA model:
temp.lm<-auto.arima(train$meanTempMA7)
summary(temp.lm)
acf(temp.lm$residuals)

#we forecast this ahead and add it to the testing data set
temp.fc<-forecast(temp.lm, 30)
test$meanTempMA7<-temp.fc$mean

# Add seasonality via sine and cosine terms to our train dataset
lm.fit <- lm(observation ~ sinSeason + cosSeason + meanTempMA7, 
             data = train)
summary(lm.fit)
acf(lm.fit$residuals, 365)

# Add trend
lm.fit <- lm(observation ~ X + sinSeason + cosSeason  + meanTempMA7, 
             data = train)
summary(lm.fit)
# Check for leftover autocorrelation in the residuals - still a lot.
#   Stay tuned for how to deal with this! For now we will move 
#   forward.
acf(lm.fit$residuals)

# These models still do assume normal residuals - these look good!
hist(lm.fit$residuals)

# We can generate forecasts with the forecast() function in the
#    forecast package
lm.forecasts <- forecast(lm.fit, h = 30, newdata = test)

# The forecast function gives us point forecasts, as well as
#    prediction intervals
lm.forecasts

# Add the forecasts to test
test$forecast <- lm.forecasts$mean

# and to the compare set
compare$forecast3 <- lm.forecasts$mean

# What do you think??
ggplot(data = compare, aes(x = time, y = observed))+
  geom_line(color = "blue")+
  geom_point(color = "blue")+
  geom_line(aes(y = forecast1), color = "red", lty=2)+
  geom_point(aes(y = forecast1), color = "red", pch=1)+
  geom_line(aes(y = forecast2), color = "darkred", lty=2)+
  geom_point(aes(y = forecast2), color = "darkred", pch=1)+
  geom_line(aes(y = forecast3), color = "brown", lty=2)+
  geom_point(aes(y = forecast3), color = "brown", pch=1)

#generate and save the accuracy metrics again.
lm.comps <- c(rmse = rmse(test$observation, test$forecast),
              mae = mae(test$observation, test$forecast))
comps.DF <-as.data.frame(cbind(model = c("arima", "stl", "lm"), rbind(arima.comps, stl.comps, lm.comps)))
comps.DF

# Which model did the best based on RMSE?
ggplot(data = comps.DF, aes(x = model, y = rmse))+
  geom_bar(stat = "identity", fill = "slateblue1")+
  labs(title = "RMSE Comparison")

# Which model did the best based on MAE?
ggplot(data = comps.DF, aes(x = model, y = mae))+
  geom_bar(stat = "identity", fill = "plum")+
  labs(title = "MAE Comparison")