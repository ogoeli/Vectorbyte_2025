## load packages
library(tidyverse)
library(forecast)
library(Metrics)
library(corrplot)


####### Enter Team Name ######
teamName <- "GATORS"

###### Enter path name to file containing VectorByte Training Data ######

pathname <- "ENTER PATH NAME HERE"
setwd(pathname)

###### Read in the data ######

dengue <- read.csv("dengueForecasting.csv")

###### Modeling Space ######
# Get creative and do your modeling here!








###### Get Forecasts ######

# Save the 12 forecasts for each state in a separate vector below. Start
# with the earliest forecast (4/14/2024). Feel free to use your coding 
# skills to avoid having to type each one out.

MG <- c(10.433970, 11.122607, 11.338292, 11.271930, 11.132900, 10.856689, 10.747865,
        10.427417,  9.881920, 9.419911,  8.899088,  8.154794) #stl, medRH, log
SP <- c(9.867889, 10.390437, 10.683834, 10.939152, 11.097042, 11.228458,
        11.309535, 11.368877, 11.395679, 11.426626, 11.448112, 11.455473) #Arima, minRH, log
RS <- c(27.64109, 32.07926, 35.49469, 34.22553, 33.47458, 29.65269, 26.86080,
        25.28273, 20.24188, 18.96964, 18.06958, 16.59861) #stl, population, sqrt
BA <- c(31.53025, 35.62920, 38.68885, 40.35306, 40.28438, 41.81339, 44.44891,
        45.25244, 42.13076, 41.47638, 39.08228, 33.56220) #stl, minRH, sqrt
PR <- c(4.568701, 4.770613, 4.924596, 4.984900, 4.932586, 4.989910, 4.972878, 
        4.837504, 4.661399, 4.534387, 4.338490, 4.116432 ) #stl, maxPressure, log

compare$forecast2

dengue <- read.csv("dengueForecasting.csv")


PR <- dengue |>
  filter(uf == 'PR')

BA <- dengue |>
  filter(uf == 'BA')

RS <- dengue |>
  filter(uf == 'RS')

SP <- dengue |>
  filter(uf == 'SP')

MG <- dengue |>
  filter(uf == 'MG')


# This code pulls them together into a data set for exporting

forecasts <- data.frame(teamName = rep(teamName, 60),
                        state = rep(c("MG", "SP", "RS", "BA",
                                      "PR"), each = 12),
                        epiweek = rep(seq(202416, 202427, 1), 5),
                        cases = c(BA, MG, PR, RS, SP))

####### Export Forecast .csv ######

filename <- paste0(teamName,"_dengueChallenge_VB_2025.csv")

# This will export the file to the folder where you have the data stored
write.csv(forecasts, filename)

# Read in the clean data set
phen <- MG

ggplot(data = phen, aes(x = date, y = cases))+   
  geom_point() 

ggplot(data = phen, aes(x = date, y = population))+   #enso, iod, pdo
  geom_point() 


phen <- MG[, !(names(MG) %in% c("test", "uf", "X", "date", "epiweek"))]

lm1 <- lm(cases ~., data = phen)
lm1 <- cor(phen, use = "complete.obs")
summary(lm1)
corrplot(lm1)

phen <- MG[, !(names(MG) %in% c("test", "uf", "X", "date", "epiweek", "enso", "iod", "pdo", "minPrecip"))]


phen <- log(phen)

lm1 <- cor(phen, use = "complete.obs")
summary(lm1)
corrplot(lm1)

phen$X <- MG$X
phen$date <- MG$date

ggplot(data = phen, aes(x = date, y = cases))+   
  geom_point() 


##turn our dataset into time series
phenTS <- ts(phen$cases, frequency = 52) 

##decompose our data
phenDecomp <- decompose(phenTS) 
plot(phenDecomp)

##look at our ACF lag
acf(phen$cases, lag.max = 52)

##partition our dataset for modeling
n <- nrow(phen)

# The length of the data set minus the most recent 30 days
trainN <- n-12
testN <- trainN+1

# Index the earlier rows for training
train <- phen[1:trainN,] 

# Index the later 30 for testing
test <- phen[testN:n,] ## or phen[-1:trainN,]
nrow(test) # Should be 30

#Simple modeling with ARIMA
ts.train <- ts(train$cases, frequency = 52)

#fit ARIMA
fit.arima<-auto.arima(ts.train)
summary(fit.arima)

par(mfrow=c(3,1), bty="n", mar = c(4,4,1,1))
plot(fit.arima$residuals)
acf(fit.arima$residuals, main="")
hist(fit.arima$residuals, main="")

# make a forecast 30 days out from the train date
arima.fc<-forecast(fit.arima, h=12)
plot(arima.fc)


#look at the fit and predict
plot(as.vector(phen$cases), xlab="day", ylab="pheno", type="l", col=1, lwd=2)
lines(as.vector(fit.arima$fitted), col=2, lty=2, lwd=2)
legend("topleft", legend=c("data", "fit", "FC"), col=c(1:2, "dodgerblue"), 
       lty=c(1,2,1), lwd=2)

## zoom into the lazt part
plot(as.vector(phen$cases), xlab="day", ylab="pheno", type="l", xlim=c(400, 460),
     col=1, lwd=2)
lines(as.vector(fit.arima$fitted), col=2, lty=2, lwd=2)
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
stl.arima.forecasts <- forecast(stl.fit.arima, h = 12)

# The forecast function gives us point forecasts, as well as
#    prediction intervals
stl.arima.forecasts

# Let's look at the forecasts!
plot(stl.arima.forecasts)


## COMPARE forecasts
# First make a data frame with both in there
compare <- data.frame(time = seq(1:12),
                      observed = test$cases,
                      forecast1 = arima.fc$mean,
                      forecast2 = stl.arima.forecasts$mean)

# What do you think??
ggplot(data = compare, aes(x = time, y = observed))+
  geom_line(color = "blue")+
  geom_point(color = "blue")+
  geom_line(aes(y = forecast1), color = "red", lty=2)+
  geom_point(aes(y = forecast1), color = "red", pch=1)+
  geom_line(aes(y = forecast2), color = "green", lty=2)+
  geom_point(aes(y = forecast2), color = "green", pch=1)


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
plot(train$medRH, type="l")
# We will start with just our predictors
lm.fit <- lm(cases ~ medRH, 
             data = train)
summary(lm.fit)
acf(lm.fit$residuals, 52) 

#filtering the temperature to smooth it out
## 7 day moving average, not including today
#leaves are out on trees might depend on not just the temperature today, 
#but maybe over the last week.
train$meanTempMA7 <- as.vector(stats::filter(ts(train$medRH), 
                                             filter = c(0,rep(1/3, 3)),
                                             sides = 1))
plot(train$medRH, type="l")

# Add to the training set
train$sinSeason <- sin((2*pi*train$X)/52)
train$cosSeason <- cos((2*pi*train$X)/52)

# Add to the testing set
test$sinSeason <- sin((2*pi*test$X)/52)
test$cosSeason <- cos((2*pi*test$X)/52)

#build a model for estimating average temperature over the previous 7 days
#using an ARIMA model:
temp.lm<-auto.arima(train$meanTempMA7)
summary(temp.lm)
acf(temp.lm$residuals)

#we forecast this ahead and add it to the testing data set
temp.fc<-forecast(temp.lm, 12)
test$meanTempMA7<-temp.fc$mean

# Add seasonality via sine and cosine terms to our train dataset
lm.fit <- lm(cases ~ sinSeason + cosSeason + meanTempMA7, 
             data = train)
summary(lm.fit)
acf(lm.fit$residuals, 52)

# Add trend
lm.fit <- lm(cases ~ X + sinSeason + cosSeason  + meanTempMA7, 
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
lm.forecasts <- forecast(lm.fit, h = 12, newdata = test)

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
lm.comps <- c(rmse = rmse(test$cases, test$forecast),
              mae = mae(test$cases, test$forecast))
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

