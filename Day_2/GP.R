#load packages
library(mvtnorm)
library(hetGP)
library(ggplot2)
library(MASS)

#we create the data for our example. (X, y) is our given data
# number of data points
n <- 8 

# Inputs - between 0 and 2pi
X <- matrix(seq(0, 2*pi, length= n), ncol=1) 

# Creating the response using the formula
y <- 5*sin(X) 

#we specify theta using the argument known
#  use the mleHomGP function to fit the GP
# Fitting GP 
gp <- mleHomGP(X = X, Z = y, known = list(theta = 1))
# Print output
gp


# Predictions
# Creating a prediction set: sequence from -0.5 to 2*pi + 0.5 with 100 evenly spaced points
XX <- matrix(seq(-0.5, 2*pi + 0.5, length= 100), ncol=1)
# Using the function to make predictions using our gpi object
yy <- predict(object = gp, x = XX)
# This will tell us the results that we have stored.
names(yy)

#
mean <- yy$mean # store mean
s2 <- yy$sd2 + yy$nugs # store variance

# Using the function and pass xprime as the new X matrix to get Sigma(XX, XX)
yy <- predict(object = gp, x = XX, xprime = XX)
# This is the covariance structure. This too does not include the nugget term.
Kn <- yy$cov 
Sigma <- Kn + diag(yy$nugs) # this is our predictive sigma of interest

# Since our posterior is a distribution, we take 100 samples from this.
YY <- rmvnorm (100, mean = mean, sigma = Sigma, checkSymmetry = F)

# We calculate the 95% bounds
q1 <- yy$mean + qnorm(0.025, 0, sqrt(s2))
q2 <- yy$mean + qnorm(0.975, 0, sqrt(s2))
trueYY <- 5 * sin(XX)

#plot our result
matplot(XX, t(YY), type = 'l', col = "gray", lty = 1)
matlines(XX, mean, col = "red", lty = 1, lwd = 2)
matlines(XX, q1, col = "red", lty = 2, lwd = 3)
matlines(XX, q2, col = "red", lty = 2, lwd = 3)
matlines(XX, trueYY, col = "blue", lty = 1, lwd = 2)
matpoints(X, y, col = "black", cex = 1.5, pch = 19)


#GP
#------------------------------------------------------------------------------------------
#Using GPs for data on tick abundances over time
library(tidyverse)
library(hetGP)


# Pulling the data from the NEON data base. 
target <- readr::read_csv("https://data.ecoforecast.org/neon4cast-targets/ticks/ticks-targets.csv.gz", guess_max = 1e1)

# Visualizing the data
head(target)

# transforms y
f <- function(x) {
  y <- log(x + 1)
  return(y)
}

# This function back transforms the input argument
fi <- function(y) {
  x <- exp(y) - 1
  return(x)
}


fx.iso_week <- function(datetime){
  startdate <- min(datetime) # identify start week
  x1 <- as.numeric(stringr::str_sub(ISOweek::ISOweek(datetime),7, 8)) # find iso week #
  return(x1)
}

#plot
x <- c(1:106)
sin_53 <- sin(2*pi*x/53)
sin_106 <- (sin(2*pi*x/106))
sin_106_2 <- (sin(2*pi*x/106))^2

par(mfrow=c(1, 3), mar = c(4, 5, 7, 1), cex.axis = 2, cex.lab = 2, cex.main = 3, font.lab = 2)
plot(x, sin_53, col = 2, pch = 19, ylim = c(-1, 1), ylab = "sin wave", 
     main = expression(paste(sin,  " ", (frac(2 * pi * X[1], 53)))))
abline(h = 0, lwd = 2)
plot(x, sin_106, col = 3, pch = 19, ylim = c(-1, 1), ylab = "sin wave", 
     main = expression(paste(sin,  " ", (frac(2 * pi * X[1], 106)))))
abline(h = 0, lwd = 2)
plot(x, sin_106_2, col = 4, pch = 19, ylim = c(-1, 1), ylab = "sin wave", 
     main = expression(paste(sin^2,  " ", (frac(2 * pi * X[1], 106)))))
abline(h = 0, lwd = 2)

# Then with datetime and the function, we can assign
fx.sin <- function(datetime){
  # identify iso week
  x <- as.numeric(stringr::str_sub(ISOweek::ISOweek(datetime),7, 8)) # find iso week 
  # calculate sin value for that week
  x2 <- (sin(2*pi*x/106))^2 
  return(x2)
}

normalize <- function(x, minx, maxx){
  return( (x - minx)/(maxx - minx))
}

# Choose a random site number: Anything between 1-9.
site_number <- 2

# Obtaining site name
site_names <- unique(target$site_id)

# Subsetting all the data at that location
df <- subset(target, target$site_id == site_names[site_number])
head(df)


# extracting only the datetime and obs columns
df <- df[, c("datetime", "observation")]

# Selecting a date before which we consider everything as training data and after this is testing data.
cutoff = as.Date('2022-12-31')
df_train <- subset(df, df$datetime <= cutoff)
df_test <- subset(df, df$datetime > cutoff)


# Setting up iso-week and sin wave predictors by calling the functions
X1 <- fx.iso_week(df_train$datetime) # range is 1-53
X2 <- fx.sin(df_train$datetime) # range is 0 to 1

# Centering the iso-week by diving by 53
X1c <- normalize(X1, min(X1), max(X1))

# We combine columns centered X1 and X2, into a matrix as our input space
X <- as.matrix(cbind.data.frame(X1c, X2))
head(X)

# Extract y: observation from our training model. 
y_obs <- df_train$observation

# Transform the response
y <- f(y_obs)

# Fitting a GP with our data, and some starting values for theta and g
gp <- mleHomGP(X, y)

# Create a grid from start date in our data set to one year in future (so we forecast for next season)
startdate <- as.Date(min(df$datetime))# identify start week
grid_datetime <- seq.Date(startdate, Sys.Date() + 30, by = 7) # create sequence from 

# Build the input space for the predictive space (All weeks from 04-2014 to 07-2025)
XXt1 <- fx.iso_week(grid_datetime)
XXt2 <- fx.sin(grid_datetime)

# Standardize
XXt1c <- normalize(XXt1, min(XXt1), max(XXt1))

# Store inputs as a matrix
XXt <- as.matrix(cbind.data.frame(XXt1c, XXt2))

# Make predictions using predGP with the gp object and the predictive set
ppt <- predict(gp, XXt) 


# Now we store the mean as our predicted response i.e. density along with quantiles
yyt <- ppt$mean
q1t <- ppt$mean + qnorm(0.025,0,sqrt(ppt$sd2 + ppt$nugs)) #lower bound
q2t <- ppt$mean + qnorm(0.975,0,sqrt(ppt$sd2 + ppt$nugs)) # upper bound

# Back transform our data to original
gp_yy <- fi(yyt)
gp_q1 <- fi(q1t)
gp_q2 <- fi(q2t)

# Plot the observed points
plot(as.Date(df$datetime), df$observation,
     main = paste(site_names[site_number]), col = "black",
     xlab = "Dates" , ylab = "Abundance",
     # xlim = c(as.Date(min(df$datetime)), as.Date(cutoff)),
     ylim = c(min(df_train$observation, gp_yy, gp_q1), max(df_train$observation, gp_yy, gp_q2)* 1.05))

# Plot the testing set data 
points(as.Date(df_test$datetime), df_test$observation, col ="black", pch = 19)

# Line to indicate seperation between train and test data
abline(v = as.Date(cutoff), lwd = 2)

# Add the predicted response and the quantiles
lines(grid_datetime, gp_yy, col = 4, lwd = 2)
lines(grid_datetime, gp_q1, col = 4, lwd = 1.2, lty = 2)
lines(grid_datetime, gp_q2, col = 4, lwd = 1.2, lty =2)


# Obtain true observed values for testing set
yt_true <- f(df_test$observation)

# FInd corresponding predictions from our model in the grid we predicted on
yt_pred <- yyt[which(grid_datetime  %in% df_test$datetime)]

# calculate RMSE
rmse <- sqrt(mean((yt_true - yt_pred)^2))
rmse

#-------------------------------------------------------------------------------------------




#hetGP
#-----------------------------------------------------------------------------------------
# create predictors
X1 <- fx.iso_week(df_train$datetime)
X2 <- fx.sin(df_train$datetime)

# standardize and put into matrix
X1c <- normalize(X1, min(X1), max(X1))
X <- as.matrix(cbind.data.frame(X1c, X2))

# Build prediction grid (From 04-2014 to 07-2025)
XXt1 <- fx.iso_week(grid_datetime)
XXt2 <- fx.sin(grid_datetime)

# standardize and put into matrix
XXt1c <- normalize(XXt1, min(XXt1), max(XXt1))
XXt <- as.matrix(cbind.data.frame(XXt1c, XXt2))

# Transform the training response
y_obs <- df_train$observation
y <- f(y_obs)

# Fit a hetGP model. X must be s matrix and nrow(X) should be same as length(y)
het_gpi <- hetGP::mleHetGP(X = X, Z = y)

# Predictions using the base R predict command with a hetGP object and new locationss
het_ppt <- predict(het_gpi, XXt)


# Mean density for predictive locations and Confidence bounds
het_yyt <- het_ppt$mean
het_q1t <- qnorm(0.975, het_ppt$mean, sqrt(het_ppt$sd2 + het_ppt$nugs))
het_q2t <- qnorm(0.025, het_ppt$mean, sqrt(het_ppt$sd2 + het_ppt$nugs)) 

# Back transforming to original scale
het_yy <- fi(het_yyt)
het_q1 <- fi(het_q1t)
het_q2 <- fi(het_q2t)


# Plot Original data
plot(as.Date(df$datetime), df$observation,
     main = paste(site_names[site_number]), col = "black",
     xlab = "Dates" , ylab = "Abundance",
     # xlim = c(as.Date(min(df$datetime)), as.Date(cutoff)),
     ylim = c(min(df_train$observation, het_yy, het_q2), max(df_train$observation, het_yy, het_q1)* 1.2))

# Add testing observations
points(as.Date(df_test$datetime), df_test$observation, col ="black", pch = 19)

# Line to indicate our cutoff point
abline(v = as.Date(cutoff), lwd = 2)

# HetGP Model mean predictions and bounds.
lines(grid_datetime, het_yy, col = 2, lwd = 2)
lines(grid_datetime, het_q1, col = 2, lwd = 1.2, lty = 2)
lines(grid_datetime, het_q2, col = 2, lwd = 1.2, lty =2)
legend("topleft", legend = c("Train Y","Test Y", "HetGP preds"),
       col = c(1, 1, 2), lty = c(NA, NA, 1),
       pch = c(1, 19, NA), cex = 0.5)


# compare the RMSE’s using the predictions of the hetGP model
yt_true <- f(df_test$observation) # Original data
het_yt_pred <- het_yyt[which(grid_datetime  %in% df_test$datetime)] # model preds

# calculate rmse for hetGP model
rmse_het <- sqrt(mean((yt_true - het_yt_pred)^2))
rmse_het
#------------------------------------------------------------------------------------------


#test
#-------------------------------------------------------------------------------------------

# Your turn
X <- mcycle$time
y <- mcycle$accel

# Predict on this set
XX <- matrix(seq(0, 301, length= 200), ncol=1)

# Data visualization
plot(X, y, ylim = c(-150, 100))


# Add code to fit a hetGP model and visualise it as above
# Fit a hetGP model. X must be s matrix and nrow(X) should be same as length(y)
# Fit a hetGP model. X must be s matrix and nrow(X) should be same as length(y)
het_gpi <- hetGP::mleHetGP(X = X, Z = y)

# Predictions using the base R predict command with a hetGP object and new locationss
het_ppt <- predict(het_gpi, XX)


# Mean density for predictive locations and Confidence bounds
het_yyt <- het_ppt$mean
het_q1t <- qnorm(0.975, het_ppt$mean, sqrt(het_ppt$sd2 + het_ppt$nugs))
het_q2t <- qnorm(0.025, het_ppt$mean, sqrt(het_ppt$sd2 + het_ppt$nugs)) 

# Back transforming to original scale
het_yy <- fi(het_yyt)
het_q1 <- fi(het_q1t)
het_q2 <- fi(het_q2t)

# Plot Original data
plot(as.Date(X), y, col = "black", xlab = "Dates" , ylab = "Abundance")

# Add testing observations
points(as.Date(X), y, col ="black", pch = 19)

# Line to indicate our cutoff point
abline(v = as.Date(cutoff), lwd = 2)

# HetGP Model mean predictions and bounds.
lines(grid_datetime, het_yy, col = 2, lwd = 2)
lines(grid_datetime, het_q1, col = 2, lwd = 1.2, lty = 2)
lines(grid_datetime, het_q2, col = 2, lwd = 1.2, lty =2)

legend("topleft", legend = c("Train Y","Test Y", "HetGP preds"),
       col = c(1, 1, 2), lty = c(NA, NA, 1),
       pch = c(1, 19, NA), cex = 0.5)
