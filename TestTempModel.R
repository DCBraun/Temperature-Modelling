############################
# TestTempModel.R

# Model 1990 Fraser River temperatures using rough method of Hauge and Patterson 2015 
# First model the seasonal variability of water temp, air temp, and dishcharge with harmonic models
# Then regress water temp residuals against air temp and discharge residuals (possibly with lags and/or moving averages)
# Determine autocorrelation and add a lag (not yet done in this file)
# Pull model coefficients and predict river temperatures
# Model fit diagnostics (not yet done in this file)

# Created January 22, 2015
# A Putt
#############################    

setwd("~/InStream/TemperatureModelling")

# Read in temperautre, discharge, and air data
temp      <- read.csv("fraser1990.csv",head=TRUE,na.strings="-") # Fraser river temp from Patterson
air       <- read.csv("Surrey99AAirTemp1990.csv",header=TRUE) # Air temp in Surrey
discharge <- read.csv("FraserMission1990.csv",header=TRUE) # Discharge at Mission
str(temp)

########## 
# Fit the maximum temperatures to the date index using harmonic functions
# ! Haven't look at any of the variables for normality/linear assumptions

# Create a function for getting the harmonics
# Period is 365
# Note the Merran has a t' paramter to improve optimization that I'm not including here
HarmFunc <- function(data) {
  cosine  <- cos(2 * pi * data$dateindex/365)
  sine    <- sin(2 * pi * data$dateindex/365)
  cosine2 <- cos(2 * pi * data$dateindex * 2/365)
  sine2   <- sin(2 * pi * data$dateindex * 2/365)
  cosine3 <- cos(2 * pi * data$dateindex * 3/365)
  sine3   <- sin(2 * pi * data$dateindex * 3/365)
  return(list(cosine=cosine,sine=sine,cosine2=cosine2,sine2=sine2,cosine3=cosine3,sine3=sine3))
}

HarmTemp      <- HarmFunc(temp)

#  For temperature, fit and plot all three fundamental frequencies
lm.temperature  <- lm(temp$max ~ HarmTemp$cosine + HarmTemp$sine, na.action=na.exclude)
lm2.temperature <- lm(temp$max ~ HarmTemp$sine + HarmTemp$cosine + HarmTemp$cosine2 + HarmTemp$sine2, na.action=na.exclude)
lm3.temperature <- lm(temp$max ~ HarmTemp$sine + HarmTemp$cosine + HarmTemp$cosine2 + HarmTemp$sine2 + HarmTemp$cosine3 + HarmTemp$sine3, na.action=na.exclude)

#  Create a plot.
par(oma = rep(2, 4))
par(mar = c(4, 5, 1, 1))
plot(temp$dateindex, temp$max, type = "l", xlab = "Time (Days)", ylab = expression(paste("Temperature (",degree,"C)")), main = "", cex.lab = 1.6, lwd = 2, col = "darkgrey")
lines(temp$dateindex, predict.lm(lm.temperature),  lwd = 3, lty = 5)
lines(temp$dateindex, predict.lm(lm2.temperature), lwd = 3, lty = 5, col="red")
lines(temp$dateindex, predict.lm(lm3.temperature), lwd = 3, lty = 5, col="blue")

# The fit will just keep getting better and better with only one year, but with more years it
# should be easier to tell when the model has too many parameters

##########
# Create linear models for all three variables with three harmonics
HarmAir       <- HarmFunc(air)
HarmDischarge <- HarmFunc(discharge)

lm3.air       <- lm(air$max ~ HarmAir$sine + HarmAir$cosine + HarmAir$cosine2 + HarmAir$sine2 + HarmAir$cosine3 + HarmAir$sine3, na.action=na.exclude)
lm3.discharge <- lm(discharge$discharge ~ HarmDischarge$sine + HarmDischarge$cosine + HarmDischarge$cosine2 + HarmDischarge$sine2 + HarmDischarge$cosine3 + HarmDischarge$sine3, na.action=na.exclude)

##########
# Create a function for plotting the linear model results (only shows one fit, but easily modified)
LmPlotFunc <- function(data,yvals,lmdata) {
  par(oma = rep(2, 4))
  par(mar = c(4, 5, 1, 1))
  plot(data$dateindex, yvals, type = "l", xlab = "Time (Days)", ylab = expression(paste("Temperature (",degree,"C)")), main = "", cex.lab = 1.6, lwd = 2, col = "darkgrey")
  lines(data$dateindex, predict.lm(lmdata), lwd = 3, lty = 5)
}

LmPlotFunc(temp,temp$max,lm3.temperature)
LmPlotFunc(air,air$max,lm3.air)
LmPlotFunc(discharge,discharge$discharge,lm3.discharge)

##########
# Pull the residuals from the 3 harmonic model (for now) and fit to air temp and discharge residuals
tempresids      <- residuals(lm3.temperature,na.action=na.exclude)
airresids       <- residuals(lm3.air,na.action=na.exclude)
dischargeresids <- residuals(lm3.discharge,na.action=na.exclude)

# Model the residuals
lmairResid          <- lm(tempresids ~ airresids, na.action=na.exclude)
lmdischargeResid    <- lm(tempresids ~ dischargeresids, na.action=na.exclude)
lmairDischargeResid <- lm(tempresids ~ airresids + dischargeresids, na.action=na.exclude)

# Merren found a better correlation with a 7d average air temperature:
# Use filter function
avgNum  <- 7
air7day <- filter(airresids,rep(1/avgNum,avgNum),side=1)

# Re run the lm
lmairResid2 <- lm(tempresids ~ air7day, na.action=na.omit) # Fit is way improved with 7 day moving average, but would obviously need to explore

# ! Obviously a ton of fitting and diagnositcs is required here!!

##########
# Merran looked for autocorrelation and ended up applying a lag of one to pretty much all equations

##########
# Create the final models to predict temperature
# T = S + R + r
# S: seasonality in water temperature; R: nonseasonal variability (either airResid, dischargeResid, or airDischargeResid; r: autocorrelation

# Re-label coefficents to shorten code a bit!
S  <- lm3.temperature$coefficients
R1 <- lmairResid2$coefficients
R2 <- lmdischargeResid$coefficients
R3 <- lmairDischargeResid$coefficients
t  <- temp$dateindex # values to be fitted

# This model only incorporates the variation in air temperature (model R1); would need to also
# test the model diagnostics using R2 or R3

T <- S["(Intercept)"] + S["HarmTemp$sine"]*sin(2*pi*t/365) + S["HarmTemp$cosine"]*cos(2*pi*t/365) +
     S["HarmTemp$sine2"]*sin(2*pi*t*2/365) + S["HarmTemp$cosine2"]*cos(2*pi*t*2/365) +
     S["HarmTemp$sine3"]*sin(2*pi*t*3/365) + S["HarmTemp$cosine3"]*cos(2*pi*t*3/365) +
     R1["(Intercept)"] + R1["air7day"]*air7day

# Plot the predicted temperatures with the estimated time series T
par(oma = rep(2, 4))
par(mar = c(4, 5, 1, 1))
plot(temp$dateindex, temp$max, type = "l", xlab = "Time (Days)", ylab = expression(paste("Temperature (",degree,"C)")), main = "", cex.lab = 1.6, lwd = 2, col = "black")
lines(temp$dateindex, T,  lwd = 3, lty = 5, col="red")

##########
# Run a bunch of model diagnostics...
