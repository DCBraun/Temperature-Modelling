############################
# TempModelFunctions.R
# Project: TemperatureModelling

# File for the functions that appear in the temperature modelling files
# Not all of these functions are currently used in the TempModel.r code
# Some were used and have since been removed, others were created but never used.

# Created January 29, 2015
# A Putt
############################# 

########## HarmFunc ##########
# Create a function for getting the harmonics
# Data=the data to be modelled
# Period is 365
HarmFunc <- function(data) {
  cosine  <- cos(2*pi*data$dateindex/365)
  sine    <- sin(2*pi*data$dateindex/365)
  cosine2 <- cos(2*pi*data$dateindex*2/365)
  sine2   <- sin(2*pi*data$dateindex*2/365)
  cosine3 <- cos(2*pi*data$dateindex*3/365)
  sine3   <- sin(2*pi*data$dateindex*3/365)
  return(list(cosine=cosine,sine=sine,cosine2=cosine2,sine2=sine2,cosine3=cosine3,sine3=sine3))
}

########## LmPlotFunc ##########
# Create a function for plotting the linear model results (only shows one fit, but easily modified)
# data=the data used for the lm; yvals=the variable modelled against the index; lmdata=the linear model
ylabT <- expression(paste("Temperature (",degree,"C)"))
ylabD <- paste("Discharge m3/s")
LmPlotFunc <- function(data,yvals,lmdata,yLab,title) {
  windows()
  par(mfrow=c(1,1))
  par(oma = rep(2, 4))
  par(mar = c(4, 5, 1, 1))
  plot(data$dateyear, yvals, type = "l", xlab = "Time (Days)", ylab = yLab, cex.lab = 1.6, lwd = 2, col = "darkgrey")
  mtext(side=3, line=1.5, cex=1.6, sprintf("Step 1: Harmonic Model Fits: %s",title))
  lines(data$dateyear, predict(lmdata,newdata=data.frame(dateindex=data$dateindex)), lwd = 3, lty = 5)
}

########## Model Diagnostics ##########
# Create a function to test model fit
# The stationarity test would probably be best done with plots
ModelDiagnostics <- function(data,lmmodel,title) {
  windows()
  Stationarity <- Box.test(residuals(lmmodel),lag=5) # low p value indicates stationarity
  par(mfrow=c(2,3))
  par(oma = rep(2, 4))
  Predicted <- predict(lmmodel)
  Residuals <- data$watertemp-Predicted
  PartialAutocorrelation <- acf(Residuals,type="partial",na.action=na.pass) # Most of my models have significant autocorrelation at 1
  SerialAutocorrelation <- acf(Residuals,na.action=na.pass) # Most of my models have significant autocorrelation at 1
  ResidPlot <- plot(Residuals,Predicted)
  NormalQQPlot <- qqnorm(Residuals)
  Hist <- hist(Residuals)
  mtext(side=3, title, outer=TRUE)
  FuncOut <- list(Stationarity=Stationarity,SerialAutocorrelation=SerialAutocorrelation)
  return(FuncOut)
}

########## tslag ##########
# Create a function that outputs a lagged variable
tslag <- function(var, lagNum=1) {
  n <- length(var)
  c(rep(NA,lagNum),var)[1:n]
}

########## NormalTest ##########
# Create a function to test for normality in a variable
NormalTest <- function(variable,title) {
  windows()
  #pval <- shapiro.test(variable)$p.value
  #print(sprintf("Shapiro test p-value: %s", pval))
  #print("Reject null hypothesis of normality if p <= 0.1")
  par(mfrow=c(1,2))
  par(oma = rep(2, 4))
  hist(variable)
  qqnorm(variable)
  mtext(side=3,title,outer=TRUE)
}
