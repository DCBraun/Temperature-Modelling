############################
# TestTempModel.R
# Project: TemperatureModelling

# Model Fraser River temperatures using rough method of Hauge and Patterson 2015 
# First model the seasonal variability of water temp, air temp, and dishcharge with harmonic models
# Then regress water temp residuals against air temp and discharge residuals (possibly with lags and/or moving averages)
# Pull model coefficients and predict river temperatures
# Also try one model with air temperature and discharge as variables

# Created February 2, 2015
# A Putt
#############################    

########## STEP 1: Fit Seasonal Model ##########
# Test all variables for normality
# Function: NormalTest(variable)
NormalTest(means$airtemp, title=paste("Step 1 Normal test of air temp data"))   # Not normal but can probably still use
NormalTest(means$watertemp,title=paste("Step 1 Normal test of water temp data")) # Not normal but can probably still use
# NormalTest(means$discharge,title=paste("Step 1 Normal test of discharge data")) # Totally not normal; log transform
NormalTest(log(means$discharge),title=paste("Step 1 Normal test of log discharge data")) # This is better

# Create linear models for all three variables with three harmonics
# This uses the function HarmFunc(data), which obtains the harmonics to be fed into the lms
# I have added an autocorrelation structure with a lag of one. Much improved the fit, but more diagnostics could be done 
Harm  <- HarmFunc(means) # I can't get gls to work unless I can supply a data argument, but I'll leave this func call in for now.

airHarm  <- gls(airtemp        ~ cos(2*pi*dateindex/365)+sin(2*pi*dateindex/365)+cos(2*pi*dateindex*2/365)+sin(2*pi*dateindex*2/365)+cos(2*pi*dateindex*3/365)+sin(2*pi*dateindex*3/365), cor=corAR1(form=~1), data=means,na.action=na.exclude)
disHarm  <- gls(log(discharge) ~ cos(2*pi*dateindex/365)+sin(2*pi*dateindex/365)+cos(2*pi*dateindex*2/365)+sin(2*pi*dateindex*2/365)+cos(2*pi*dateindex*3/365)+sin(2*pi*dateindex*3/365), cor=corAR1(form=~1), data=means,na.action=na.exclude)
tempHarm <- gls(watertemp      ~ cos(2*pi*dateindex/365)+sin(2*pi*dateindex/365)+cos(2*pi*dateindex*2/365)+sin(2*pi*dateindex*2/365)+cos(2*pi*dateindex*3/365)+sin(2*pi*dateindex*3/365), cor=corAR1(form=~1), data=means,na.action=na.exclude)

# Can check diagnostics using ModelDiagnostics(lmmodel) function
DiagnoseSeasonal <- ModelDiagnostics(means,tempHarm,title=paste("Step 1: Model Diagnosis for Seasonal Component"))

# Plot the data using LmPlotFunc(data,yvals,yLab,title) YlabT is temp, ylabD is discharge
LmPlotFunc(means,means$watertemp,tempHarm,ylabT,paste("Water Temperature"))
LmPlotFunc(means,means$airtemp,airHarm,ylabT, paste("Air Temperature"))
LmPlotFunc(means,log(means$discharge),disHarm,ylabD, paste("Discharge"))

########## STEP 2: Fit Non Seasonal Component Using Resids ########## 
# Obtain residuals
means$tempresids      <- residuals(tempHarm,na.action=na.exclude)
means$airresids       <- residuals(airHarm,na.action=na.exclude)
means$dischargeresids <- residuals(disHarm,na.action=na.exclude)

# Need to centre any residuals that will be used in the interaction model
means$airresids.centered       <- scale(means$airresids,scale=FALSE)
means$dischargeresids.centered <- scale(means$dischargeresids,scale=FALSE)

# Calculate the 7 day average air temp (Merran and Patterson 2014)
# Calculates the average of the previous 7 days, so the start of the vector has NAs
# Missing data in the middle of the time series also results in NAs
means$air7day <- rollapply(means$airresids, 7, mean, fill=NA, align="right")

# Check normality of variables
NormalTest(means$tempresids, title=paste("Step 2 Normal test of water temp resid data"))
#NormalTest(means$airresids, title=paste("Step 2 Normal test of air temp resid data"))
NormalTest(means$dischargeresids, title=paste("Step 2 Normal test of discharge resid data"))
NormalTest(means$air7day, title=paste("Step 2 Normal test of rolling mean air temp data"))

# Model the residuals
ResidModAir            <- gls(tempresids ~ airresids, data=means, na.action=na.exclude)
ResidModDis            <- gls(tempresids ~ dischargeresids, data=means, na.action=na.exclude)
ResidModAirDis         <- gls(tempresids ~ airresids + dischargeresids, data=means, na.action=na.exclude)
ResidModAirDisInt      <- gls(tempresids ~ airresids.centered:dischargeresids.centered, data=means, na.action=na.exclude)
ResidModAirLag         <- gls(tempresids ~ airresids, data=means, na.action=na.exclude,cor=corAR1(form=~1))
ResidModDisLag         <- gls(tempresids ~ dischargeresids, data=means, na.action=na.exclude,cor=corAR1(form=~1))
ResidModAirDisLag      <- gls(tempresids ~ airresids + dischargeresids, data=means, na.action=na.exclude,cor=corAR1(form=~1))
ResidModAirDisIntLag   <- gls(tempresids ~ airresids.centered:dischargeresids.centered, data=means, na.action=na.exclude,cor=corAR1(form=~1))
ResidModAirAvg         <- gls(tempresids ~ air7day, data=means, na.action=na.exclude) # Fit is way improved with 7 day moving average, but would obviously need to explore
ResidModAirAvgLag      <- gls(tempresids ~ air7day, data=means, na.action=na.exclude,cor=corAR1(form=~1)) # Fit is way improved with 7 day moving average, but would obviously need to explore


# Create an AIC table to evaluate these models
ResidModAICs   <- data.frame(Air=AIC(ResidModAir),Discharge=AIC(ResidModDis),AirDischarge=AIC(ResidModAirDis),AirDisInt=AIC(ResidModAirDisInt),Air7Day=AIC(ResidModAirAvg),
                             AirLag=AIC(ResidModAirLag),DisLag=AIC(ResidModDisLag),AirDisLag=AIC(ResidModAirDisLag),AirDisIntLag=AIC(ResidModAirDisLag),Air7DayLag=AIC(ResidModAirAvgLag))
ResidModAICs   <- gather(ResidModAICs,Model,AIC,1:ncol(ResidModAICs))
MinAICPosition <- which(ResidModAICs$AIC == min(ResidModAICs$AIC), arr.ind=TRUE)
print("Residual Models AIC (Step 2):")
print(ResidModAICs)
print(sprintf("Lowest AIC model: %s; %s", ResidModAICs[MinAICPosition[1],2], ResidModAICs[MinAICPosition[1],1]))

DiagnoseNonSeasonal <- ModelDiagnostics(means,ResidModAirAvgLag,title=paste("Step 2: Model Diagnosis for Non Seasonal Component")) # Not working need to fix model diagnostic

########## Step 3: Combine the Seasonal and Non Seasonal Components ##########

Tp.addComponents  <- predict(tempHarm,newdata=data.frame(dateindex=means$dateindex),na.action=na.pass)+predict(ResidModAirAvg,newdata=data.frame(air7day=means$air7day),na.action=na.pass)
mse.addComponents <- mean((Tp.addComponents-means$watertemp)^2,na.rm=TRUE)

########## Step 4: Alternative Model with Discharge and Air Temp as Parameters ##########
allModel  <- gls(watertemp ~ cos(2*pi*dateindex/365)+sin(2*pi*dateindex/365)+cos(2*pi*dateindex*2/365)+sin(2*pi*dateindex*2/365)+cos(2*pi*dateindex*3/365)+sin(2*pi*dateindex*3/365)
                 +air7day+log(discharge), cor=corAR1(form=~1), data=means,na.action=na.exclude)
Tp.allModel  <- predict(allModel)
mse.allModel <- mean((Tp.allModel-means$watertemp)^2,na.rm=TRUE)

########## Step 5: Plot the Two Models for Visual ##########
# Plot the predicted temperatures with the estimated time series T
windows()
par(oma = c(3, 2, 4, 2))
par(mar = c(4, 5, 1, 1))
plot(means$dateyear, means$watertemp, type = "l", xlab = "Time (Days)", ylab = expression(paste("Temperature (",degree,"C)")), main = "", cex.lab = 1.6, lwd = 2, col = "black")
lines(means$dateyear, Tp.addComponents, lwd = 3, lty = 3, col="red")
lines(means$dateyear, Tp.allModel, lwd=3, lty=3, col="blue")
mtext(side=3,line=1.5,cex=1.6,"Model Fits")
legend("topleft",c("North Arm Temperatures","Seasonal and Non Seasonal Added","Seasonal with Air and Discharge as Pars"),lwd=2,lty=c(1,3,3),col=c("black","red","blue"),bty="n",cex=0.9)
mtext(side=1,line=1,sprintf("mse add model: %s, mse combined model: %s",round(mse.addComponents,3),round(mse.allModel,3)),outer=TRUE)

# Obtain the final residuals
addComp.resids <- means$watertemp-Tp.addComponents
allMod.resids  <- means$watertemp-Tp.allModel

print("Summary of all model resids")
print(summary(allModel))
