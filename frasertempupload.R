############################
# frasertempupload.r

# Sources a long format of the LOWFRAS.xls data sent by David Patterson
# Creates a date object and then plots

# Created January 26, 2015
# A Putt
#############################    

library(tidyr)
library(dplyr)

tempdata <- read.csv("Data/lowerfrasertemps.csv",head=TRUE,na.strings="-")
tempdata <- tempdata %>% 
  unite(dateyear,date,year,sep="-",remove=FALSE)
tempdata$dateyear <- as.POSIXlt(tempdata$dateyear,format="%d-%B-%Y")

windows()
plot(tempdata$dateyear,tempdata$mean,type="l")
lines(tempdata$dateyear,tempdata$min,type="l",col="blue")
lines(tempdata$dateyear,tempdata$max,type="l",col="red")
