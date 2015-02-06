############################
# TempModelDataUpload.r

# Manipulates historic water temperature, air temperature, and discharge data for modelling
# For all of the files I do quite a bit of manipulation on dates, date indexes, etc. 
# Even where the column already existed I sometimes had to alter formats, etc. 
# This makes the code seem longer and more complicated than it really is.

# Created January 26, 2015
# A Putt
############################# 

##########
# Read in the temperature data from North Arm and Patullo Bridge
# North Arm going from 1954 to 1962; Pattullo Bridge goes from 1963 to 1993. 
# There is a bit of everlap data, but I have removed it and stored it in the excel file
temp <- read.csv("Data/lowerfrasertemps.csv",head=TRUE,na.strings="-")
temp <- temp %>% 
  unite(dateyear,date,year,sep="-",remove=FALSE)
temp$dateyear <- as.POSIXlt(temp$dateyear,format="%d-%B-%Y")
temp$date     <- as.factor(as.character(temp$dateyear,format="%d-%b")) # Need to rewrite this column to include zeros before single digit numbers for joining later on.

##########
# Read in the discharge data from Hope (1912-2012)
discharge          <- read.csv("Data/FRhopeDischarge.csv",header=TRUE)
discharge$dateyear <- as.POSIXlt(discharge$date,format="%Y-%m-%d")
discharge$date     <- as.factor(as.character(discharge$dateyear,format="%d-%b"))
discharge$year     <- as.character(discharge$dateyear,format="%Y")

##########
# Read in the air temperature data from Kamloops
air <- read.csv("Data/KamloopsHistAir.csv",head=TRUE, check.names=FALSE)
names(air)[1] <- "date"
air <- air %>%
  gather("year","mean",2:ncol(air)) %>%
  unite(dateyear,date,year,sep="-",remove=FALSE)
air$dateyear <- as.POSIXlt(air$dateyear,format="%d-%B-%Y")
air$year     <- as.integer(as.character(air$year)) 
air$date     <- as.factor(as.character(air$dateyear,format="%d-%b"))

##########
# Create a new table that just has the factor levels and the date index so that date index can be added to the data tables
Year      <- seq(as.Date("1911/1/1"),as.Date("1911/12/31"),"days")
DateIndex <- data.frame(date=as.factor(as.character(Year,format="%d-%b")),dateindex=seq(1,365,1))
LeapYear  <- data.frame(date=as.factor(as.character("29-Feb",format="%d-%b")),dateindex=366)
DateIndex <- rbind(DateIndex,LeapYear) # Note that the leap year isn't in order. I think for now I'm going to delete all Feb 29 for simplification

##########
# Add the date index to all of the tables
# For some reason left join isn't working on this machine so I used merge, which requires a re-order after
air       <- merge(air,DateIndex,by="date",all.x=TRUE)
discharge <- merge(discharge,DateIndex,by="date",all.x=TRUE)
temp      <- merge(temp,DateIndex,by="date",all.x=TRUE)
# Now I reorder, but note that this will mess up the placement of Feb 29ths due to their date index of 366
air       <- air[order(air$year,air$dateindex),]
discharge <- discharge[order(discharge$year,discharge$dateindex),]
temp      <- temp[order(temp$year,temp$dateindex),]

# Remove Feb 29 from all of the frames for now
air       <- subset(air,date != "29-Feb")
discharge <- subset(discharge, date != "29-Feb")
temp      <- subset(temp,date!="29-Feb")

##########
# Make all three data tables the same length so that they can safely be appended together and modeled.
# Can use the minimum and maximum years (non NA) of whatever table you want using the below code, but dates must be ordered ascending
MinimumYear <- max(c(air$year[which(!is.na(air$mean))[1]],discharge$year[which(!is.na(discharge$discharge))[1]]))
last <- function(x) { tail(x,n=1) }
MaximumYear <- min(c(last(air$year[which(!is.na(air$mean))]),last(discharge$year[which(!is.na(discharge$discharge))])))
# Here I override with my own years for simplicity
MinimumYear <- 1980
MaximumYear <- 1990

# Subset all of the data frames to only have this date range
allAir       <- air # First make backups in case the whole range is needed later
allDischarge <- discharge
allTemp      <- temp
air          <- subset(air,year >= MinimumYear & year <= MaximumYear)
discharge    <- subset(discharge,year >= MinimumYear & year <= MaximumYear)
temp         <- subset(temp,year >= MinimumYear & year <= MaximumYear)

# Create one large data frame for safer modelling
# First I temporarily merge two of them, change column names, and then merge the third 
means.tempframe <- merge(air,discharge,by=c("dateyear","year","date","dateindex"),all=TRUE,sort=TRUE)
names(means.tempframe)[names(means.tempframe)=='mean'] <- 'airtemp'
means <- merge(means.tempframe,temp[,-which(names(temp) %in% c("min","max"))],by=c("dateyear","year","date","dateindex"),all=TRUE,sort=TRUE)
names(means)[names(means)=='mean'] <- 'watertemp'

########## 
# Print any statements to the console that you think might be useful
cat(paste("Fraser River Historic Temperature Modelling for North Arm/Putollo Bridge", "\n",
          "Air temperature data for Kamloops: Data available from 1951 to 2013", "\n",
          "Discharge data from Hope: Data available from 1912 to 2012","\n"))
print(sprintf("Selected data from %s to %s for modelling",MinimumYear,MaximumYear))
