############################
# TempModelRunCode

# Provides minimal instructions for running the model and includes a to do list and 
# relevant model notes
# Model is sourced from this file

# Created February 6, 2015
# A Putt
############################# 

# To Do List:
# I don't consider leap years in this model; I just remove the 29th of Feb whenever it occurs
# I really don't like how I add the correlation. My final phi is very large, which suggests that something is wrong.

# Notes: 

##########
# Model source code
# Source the libraries used for model. Libraries can also be installed if necessary.
source("TempModelLibraries.R")

# All functions for the temperature model are stored in this function file
source("TempModelFunctions.R")

# Data are stored in the Data subdirectory, and are sourced and manipulated in this file
# Once we decide what exactly is wanted, we can create a way to upload water temp, air temp,
# and discharge data from different sources that will then be sent to the model.
# For now the code is very specific to the raw data I had to work with
source("TempModelDataUpload.R")

# This is the meat of the model. Functions are pulled from the function tile and data are
# brought in from the data upload file
# The model is broken into 5 steps:

# Step 1: Assess the normality of inputs and model the seasonal component
 # Output will be 3 plots showing the normality of inputs, 1 plot showing
 # model diagnosics for the three harmonic gls of temperature with an ar1 correlation structure
 # and three plots showing the model fits to the original temp data
# Step 2: Model the residuals of the different harmonic models (correlation structure of ar1 as well) 
 # and select the best model via AIC
 # This step models the non-seasonal variability
 # Output will be 3 plots showing normality of harmonic model residuals to be modeled for non-seasonality,
 # AIC table, and model diagnostics of the best model (i.e., rolling avg of air temp resids vs water temp resids)
 # Best model selection is not currently automated. May not ever need to be if the model isn't being used in multiple instances
# Step 3: Combine predictions of the two models and calculate mse
# Step 4: Model the harmonics, autocorrelation, avg air temperatures, and discharges in one mle
# Step 5: Plot the two models to give visual and print mse values onto the plot
source("TempModel.R")
