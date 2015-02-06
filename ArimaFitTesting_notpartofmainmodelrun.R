# A bit of AIC
# Random code. Just needs means loaded from the data upload package

d=2

model1 <- arima(means$watertemp,order=c(l,d,0))
model2 <- arima(means$watertemp,order=c(l+1,d,0))
model3 <- arima(means$watertemp,order=c(l+2,d,0))
model4 <- arima(means$watertemp,order=c(l+3,d,0))
model5 <- arima(means$watertemp,order=c(l+4,d,0))
model6 <- arima(means$watertemp,order=c(l+5,d,0))
model7 <- arima(means$watertemp,order=c(l+6,d,0))
model8 <- arima(means$watertemp,order=c(l+7,d,0))
model9 <- arima(means$watertemp,order=c(l+8,d,0))
model10 <- arima(means$watertemp,order=c(l+9,d,0))
AIC(model1,model2,model3,model4,model5,model6,model7,model8,model9,model10)


## i also played around with adding differencing