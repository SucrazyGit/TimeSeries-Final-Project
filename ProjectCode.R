#############
## Imports ##
#############

##library('forecast')
library('tswge')
library('tidyr')
library('dplyr')
library('nnfor')
library('vars')
library('RColorBrewer')

data = read.csv("https://raw.githubusercontent.com/SucrazyGit/TS-Final-Project/main/Turbine_Data.csv")

######################
## Data Preparation ##
######################

## Data Importing ##
data.new = data[seq(1, nrow(data), 6), ] ## Getting Data Collected Every Hour
wind = data.new[19405:19704,] ## Getting only relevant data

## Data Setup ##
wsTrain = wind[1:276,]
wsTest = wind[277:300,]
wlTrain = wind[1:228,]
wlTest = wind[229:300,]

#########
## EDA ##
#########

## Main Plots ##
plotts.wge(wind$ActivePower, ylab = "Power", main = "Hourly Wind Turbine Power Data")
plotts.wge(wind$WindSpeed, ylab = "Wind Speed", main = "Hourly Wind Turbine Wind Speed Data")

## Sample Plots ##
plotts.sample.wge(wind$ActivePower)
plotts.sample.wge(wind$WindSpeed)

## ACF and Spectral Density Plots ##
acf(wind$ActivePower)
parzen.wge(wind$ActivePower)


#########################
## Wind Speed Analysis ##
#########################

## Small Term Predictions ##
ws.ws.mlp.fit = mlp(ts(wsTrain$WindSpeed, frequency = 24), reps = 50, comb = "median")
ws.ws.mlp.fore = forecast(ws.ws.mlp.fit, h = 24)
plotts.wge(ws.ws.mlp.fore$mean)

## Large Term Predictions ##
wl.ws.mlp.fit = mlp(ts(wlTrain$WindSpeed, frequency = 24), reps = 50, comb = "median")
wl.ws.mlp.fore = forecast(wl.ws.mlp.fit, h = 72)
plotts.wge(wl.ws.mlp.fore$mean)

######################################
## Active Power Univariate Analysis ##
######################################

## Root Calculations and Seasonality Plots ##
factor.wge(c(rep(0,23),1)) 
factor.wge(wind$ActivePower)
wind.s24 = artrans.wge(wind$ActivePower, c(rep(0,23), 1))
plotts.sample.wge(wind.s24)
acf(wind.s24,lag = 30) 

#### Small Term Predictions ####

## ARMA(0,0,0) s=12 Model ##
wsTrain.s24 = artrans.wge(wsTrain$ActivePower, c(rep(0,23), 1)) ## Removing Seasonality of 24
wsTrain.s24.aic = aic.wge(wsTrain.s24, type = 'bic')  ## AIC and BIC pick AR(1)
wsTrain.s24.aic
## ARMA(0,0,0) s=12 Forecast ##
wsTrain.arima.fore = fore.arima.wge(wsTrain$ActivePower, s = 24, n.ahead = 24)
## ARMA(0,0,0) s=12 Metrics ##
ws.arima.mse = mean((wsTest$ActivePower - wsTrain.arima.fore$f) ^2)
ws.arima.mse
roll.win.rmse.wge(wind$ActivePower, horizon = 24, s=24)
## ARMA(0,0,0) s=12 Plots ##
plot(seq(1,300,1), wind$ActivePower, type = "l",xlim = c(0,300), ylab = "Power", main = "24 Hour Power Forecast")
lines(seq(277,300,1), wsTrain.arima.fore$f, type = "l", col = "red")

## ARMA(1,0,0) s=12 Model ##
wsTrain.s24 = artrans.wge(wsTrain$ActivePower, c(rep(0,23), 1)) ## Removing Seasonality of 24
wsTrain.s24.aic = aic.wge(wsTrain.s24, type = 'bic')  ## AIC and BIC pick AR(1)
wsTrain.s24.aic
## ARMA(1,0,0) s=12 Forecast ##
wsTrain.arima.fore = fore.arima.wge(wsTrain$ActivePower, p = wsTrain.s24.aic$p, s = 24, n.ahead = 24)
## ARMA(1,0,0) s=12 Metrics ##
ws.arima.mse = mean((wsTest$ActivePower - wsTrain.arima.fore$f) ^2)
ws.arima.mse
roll.win.rmse.wge(wind$ActivePower, p = wsTrain.s24.aic$p, horizon = 24, s=24)
## ARMA(1,0,0) s=12 Plots ##
plot(seq(1,300,1), wind$ActivePower, type = "l",xlim = c(0,300), ylab = "Power", main = "24 Hour Power Forecast")
lines(seq(277,300,1), wsTrain.arima.fore$f, type = "l", col = "red")

#### Large Term Predictions ####

## ARMA(0,0,0) s=12 Model ##
wlTrain.s24 = artrans.wge(wlTrain$ActivePower, c(rep(0,23), 1)) ## Removing Seasonality of 24
wlTrain.s24.aic = aic.wge(wlTrain.s24, type = 'bic')  ## AIC and BIC pick AR(1)
wlTrain.s24.aic
## ARMA(0,0,0) s=12 Forecast ##
wlTrain.arima.fore = fore.arima.wge(wlTrain$ActivePower, s = 24, n.ahead = 72)
## ARMA(0,0,0) s=12 Metrics ##
wl.arima.mse = mean((wlTest$ActivePower - wlTrain.arima.fore$f) ^2)
wl.arima.mse
roll.win.rmse.wge(wind$ActivePower, horizon = 72, s=24)
## ARMA(0,0,0) s=12 Plots ##
plot(seq(1,300,1), wind$ActivePower, type = "l",xlim = c(0,300), ylab = "Power", main = "72 Hour Power Forecast")
lines(seq(229,300,1), wlTrain.arima.fore$f, type = "l", col = "red")

## ARMA(1,0,0) s=12 Model ##
wlTrain.s24 = artrans.wge(wlTrain$ActivePower, c(rep(0,23), 1)) ## Removing Seasonality of 24
wlTrain.s24.aic = aic.wge(wlTrain.s24, type = 'bic')  ## AIC and BIC pick AR(1)
wlTrain.s24.aic
## ARMA(1,0,0) s=12 Forecast ##
wlTrain.arima.fore = fore.arima.wge(wlTrain$ActivePower, phi = wlTrain.s24.aic$p, s = 24, n.ahead = 72)
## ARMA(1,0,0) s=12 Metrics ##
wl.arima.mse = mean((wlTest$ActivePower - wlTrain.arima.fore$f) ^2)
wl.arima.mse
roll.win.rmse.wge(wind$ActivePower, phi = wlTrain.s24.aic$p, horizon = 72, s=24)
## ARMA(1,0,0) s=12 Plots ##
plot(seq(1,300,1), wind$ActivePower, type = "l",xlim = c(0,300), ylab = "Power", main = "72 Hour Power Forecast")
lines(seq(229,300,1), wlTrain.arima.fore$f, type = "l", col = "red")

###############################
## Active Power VAR Analysis ##
###############################

#### Small Term Predictions ####

## VAR Model Setup ##
wsTrain.ap = wsTrain$ActivePower
wsTrain.ws = wsTrain$WindSpeed
ws.train.df = c(wsTrain.ap,wsTrain.ws)
## VAR Model Selection ##
VARselect(ws.train.df, lag.max = 10, type= "both", season = 24, exogen = NULL) # AIC picked p = 6
ws.var = VAR(cbind(wsTrain.ap, wsTrain.ws), season = 24, type = "both", p = 6)
## VAR Model Forecast ##
ws.var.fore = predict(ws.var, n.ahead = 24)
## VAR Model Metrics ##
ws.var.mse = mean((wsTest$ActivePower - ws.var.fore$fcst$wsTrain.ap[1:24,1]) ^2)
ws.var.mse
## VAR Model Plots ##
plot(seq(1,300,1), wind$ActivePower, type = "l",xlim = c(0,300), ylab = "Power", main = "24 Hour Power Forecast")
points(seq(277,300,1), ws.var.fore$fcst$wsTrain.ap[1:24,1], type = "l", col = "red")
fanchart(ws.var.fore, colors = brewer.pal(n=8, name="Reds"))
fanchart()

#### Large Term Predictions ####

## VAR Model Setup ##
wlTrain.ap = wlTrain$ActivePower
wlTrain.ws = wlTrain$WindSpeed
wl.train.df = c(wlTrain.ap, wlTrain.ws)
## VAR Model Selection ##
VARselect(wl.train.df, lag.max = 10, type= "both", season = 24, exogen = NULL) # AIC picked p = 6
wl.var = VAR(cbind(wlTrain.ap, wlTrain.ws), season = 24, type = "both", p = 6)
ws.var = VAR(cbind(wsTrain.ap, wsTrain.ws), season = 24, type = "both", p = 6)
## VAR Model Forecast ##
wl.var.fore = predict(wl.var, n.ahead = 72)
## VAR Model Metrics ##
wl.var.mse = mean((wlTest$ActivePower - wl.var.fore$fcst$wlTrain.ap[1:72,1]) ^2)
wl.var.mse
## VAR Model Plots ##
plot(seq(1,300,1), wind$ActivePower, type = "l",xlim = c(0,300), ylab = "Power", main = "72 Hour Power Forecast")
points(seq(229,300,1),wl.var.fore$fcst$wlTrain.ap[1:72,1], type = "l", col = "red")

##########################################
## Active Power Neural Network Analysis ##
##########################################

#### Small Term Predictions ####

## NN Model Setup ##
ws.train.df = data.frame(windspeed = ts(wsTrain$WindSpeed))
## NN Model Building ##
ws.train.fore.df = data.frame(part = ts(c(wsTrain$WindSpeed, ws.ws.mlp.fore$mean)))
ws.mlp.fit = mlp(ts(wsTrain$ActivePower, frequency = 24), reps = 50, comb = "median", xreg = ws.train.df) # 5 hidden node is best using rmse
ws.mlp.fit
plot(ws.mlp.fit)
## NN Model Forecast ##
ws.mlp.fore = forecast(ws.mlp.fit, h = 24, xreg = ws.train.fore.df)
## NN Model Metrics ##
ws.mlp.mse = mean((wsTest$ActivePower - ws.mlp.fore$mean) ^2)
ws.mlp.mse
## NN Model Plots ##
plot(seq(1,300,1), wind$ActivePower, type = "l",xlim = c(0,300), ylab = "Power", main = "24 Hour Power Forecast")
points(seq(277,300,1), ws.mlp.fore$mean, type = "l", col = "red")

#### Large Term Predictions ####

## NN Model Setup ##
wl.train.df = data.frame(windspeed = ts(wlTrain$WindSpeed))
## NN Model Building ##
wl.train.fore.df = data.frame(part = ts(c(wlTrain$WindSpeed, wl.ws.mlp.fore$mean)))
wl.mlp.fit = mlp(ts(wlTrain$ActivePower, frequency = 24), reps = 50, hd=4, comb = "median",xreg = wl.train.df) # 4 hidden node is best using rmse
wl.mlp.fit
plot(wl.mlp.fit)
## NN Model Forecast ##
wl.mlp.fore = forecast(wl.mlp.fit, h = 72, xreg = wl.train.fore.df)
## NN Model Metrics ##
wl.mlp.mse = mean((wlTest$ActivePower - wl.mlp.fore$mean) ^2)
wl.mlp.mse
## NN Model Plots ##
plot(seq(1,300,1), wind$ActivePower, type = "l",xlim = c(0,300), ylab = "Power", main = "72 Hour Power Forecast")
points(seq(229,300,1), wl.mlp.fore$mean, type = "l", col = "red")

####################################
## Active Power Ensemble Analysis ##
####################################

#### Small Term Predictions ####

## Ensemble Model Forecast ##
ws.ens.fore  = (ws.var.fore$fcst$wsTrain.ap[,1] + ws.mlp.fore$mean)/2
## Ensemble Model Metrics ##
ws.ens.mse = mean((wsTest$ActivePower - ws.ens.fore) ^2)
ws.ens.mse
## Ensemble Model Plots ##
plot(seq(1,300,1), wind$ActivePower, type = "l",xlim = c(0,300), ylab = "Power", main = "24 Hour Power Forecast")
points(seq(277,300,1), ws.ens.fore, type = "l", col = "red")

#### Large Term Predictions ####

## Ensemble Model Forecast ##
wl.ens.fore  = (wl.var.fore$fcst$wlTrain.ap[,1] + wl.mlp.fore$mean)/2
## Ensemble Model Metrics ##
wl.ens.mse = mean((wlTest$ActivePower - wl.ens.fore) ^2)
wl.ens.mse
## Ensemble Model Plots ##
plot(seq(1,300,1), wind$ActivePower, type = "l",xlim = c(0,300), ylab = "Power", main = "72 Hour Power Forecast")
points(seq(229,300,1), wl.ens.fore, type = "l", col = "red")

####################
## Final Forecast ##
####################

## ARMA(0,0,0) s=12 Model Forecast ##
final.arimaOne.fore = fore.arima.wge(wind$ActivePower, s = 24, n.ahead = 72)

## ARMA(1,0,0) s=12 Model Forecast ##
final.s24 = artrans.wge(wind$ActivePower, c(rep(0,23), 1))
final.s24.aic = aic.wge(final.s24, type = 'bic')
final.arimaTwo.fore = fore.arima.wge(wind$ActivePower, phi = final.s24.aic$p, s = 24, n.ahead = 72)

## Var Model Forecast ##
final.ap = wind$ActivePower
final.ws = wind$WindSpeed
final.train.df = c(final.ap, final.ws)
final.var = VAR(cbind(final.ap, final.ws), season = 24, type = "both", p = 6)
final.var.fore = predict(final.var, n.ahead = 72)

## Wind Speed Forecast ##
final.ws.mlp.fit = mlp(ts(wind$WindSpeed, frequency = 24), reps = 50, comb = "median")
final.ws.mlp.fore = forecast(final.ws.mlp.fit, h = 72)

## MLP Model Building and Forecast ##
final.train.df = data.frame(windspeed = ts(wind$WindSpeed))
final.train.fore.df = data.frame(part = ts(c(wind$WindSpeed, final.ws.mlp.fore$mean)))
final.mlp.fit = mlp(ts(wind$ActivePower, frequency = 24), reps = 50, hd=4, comb = "median", xreg = final.train.df)
final.mlp.fore = forecast(final.mlp.fit, h = 72, xreg = final.train.fore.df)

## Ensemble Model Forecast ##
final.ens.fore  = (final.var.fore$fcst$final.ap[,1] + final.mlp.fore$mean)/2

## Final Plot ##
plot(seq(1,300,1), wind$ActivePower, type = "l",xlim = c(0,372), ylab = "Power", xlab = "Time", main = "72 Hour Power Forecast")
points(seq(301,372,1), final.arimaTwo.fore$f, type = "l", col = "blue", lwd=1.5)
points(seq(301,372,1), final.var.fore$fcst$final.ap[1:72,1], type = "l", col = "green", lwd=1.5)
points(seq(301,372,1), final.mlp.fore$mean, type = "l", col = "purple", lwd=1.5)
points(seq(301,372,1), final.ens.fore, type = "l", col = "orange", lwd=1.5)
points(seq(301,372,1), final.arimaOne.fore$f, type = "l", col = "red", lwd=2)
