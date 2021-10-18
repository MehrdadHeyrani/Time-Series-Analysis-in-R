################### R codes ########################
#                                                  #
#                     R Book                       #
#                                                  #
#                Mehrdad Heyrani                   #
#          Mehrdad.Heyrani@uottawa.ca              #
#                                                  #
#                Nasim RoshanZamir                 #
#                NRosh052@uottawa.ca               #
####################################################

####################################################
####################################################
####################################################
####################################################
####################################################
####################################################
##########                                ##########
##########         SECTION 1              ##########
##########  Univariate time series        ##########
####################################################
####################################################
####################################################
####################################################
####################################################
####################################################

####################################################
####################################################
##########                                ##########
##########         Chapter 4              ##########
##########    Volatility Modeling         ##########
####################################################
####################################################


################### code 4-1 #######################
library(fGarch)
library(rugarch)
library(FinTS)
library(forecast)
library(quantmod)      

mydata = read.csv("ifbIndex.csv")

y=diff(log(mydata[,2]))*100   # get returns and multiply them by 100


ArchTest (y, lags=12)

Fit=garchFit(~ garch(1,1), data = y,cond.dist="std")

plot(Fit)
par(mfrow = c(1, 1), mar = c(1.9, 4.3, 1.9, .5), mgp = c(2, .6, 0))

################### code 4-2 #######################

predict(Fit, n.ahead = 10,plot=TRUE)

################### code 4-3 #######################

##############################################

predict(Fit, n.ahead = 10,plot=TRUE)

################### code 4-3 #######################

nObs <- length(y)             # Total Number of observations
from <- seq(1,200)            # In sample vector 
to <- seq(201,414)            # Out of sample  vector fo
Vol_vec <- rep(0,(nObs-200))  # Empty vector for storage of 617 Sigma estimates.
Mean_vec <- rep(0,(nObs-200)) # Empty vector for storage of 617 Mean estimates.

for (i in 1:214){
  # The rolling window of 214 observations.
  data_insert <- y[from[i]:to[i]]
  # Fitting an AR(1)-GARCH(1,1) model with normal cond.dist.
  fitted_model <- garchFit(~ arma(1,0) + garch(1,1), data_insert,
                           trace = FALSE,
                           cond.dist = "sstd")
  # One day ahead forecast of conditional standard deviation.
  prediction_model <- predict(fitted_model, n.ahead = 1)
  Mean_vec[i] <- prediction_model$meanForecast
  Vol_vec[i]  <- prediction_model$standardDeviation
  
  if (length(to)-i != 0){
    print(c('just',(length(to) - i),'iterations left'))
  } else {
    print(c('End!'))
  }
}

plot(y[201:414], type = 'l')
lines(Vol_vec, col = 'red', lwd=3, lty=2)
lines(Mean_vec, col = 'blue', lwd=3,lty=10)

legend("topright", legend = c("return","SD(f)", "Mean(f)"),
       col = c("black","red", "blue"),lwd=c(1,2,2), bty="n", lty=c(1,2,10)) 

accuracy(Mean_vec, y[201:413])

################### code 4-4 #######################
library(rugarch)
setwd("E:/BOOK/Data and code") # set in your PC
mydata1 = read.csv("IFB-dayli.csv")
ifb=diff(log(mydata1[,2]))*100 
spec = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)), 
                  mean.model=list(armaOrder=c(0,0), include.mean=TRUE),  
                  distribution.model="norm")

fit = ugarchfit(data =y , spec = spec)
par(mfrow = c(2, 2), mar = c(1.9, 4.3, 1.9, .5), mgp = c(2, .6, 0))

plot(fit)

bootp = ugarchboot(fit, method = c("Full"), out.sample = 10, n.ahead = 200, n.bootpred = 200) 
show(bootp) 
plot(bootp)

fit = ugarchfit(data =y , spec = spec,out.sample =214)
fore=ugarchforecast(fit, data = y, n.ahead = 1, n.roll = 200, out.sample = 214)
fore
plot(fore)

mod1=ugarchroll(spec, data = y, n.ahead = 1,
                n.start =200, refit.window = "moving",
                solver = "hybrid", fit.control = list(),
                keep.coef = TRUE)
plot(mod1)
report(mod1, type="fpm")
