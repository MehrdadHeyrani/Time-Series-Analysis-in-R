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
##########         Chapter 2              ##########
##########                                ##########
####################################################
####################################################

################### code 2-1 #######################
###
library(quantmod)      # Load  package

getSymbols("AAPL")     #Download daily prices Apple
getSymbols("^GSPC")    #Download daily prices S&P 500
getSymbols("AAPL",from="2010-01-02", to="2016-06-06")
getSymbols("^GSPC",from="2010-01-02", to="2016-06-06")

SP500.rtn=diff(log(GSPC$GSPC.Adjusted)) 
AAPL.rtn=diff(log(AAPL$AAPL.Adjusted)) 

plot.ts(AAPL.rtn, SP500.rtn, col="blue")
cor(cbind(AAPL.rtn, SP500.rtn))

################### code 2-2 #######################

# AR simulation with phi=0.7

setseed(1)
y=w=rnorm(100)
for(t in 2:100)y[t] <0.7*y[t-1]+w[t]
op <- par(no.readonly=TRUE)
layout(matrix(c(1,1,2,3),2,2,byrow=TRUE))
plot(y,type="o", lwd = 3, lty = 1, pch = 1, col="green")
acf(y,main='Autocorrelations',ylab="",  ylim=c(-1,1),ci.col ="black")
pacf(y,main='Partial Autocorrelations',ylab="",   ylim=c(-1,1),ci.col ="black")
ar.p=ar(y, method = "mle")
ar.p$order
ar.p$ar
par(op)

################### code 2-3 #######################

# Determining the AR(p) order based on information criteria (AIC)
set.seed(1)
print(ar.p$aic,digits=3)
aic=ar.p$aic
length(aic)
par(mfrow = c(1,1))
plot(c(0:12),aic,type="h",xlab="order",ylab="aic",lwd=5, col=4)
lines(0:12,aic,lty=2, lwd=3,col=2)

################### code 2-4 #######################

# AR(p) modeling for GDP

data=read.table("GDPiran.txt",header=T)
head(data)
d=ts(1:56, start = c(1338, 1), frequency =1)
time = as.Date(d)
gdp=ts(log(data$GDP))
acf(gdp, main="Autocorrelations",lwd=3)
pacf(gdp, main="Partial Autocorrelations",lwd=3)
ar.fit=ar(gdp)
ar.fit$order
aic=ar.fit$aic
length(aic)
par(mfrow = c(3,1))
plot(time,gdp,type="l", lwd = 3, lty = 1, pch = 1, col="blue")

plot(c(0:17),aic,type="h",xlab="order",ylab="aic",lwd=5, col=4, main="AIC order")
lines(0:17,aic,lty=2, lwd=3,col=2)

ar.fit
################### code 2-5 #######################

library("TSA")
eacf(gdp)
fit=arima(gdp, order = c(2, 0, 0),method = c("ML"))
tsdiag(fit)

library(lmtest)
coeftest(fit)
res <- residuals(fit)
tsdisplay(res)
Box.test(fit$residuals, type = c("Box-Pierce"))
Box.test(fit$residuals, type = c( "Ljung-Box"))
library(forecast)
predict(fit, n.ahead = 5)

library(fArma)
armaRoots(fit$coef[1:1])
coeftest(fit)

res <- residuals(armafit)
tsdisplay(res)
################### code 2-6 #######################
#### 9 steps ADF Unit Root test 

library("urca")
data=read.table("Li.txt",header=T)
x <- ts(log(data[,2]), start=c(1357),end=c(1392), frequency=1)
adf.test(x, alternative = "stationary")


####################### step A: (1 & 2 & 3 & 4) ###############################
# Arguments: type = c("none", "drift", "trend") AND selectlags = c("Fixed", "AIC", "BIC")
#drit only intercept
#trend both intercept and trend
unitroot_A<-ur.df(x, lag =2, type = c("trend"), selectlags = c("AIC"))
summary(unitroot_A)
plot(unitroot_A)
unitroot_A@teststat

################### step B: (5&6&7) ###########################
unitroot_B<-ur.df(x, lag =2, type = c("drift"), selectlags = c("AIC"))
summary(unitroot_B)
plot(unitroot_B)
unitroot_B@teststat

################## step C: (8&9) ##############################
unitroot_c<-ur.df(x, lag =2, type = c("none"), selectlags = c("AIC"))
summary(unitroot_c)
plot(unitroot_c)
unitroot_c@teststat

#######################################################
dx=diff(x)  
unitroot_D<-ur.df(dx, lags = 1, type = c("trend"), selectlags = c("AIC"))
summary(unitroot_D)
plot(unitroot4)
unitroot_D@teststat

################### code 2-7 #######################

######### ARIMA modeling 

eacf(dx, ar.max = 3, ma.max = 5)
armafit2=arima(x,order=c(1,1,0),method="CSS-ML")
armafit2
Box.test(armafit2$residuals,lag=2)
res <- residuals(armafit2)
tsdisplay(res)

################### code 2-8 to 11 #######################

# Kwiatkowski-Phillips-Schmidt-Shin (KPSS) Unit Root Test

unitroot_kpss=ur.kpss(x, type = c("mu", "tau"), lags = c("short", "long", "nil"),
                      use.lag = NULL)

summary(unitroot_kpss)

# Elliott, Rothenberg & Stock Unit Root Test

unitroot_ers=ur.ers(x, type = c("DF-GLS", "P-test"), model = c("constant", "trend"),
                    lag.max = 4)
summary(unitroot_ers)

# Zivot and andrews Unit Root Test

unitroot_za=ur.za(x, model = c ("both"), lag=NULL)
summary(unitroot_za)


######################## code 2-12 ################################

# ARFIMA Modeling, Forecasting and Backtesting 

data1=read.table("gold.txt",header=T)
head(data1)
d1=ts(1:198, start = c(1378), frequency =12)
time1 = as.Date(d1)
gold=ts(data1$gold)
plot(time1,gold,type="l", lwd = 3, lty = 1, pch = 1, col="blue")
acf(gold, main="Autocorrelations",lwd=3)
pacf(gold, main="Partial Autocorrelations",lwd=3)
eacf(gold)


#  Long memory
library(fracdiff)
ew=abs(da$vwretd)
# obtain Geweke-Port-Hudak estimate using command fdGPH
m3=fdGPH(abs(gold))
m3
m2=fracdiff(gold,nar=1,nma=1)
summary(m2)
tsdiag(fit)

Box.test(m2$residuals, type = c("Box-Pierce"))
Box.test(m2$residuals, type = c( "Ljung-Box"))
plot(predict(m2, n.ahead = 5), lwd=3, col="dark green")

# model comparison
data1=read.table("gold.txt",header=T)
d1=ts(1:198, start = c(1378), frequency =12)
gold=ts(data1$gold)

# Back testing 
source("backtest.R")
fit2=arima(gold,order=c(1,1,0),season=list(order=c(0,0,0)))
fit3=arima(gold,order=c(2,1,0),season=list(order=c(0,0,0)))
fit4=arima(gold,order=c(3,1,0),season=list(order=c(0,0,0)))

mm2=backtest(fit2,gold,100,1)

mm2=backtest(fit3,gold,100,1)

mm3=backtest(fit3,gold,100,1)


######################## code 2-13 ################################


fit1=arima(gold, order = c(1, 0, 1),method = c("ML"))
fit1
library(lmtest)
coeftest(fit1)
res1 <- residuals(fit1)
tsdisplay(res1)
library(fArma)
armaRoots(fit1$coef[1:1])
Box.test(fit1$residuals, type = c("Box-Pierce"))
Box.test(fit1$residuals, type = c( "Ljung-Box"))
library(forecast)
plot(forecast(fit1,h=5))
predict(fit1, n.ahead = 5)


################################################################################

    


