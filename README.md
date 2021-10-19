# Time-Series-Analysis-in-R
Financial Time Series in R  
# Table of Contents
## introduction 

## Univariate Time Series (ARMA, ARIMA, ARFIMA)
* AR(p) prosess

* MA(q) prosess

* ARMA prosess


```
> aic.x #  AIC matrix for ARMA(p: 0 to 5 , q: 0 to 5)

          [,1]       [,2]        [,3]       [,4]      [,5]       [,6]
[1,]  119.6381   52.33283   -1.854187  -38.11642  -62.4972  -79.67775
[2,] -112.8027 -131.14947 -136.670651 -137.93389 -137.6903 -141.43686
[3,] -149.2428 -112.48586 -141.142849 -135.11775 -135.1125 -135.48990
[4,] -142.0503 -140.25619 -133.511188 -138.15506 -134.0046 -137.78767
[5,] -140.0651 -139.46296 -137.102064 -136.59828 -136.7591 -135.83473
[6,] -138.4259 -122.29046 -137.021627 -134.77720 -136.5486 -133.92958
> bic.x #  BIC matrix for ARMA(p: 0 to 5 , q: 0 to 5)
          [,1]       [,2]       [,3]       [,4]       [,5]       [,6]
[1,]  123.6888   58.40888    6.24722  -27.98966  -50.34509  -65.50029
[2,] -106.7267 -123.04806 -126.54389 -125.78178 -123.51284 -125.23405
[3,] -141.1414 -102.35910 -128.99074 -120.94029 -118.90965 -117.26174
[4,] -131.9235 -128.10408 -119.33373 -121.95225 -115.77647 -117.53415
[5,] -127.9130 -125.28550 -120.89925 -118.37011 -116.50560 -113.55586
[6,] -124.2484 -106.08764 -118.79346 -114.52368 -114.26971 -109.62536

```

### Unit Root Test: 9 steps ADF test, KPSS Test, and ZA Test

```
#### 9 steps ADF Unit Root test 
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

```

* Unit Root Test

```
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

```


### ARIMA modeling 

### ARFIMA with long memory test

<img src="https://user-images.githubusercontent.com/77374087/135507740-fd5b577d-8b5a-4a9a-b5c4-1ccc96bf41c9.png" width="500" height="350">

## univariate time series forecasting and Backtesting 

```
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

> mm2=backtest(fit2,gold,100,1)
[1] "RMSE of out-of-sample forecasts"
[1] 0.001908898
[1] "Mean absolute error of out-of-sample forecasts"
[1] 0.001344186
> mm2=backtest(fit3,gold,100,1)
[1] "RMSE of out-of-sample forecasts"
[1] 0.001918403
[1] "Mean absolute error of out-of-sample forecasts"
[1] 0.001349251
> mm3=backtest(fit3,gold,100,1)
[1] "RMSE of out-of-sample forecasts"
[1] 0.001918403
[1] "Mean absolute error of out-of-sample forecasts"
[1] 0.001349251

```

## Volatility Modeling and Forecasting 

### Rolling Approach
```
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


```
<img src="https://user-images.githubusercontent.com/77374087/133308479-959c9440-3e83-481e-bd5e-014b564b9161.png" width="400" height="250">

### Volatility Forecast evaluation 

```
accuracy(Mean_vec, y[201:413])
                 ME     RMSE      MAE      MPE     MAPE
Test set -0.2543846 1.684526 1.270509 97.90186 153.2406
```

### using Roll function and foprecast fnction from rugarch Package


## Value at Risk


<img src="https://user-images.githubusercontent.com/77374087/137940865-669bdc23-afc2-4e98-ae1d-915db7cf0421.png" width="600" height="250">


<img src="https://user-images.githubusercontent.com/77374087/137941613-35c70ca6-a802-45bf-9e1b-85cf6b73825f.png" width="400" height="300">


## GARCH Value at Risk 
```
library(fGarch)
g = garchFit(~garch(1,1),y,cond.dist = "norm",include.mean =
               FALSE,trace = FALSE) 
omega = g@fit$matcoef[1,1]
alpha = g@fit$matcoef[2,1]
beta =  g@fit$matcoef[3,1]
sigma2 = omega + alpha * y[T]^2 + beta * g@h.t[T]
# compute sigma2 for t+1
VaR9 = sqrt(sigma2) * qnorm(p) * value
VaR9

VaR99=g@sigma.t*qnorm(p)

# Value at Risk plot


VaRplot <- function(var,return) {
  
  qplot(y = var , x = 1:length(var) , geom = 'line') +
    geom_point(aes(x = 1:length(var) , y = return , color = as.factor(var< return)) , size = 3) + 
    scale_color_manual(values = c('red' , 'green'),labels = c("Return<VaR", "Return"))+
    labs(y = 'Daily Returns' , x = 'Test set return',color="---- VaR     " ) + 
    theme(legend.position = "bottom")+
    #theme(legend.position = c(.15, 1),legend.justification = c("right", "top"))+
    ggtitle("VaR forecast")
}

  
  
VaRplot(VaR99,y)
```

<img src="https://user-images.githubusercontent.com/77374087/137942023-017c5f0c-c851-4e1d-b0b6-a16de6c9b221.png" width="500" height="300">

## Rolling Window VaR forecasting 


```
>T = length(y)           # number of observations for return y
>WE = 300                # estimation window length
>p = 0.01                # probability
>l1 = WE * p             # HS observation
>VaR_upp = matrix(nrow=T,ncol=3)    # matrix to hold VaR 
>VaR_dwn = matrix(nrow=T,ncol=3)    # matrix to hold VaR 

for (t in (WE + 1):T){
  t1 = t - WE;              # start of the data window
  t2 = t - 1;               # end of the data window
  window = y[t1:t2]         # data for estimation
  # Normal
  VaR_upp[t,1] = -sd(window)*qnorm(p)
  VaR_dwn[t,1] =  sd(window)*qnorm(p)
  # HS
  ys = sort(window) 
  VaR_upp[t,2] = -ys[l1]   
  VaR_dwn[t,2] =  ys[l1]     
  # GARCH(1,1)
  g=garchFit(formula=~arma(1,1)+garch(1,1),window,trace=FALSE,
             include.mean=FALSE,cond.dist = c("std"))
  par=g@fit$matcoef         
  s4= par[3]+par[4]* window[WE]^2+par[5]* g@h.t[WE]
  VaR_upp[t,3] = -sqrt(s4) * qnorm(p) 
  VaR_dwn[t,3] =  sqrt(s4) * qnorm(p) 
}
>plot(y, type="p", col="gray")
>points(VaR_upp[,1],type="l", col=2, lty=2,lwd=2)
>points(VaR_upp[,2],type="l", col=3, lty=2,lwd=2)
>points(VaR_upp[,3],type="l", col=4, lty=1,lwd=1)
>points(VaR_dwn[,1],type="l", col=2, lty=2,lwd=2)
>points(VaR_dwn[,2],type="l", col=3, lty=2,lwd=2)
>points(VaR_dwn[,3],type="l", col=4, lty=1,lwd=1)
>legend("bottomleft", legend = c("Return","VaR-norm", "VaR-HS", "VaR-GARCH"), col = c("gray","red", "green","blue"), lty = c(1,2,2,1) ,lwd=c(1,2,2,1))

```

<img src="https://user-images.githubusercontent.com/77374087/137941105-9fd2db54-c454-4271-8fa4-ce7be5c90c8e.png" width="500" height="250">


### Backtesting 

```
BacktestVaR(y[301:1046], VaR_dwn[301:1046,3], 0.01)
$LRuc
      Test     Pvalue 
0.02925274 0.86419678 

$LRcc
     Test    Pvalue 
0.1620461 0.9221725 

$AE
[1] 0.9383378
$AD
  ADmean    ADmax 
1.582997 7.544754 
$DQ
$DQ$stat
         [,1]
[1,] 2.694125
$DQ$pvalue
          [,1]
[1,] 0.8461407
$Loss
 [1] 0.0355979
```
