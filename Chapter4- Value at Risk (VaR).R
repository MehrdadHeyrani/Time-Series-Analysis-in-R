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
##########         Chapter 5              ##########
##########        Value at Risk           ##########
####################################################
####################################################


################### code 5-1 #######################
library(fGarch)
library(rugarch)
library(FinTS)
library(forecast)
setwd("E:/BOOK/Data and code")
mydata = read.csv("IFB-dayli.csv")
y=diff(log(mydata[,2]))*100  

T = length(y)  #length of data
value = 100    # portfolio value
p = 0.01       # probability
##############################################
########## univariate HS in R ################
##############################################
ys = sort(y)          # sort returns
op = T*p              # p % smallest
VaR1 = -ys[op]*value  # VaR number
VaR1
ES1 = -mean(ys[1:op]) * value
ES1
##############################################
######### univariate Normal in R #############
##############################################
sigma= sd(y)         # estimate the volatility
VaR3 =-sigma*qnorm(p)*value # calculate the VaR
VaR3
##############################################
########### univariate MA in R ###############
##############################################
#forecast time-dependent volatility is to use the MA volatility model
WE=200
for (t in seq(T-5,T)){
  t1= t-WE+1
  window = y[t1:t] # estimation window
  sigma = sd(window)
  VaR6 = -sigma * qnorm(p) * value
}
VaR6
###########################################################
library(fGarch)
g = garchFit(~garch(1,1),y,cond.dist = "norm",include.mean =
               FALSE,trace = FALSE) 
omega = g@fit$matcoef[1,1]
alpha = g@fit$matcoef[2,1]
beta = g@fit$matcoef[3,1]
sigma2 = omega + alpha * y[T]^2 + beta * g@h.t[T]
# compute sigma2 for t+1
VaR9 = -sqrt(sigma2) * qnorm(p) * value
VaR9


################### code 5-1 #######################
library(fGarch)
library(rugarch)
library(FinTS)
library(tseries)
library(forecast)
setwd("E:/BOOK/Data and code")
mydata = read.csv("IFB-dayli.csv")
y=diff(log(mydata[,2]))*100
adf.test(y, alternative = c("stationary"))
ArchTest (y, lags=12)
par(mfrow = c(1, 1), mar = c(1.9, 4.3, 1.9, .5), mgp = c(2, .6, 0))
qqnorm(y)
qqline(y, distribution = qnorm)
auto.arima(y, max.p = 5, max.q = 5)
T = length(y)                   # number of observations for return y
WE = 300                        # estimation window length
p = 0.01                        # probability
l1 = WE * p                     # HS observation
VaR_upp = matrix(nrow=T,ncol=3) # matrix to hold VaR forecasts
VaR_dwn = matrix(nrow=T,ncol=3) # matrix to hold VaR forecasts

for (t in (WE + 1):T){
  t1 = t - WE;              # start of the data window
  t2 = t - 1;               # end of the data window
  window = y[t1:t2]         # data for estimation
  # Normal
  VaR_upp[t,1] = -sd(window) * qnorm(p)
  VaR_dwn[t,1] =  sd(window) * qnorm(p)
  # HS
  ys = sort(window) 
  VaR_upp[t,2] = -ys[l1]   
  VaR_dwn[t,2] =  ys[l1]     
  # GARCH(1,1)
  g=garchFit(formula = ~garch(1,1), window ,trace=FALSE,
             include.mean=FALSE,cond.dist = c("norm"))
  par=g@fit$matcoef         
  s4=par[1]+par[2]* window[WE]^2+par[3]* g@h.t[WE]
  VaR_upp[t,3] = -sqrt(s4) * qnorm(p) 
  VaR_dwn[t,3] =  sqrt(s4) * qnorm(p) 
}


W1 = WE+1
for (i in 1:3){
  VR = sum(y[W1:T] < -VaR_upp[W1:T,i])/(p*(T - WE))
  s = sd(VaR_upp[W1:T,i]) # VaR volatility
  cat(i,"VR",VR,"VaR vol",s,"\n") # print results
}
matplot(cbind(y[W1:T],VaR_upp[W1:T,]),type="l")
par(mfrow = c(1, 1), mar = c(1.9, 1.9, 1.9, .5), mgp = c(2, .6, 0))

plot(y, type="p", col="gray")
points(VaR_upp[,1],type="l", col=1, lty=1,lwd=2)
points(VaR_upp[,2],type="l", col=3, lty=2,lwd=3)
points(VaR_upp[,3],type="l", col=4, lty=1,lwd=1)
points(VaR_dwn[,1],type="l", col=1, lty=1,lwd=2)
points(VaR_dwn[,2],type="l", col=3, lty=2,lwd=3)
points(VaR_dwn[,3],type="l", col=4, lty=1,lwd=1)


legend("bottomleft", legend = c("Return","VaR-norm", "VaR-HS", "VaR-GARCH"),
       col = c("dark gray","red", "green","blue"), lty = c(1,2,2,1)
       ,lwd=c(1,2,2,1), bty="n")

###
#########################################################
#   Bernoulli coverage test

bern_test = function(p,v){
  a=p^(sum(v))*(1 - p)^(length(v)- sum(v))
  b = (sum(v)/length(v))^(sum(v))*(1 - (sum(v)/length(v)))^
    (length(v) - sum(v))
  return(-2*log(a/b))
}

########################################################
# Independence coverage test
ind_test = function(V){
  J = matrix(ncol = 3,nrow = length(V))
  for (i in 2:length(V)){
    J[i,1] = V[i - 1] == 0 & V[i] == 0
    J[i,2] = V[i - 1] ==0 & V[i] == 1
    J[i,3] = V[i - 1] == 1 & V[i] == 0
    
  }
  V_00 = sum(J[,1],na.rm = TRUE)
  V_01 = sum(J[,2],na.rm = TRUE)
  V_10 = sum(J[,3],na.rm = TRUE)
  p_00 = V_00/(V_00 + V_01)
  p_01 = V_01/(V_00 + V_01)
  p_10 = V_10/(V_10 + V_01)
  hat_p = (V_01)/(V_00 + V_01 + V_10 )
  a = (1 - hat_p)^(V_00 + V_10)*(hat_p)^(V_01 )
  b = (p_00)^(V_00)*(p_01)^(V_01)*(p_10)^(V_10)
  return(-2 * log(a/b))
}

##########################################################
W1 = WE + 1
ya = y[W1:T]
VaRa = VaR_dwn[W1:T,]
m = c("Normal","HS","GARCH")
for (i in 1:3){
  q = y[W1:T]< -VaR_dwn[W1:T,i]
  v = VaRa*0
  v[q,i] = 1
  ber = bern_test(p,v[,i])
  ind = ind_test(v[,i])
  cat(i,m[i],"Bernoulli",ber,1 - pchisq(ber,1),"independence",
      ind,1 - pchisq(ind,1),"\n")
}



#####################################3

BacktestVaR(y[301:1046], VaR_dwn[301:1046,3], 0.01)

VaRDurTest(0.01, y[301:1046], VaR_dwn[301:1046,3])
sum(VaRloss(0.01, y[301:1046], VaR_dwn[301:1046,3]))/(length(y)-300)/100
loss= VaRloss(0.01, y[301:1046], VaR_dwn[301:1046,3])
sum((loss)/(length(y)- WE))/100

####################################################################

mydata1 = read.csv("IFB-dayli.csv")
ifb=diff(log(mydata1[,2]))*100 
ifb=ts(mydata1[,3])
spec = ugarchspec(variance.model=list(model="eGARCH", garchOrder=c(1,1)), 
                  mean.model=list(armaOrder=c(1,1), include.mean=TRUE),  
                  distribution.model="std")

mod1=ugarchroll(spec, data = ifb, n.ahead = 1,
                n.start =300, refit.window = "moving",
                solver = "hybrid", fit.control = list(),
                keep.coef = TRUE,
                calculate.VaR = TRUE, VaR.alpha = c(0.01))
par(mfrow = c(1, 2))

plot(mod1)
report(mod1, type = "VaR", VaR.alpha = 0.01, conf.level = 0.95)

#####################################
mydata1 = read.csv("IFB-dayli.csv")
y=diff(log(mydata1[,2]))*100 
y=mydata1[2:1047,3]
dates     = mydata1[2:1047,1]
dates     = as.matrix(dates)
labels    = as.numeric(format(as.Date(dates, "%Y-%m-%d"), "%Y"))
where.put = c(which(diff(labels) == 1) + 1)
t1 <- timeDate(dates)
R=xts(y,t1)

Uv1=uvGARCH(R, model = "sGARCH", garchOrder = c(1, 1), armaOrder = c(1, 1),
            distribution = "norm")
Uv2=uvGARCH(R, model = "sGARCH", garchOrder = c(1, 1), armaOrder = c(1, 1),
            distribution = "std")
Uv3=uvGARCH(R, model = "eGARCH", garchOrder = c(1, 1), armaOrder = c(1, 1),
            distribution = "norm")
Uv4=uvGARCH(R, model = "eGARCH", garchOrder = c(1, 1), armaOrder = c(1, 1),
            distribution = "std")
Uv5=uvGARCH(R, model = "gjrGARCH", garchOrder = c(1, 1), armaOrder = c(1, 1),
            distribution = "norm")
Uv6=uvGARCH(R, model = "gjrGARCH", garchOrder = c(1, 1), armaOrder = c(1, 1),
            distribution = "std")
Uv7=uvGARCH(R, model = "apARCH", garchOrder = c(1, 1), armaOrder = c(1, 1),
            distribution = "norm")
Uv8=uvGARCH(R, model = "apARCH", garchOrder = c(1, 1), armaOrder = c(1, 1),
            distribution = "std")

back1=backtestVaR.GARCH(Uv1, p = c( 0.99), nAhead = 1,
                        refitEvery = 25,
                        window = 300)
back2=backtestVaR.GARCH(Uv2, p = c( 0.99), nAhead = 1,
                        refitEvery = 25,
                        window = 300)
back3=backtestVaR.GARCH(Uv3, p = c( 0.99), nAhead = 1,
                        refitEvery = 25,
                        window = 300)
back4=backtestVaR.GARCH(Uv4, p = c( 0.99), nAhead = 1,
                        refitEvery = 25,
                        window = 300)
back5=backtestVaR.GARCH(Uv5, p = c( 0.99), nAhead = 1,
                        refitEvery = 25,
                        window = 300)
back6=backtestVaR.GARCH(Uv6, p = c( 0.99), nAhead = 1,
                        refitEvery = 25,
                        window = 300)
back7=backtestVaR.GARCH(Uv7, p = c( 0.99), nAhead = 1,
                        refitEvery = 25,
                        window = 300)

back8=backtestVaR.GARCH(Uv8, p = c( 0.99), nAhead = 1,
                        refitEvery = 25,
                        window = 300)


VaRs = matrix(nrow=500,ncol=8)
Rback=R[547:1046]
VaRs[,1]= getVaREstimates(back1)
VaRs[,2]= getVaREstimates(back2)
VaRs[,3]= getVaREstimates(back3)
VaRs[,4]= getVaREstimates(back4)
VaRs[,5]= getVaREstimates(back5)
VaRs[,6]= getVaREstimates(back6)
VaRs[,7]= getVaREstimates(back7)
VaRs[,8]= getVaREstimates(back8)

BacktestVaR(Rback,Vars[,], 0.01)$Loss$Loss
VaRDurTest(0.01, Rback,VVaRR)$Decision

Test.ifb=matrix(nrow=8,ncol=9)
for (i in 1:8) 
{
  Test.ifb[i,1]=BacktestVaR(Rback,VaRs[,i], 0.01)$LRuc[1:1]
  Test.ifb[i,2]=BacktestVaR(Rback,VaRs[,i], 0.01)$LRuc[2:2]
  Test.ifb[i,3]=BacktestVaR(Rback,VaRs[,i], 0.01)$LRcc[1:1]
  Test.ifb[i,4]=BacktestVaR(Rback,VaRs[,i], 0.01)$LRcc[2:2]
  Test.ifb[i,5]=BacktestVaR(Rback,VaRs[,i], 0.01)$DQ$stat
  Test.ifb[i,6]=BacktestVaR(Rback,VaRs[,i], 0.01)$DQ$pvalue
  Test.ifb[i,7]=BacktestVaR(Rback,VaRs[,i], 0.01)$Loss$Loss
  Test.ifb[i,8]=VaRDurTest(0.01, Rback,VaRs[,i])$LRp
  Test.ifb[i,9]=VaRDurTest(0.01, Rback,VaRs[,i])$Decision
}


write.csv(Test.ifb, file = "Test.ifb.csv")

VVaRR=getVaREstimates(back1)
viol=getVaRViolations(back1)
par(mfrow = c(2, 1), mar = c(1.9, 1.9, 1.9, .5), mgp = c(2, .6, 0))

plot(back2,t2, pch=18, legendLoc="topright")

BacktestVaR(Rback,VVaRR, 0.01)

##########################################
library(AER)
library(fGarch)
data(NYSESW)
head(NYSESW)
dates     = mydata1[2:1047,1]
dates     = as.matrix(dates)
labels    = as.numeric(format(as.Date(dates, "%Y-%m-%d"), "%Y"))
where.put = c(which(diff(labels) == 1) + 1)

t1 <- timeDate(dates)
Ret <- timeSeries(-1.0 * diff(log(mydata1[,2])) * 100,t1)

## Function for ES of t-GARCH
ESgarch <- function(y, p = 0.99){
  gfit <- garchFit(formula = ~garch(1, 1), data = y,
                   cond.dist = "std", trace = FALSE)
  sigma <-  predict(gfit, n.ahead = 1)[3]
  df <- coef(gfit)["shape"]
  ES <- sigma * (dt(qt(p, df), df)/(1 - p)) *
    ((df + (qt(p, df))^2)/(df - 1))
  return(ES)
}
## Date vectors for backtest
from <- time(Ret)[-c((nrow(Ret) - 299) : nrow(Ret))]
to <- time(Ret)[-c(1:300)]
NYSEES <- fapply(Ret, from = from, to = to, FUN = ESgarch)
NYSEESL1 <- lag(Ret, k = 1)
res <- na.omit(cbind(NYSELOSS, NYSEESL1))
colnames(res) <- c("IFB.ret", "ES99")

par(mfrow = c(1, 1), mar = c(1.9, 1.9, 1.9, .5), mgp = c(2, .6, 0))

plot(res[, 2], col = "green", ylim = range(res),
     main = "IFB: t-GARCH(1,1) ES 99%",
     ylab = "percentages", xlab = "", lwd=1)

points(res[, 1], type = "p", cex = 0.7, pch = 19
       ,col=ifelse(res[, 1]>=res[, 2], "red", "dark gray"))

legend("topleft", legend = c("Loss","Loss>ES", "ES"),
       col = c("dark gray","red", "green"), lty = c(NA,NA, 1)
       ,pch = c(19, 19, NA),lwd=c(1,1,2), bty="n") # bty for text border 


