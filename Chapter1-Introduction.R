
################################################################################
################################################################################

#                         Financial Time Series in R

#                             Mehrdad Heyrani
#                       Mehrdad.heyrani@uottawa.ca

#                              Nasim Roshanzamir
#                             nrosh051@uottawa.ca

################################################################################
################################################################################


####################################################
####################################################
##########                                ##########
##########         Chapter 1              ##########
##########                                ##########
####################################################
####################################################

################### code 1-3 #######################
x = 10
x
y = c(1,3,5,7,9)
Y
length(Y)
y = matrix(nrow = 2,ncol = 3)
dim(y)
y = matrix(c(1,2,3),3,1)
seq(1:10)
seq(from = 1, to = 10, by = 2)
seq(from = 1, to = 10, length = 5)

################### code 1-4 #######################
X <- c(1,2,3)
names(X) <- c("Iran","China","Japan")
X
X[2:3]
X["Iran"]
X[1:3]
A <- matrix(1:6, nrow=2)
A
t(A)
dim(A)
nrow(A)
ncol(A)
A[1,]
A[,1]
################### code 1-7 #######################
S=10                           # number of simulations
df=3                           # degees of freedom
rt(S,df)                       # Student-t
rlnorm(S)                      # log-normal
runif(S,min=0,max= )           # uniform
rchisq(S,df)                   # chi-squared
rbinom(S,size=4,prob=0.1)      # binomial
rpois(S,lambda=0.1)            # Poisson
rexp(S,rate=1)                 # exponential
rgamma(S,shape=2,scale=1)      # gamma
rweibull(S,shape=2,scale=1)    # Weibull
rcauchy(S,location=0,scale=1)  # Cauchy
library(MASS)
mu = c(1,1)                    # mean
Sigma = matrix(c(1, 0.5, 0.5, 2),ncol=2)     # covariance matrix
mvrnorm(S,mu,Sigma)            # multivariate normal
library(mvtnorm)
rmvt(S,Sigma,df)               # multivariate Student-t
library(Rlab)
rbern(S, prob=0.4)             # Bernoulli
library(evir)
rgev(S, xi=1, mu=0, sigma=1)   # generalized extreme value
rgpd(S, xi=2, mu=0, beta=1)    # generalized Pareto distribution

################### code 1-8 #######################
x <- seq(from = -5, to = 5, by = 0.01) 
y <- dnorm(x) 
plot(x=x,y=y,type="l",col="green",lwd=2,   
     xlab="quantile",ylab="density = dnorm(x)") 
grid(col="darkgrey",lwd=2) 
title(main="Probability Density Function (PDF)")

################### code 1-9 #######################
y <- pnorm(x)
par(mar = par()$mar + c(0,1,0,0))
plot(x=x,y=y,type="l",col="seagreen",lwd=2, xlab="quantile = qnorm(y)",
     ylab="probability = pnorm(x)") ; grid(col="darkgrey",lwd=2)
title(main="Cumulative Distribution Function (CDF)")

################### code 1-10 ######################
curve(dnorm, from=-5, to=5, col="gray", lwd=3
      , main="Density of the standard normarl distribution") 
text(-5,0.3,expression
     (f(x)==frac(1,sigma ~ sqrt(2*pi))~~e^{-~frac((x-mu)^2,2*sigma^2)} ),                   adj=0)

################### code 1-11 #######################
mykurtosis = function(x) {
  m4 = mean((x - mean(x))^4)
  kurt = m4/(sd(x)^4)-3
  kurt
}
mykurtosis(y)

mykurtosis1 = function(x,excess=3) {
  m4 = mean((x _ mean(x))^4)
  kurt = m4/(sd(x)^4)-excess
  kurt
}
mykurtosis1(y)

################### code 1-12 #######################
if(ncol(A)==nrow(A)){
  det(A)
} else {
  print("Matrix Not Square")
}

ifelse(ncol(A)==nrow(A), det(A), "Matrix Not Square")

################### code 1-13 #######################
x = rnorm(5)             # generate 5 random numbers
z = numeric(length(x))   # create vector the same length as x
for (i in 1:length(x)){
  z[i] = x[i] + 0.5
}
################### code 1-14 #######################
a = 10
if (a %% 3 == 0) {
  print("a is a multiple of 3")
} else {
  print("a is not a multiple of 3")
}

################### code 1-15 #######################
setwd("E:/BOOK/Data and code") # set in your PC
mydata = read.table("GDPiran.txt")
mydata = read.csv("ifbindex.csv")
mydata = matrix(scan(file = "data.dat"),byrow = TRUE,ncol = 3)

library("tseries")          # load the tseries library
price = get.hist.quote(instrument = "^gspc", start = "2000-01-01",
                       quote="AdjClose")     # download the prices, from January 1, 2000 until today
y = diff(log(price)) # convert the prices into returns
y = coredata(y)
plot(y)               # plot the returns

library(quantmod)            # Load the package
getSymbols("AAPL")           #Download daily prices of Apple stock from Yahoo
dim(AAPL)                    # (dimension): See the size of the downloaded data
head(AAPL)                   # See the first 6 rows of the data
tail(AAPL)                   # See the last 6 rows of the data
chartSeries(AAPL,theme="white") 
chartSeries(AAPL)
getSymbols("AAPL",from="2010-01-02", to="2016-06-06")
AAPL.rtn=diff(log(AAPL$AAPL.Adjusted))   # Compute log returns
chartSeries(AAPL.rtn,theme="white")
