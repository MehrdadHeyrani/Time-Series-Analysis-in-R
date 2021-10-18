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

# create a Loop for finding  ARMA Order based on AIC and BIC

P=6 # P-1 maximum estimated order for AR part of the model

Q=6 # Q-1 maximum estimated order for MA part of the model

aic.x <- matrix(ncol=P, nrow=Q, byrow = T)
bic.x <- matrix(ncol=P, nrow=Q, byrow = T)

x=gdp

for (i in 1:P)
  for (j in 1:Q)
  {
    arimafit <- arima(x, order=c(i-1,0,j-1), method = c("ML"), optim.control = list(maxit=1000))
    aic.x[i,j] <- AIC(arimafit)
    bic.x[i,j] <- BIC(arimafit)
  }
aic.min<- min(aic.x)
bic.min<- min(bic.x)
for (h in 1:P)
  for (f in 1:Q)
  {
    if (aic.x[h,f]==aic.min)
    {
      p.aic <- h-1
      q.aic <- f-1
    }
  }
for (h in 1:P)
  for (f in 1:Q)
  {
    if (bic.x[h,f]==bic.min)
    {
      p.bic <- h-1
      q.bic <- f-1
    }
  }

aic.x #  AIC matrix for ARMA(p: 0 to 5 , q: 0 to 5)
bic.x #  BIC matrix for ARMA(p: 0 to 5 , q: 0 to 5)

p.aic # best AR(p) order based on minimum AIC
q.aic # best MA(q) order based on minimum AIC
p.bic # best AR(p) order based on minimum BIC
q.bic # best MA(q) order based on minimum BIC

# Fit ARMA model based on AIC order 
fit <- arima(x, order=c(p.aic,0, q.aic), method = c("ML"))

coeftest(fit)
fit
plot(fit)             
