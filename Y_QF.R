# Quantitative Finance Assignment
#========================== Question 1 ==================================
library(quantmod)
data <- getSymbols("AAPL", auto.assign = F)
# calculate daily returns and monthly returns
data_rtn_d <- dailyReturn(data$AAPL.Adjusted)
data_rtn_m <- monthlyReturn(data$AAPL.Adjusted)
# standardization
data_rtn_d_norm <- (data_rtn_d-mean(data_rtn_d))/sd(data_rtn_d)
data_rtn_m_norm <- (data_rtn_m-mean(data_rtn_m))/sd(data_rtn_m)
# plot probability density function for standardized daily returns and standard normal distribution
plot(dnorm,-4,4,ylim=c(0,0.65),xlab="",ylab="PDF",col=2,cex=9,lty=2,lwd=3,main="AAPL Standardized PDF for Daily Returns")
lines(density(data_rtn_d_norm),xlim=c(-4,4),col=4,lwd=2)
legend("topleft",c("Daily Returns","Normal Distribution"),lty=c(1,2),col=c(4,2))
grid()
# plot pdf for standardized monthly returns and standard normal distribution
plot(dnorm,-4,4,ylim=c(0,0.5),xlab="",ylab="PDF",col=2,cex=9,lty=2,lwd=3,main="AAPL Standardized PDF for Monthly Returns")
lines(density(data_rtn_m_norm),xlim=c(-4,4),col=4,lwd=2)
legend("topleft",c("Monthly Returns","Normal Distribution"),lty=c(1,2),col=c(4,2))
grid()

#========================== Question 2 ==================================
# ============================ Binomial Euro =========================
# Multiplicative Binomial Tree for European
Binomial_E <- function(type, K, T, S, r, N, u, d)
{
  dt <- T / N
  p <- (exp(r*dt)-d)/(u-d)
  disc <- exp(-r*dt)
  # get stock prices
  s <- c()
  s[1] <- S*u^N
  for (i in 2:(N+1))
  {
    s[i] <- s[i-1]*d/u
  }
  # initialise option values at maturity
  if(type=="Call")
  {
    M <- as.matrix(pmax(s-K,0))
  } else{
    M <- as.matrix(pmax(K-s,0))
  }
  # use matrix to calculate back the call option value
  for (i in 1:N)
  {
    # delete the last line in matrix M
    M1 <- M[-nrow(M),]
    # delete the first line in matrix M
    M2 <- M[-1,]
    M <- as.matrix(disc*(p*M1+(1-p)*M2))
    # print(M)
  }
  M[1,1]
}
# Binomial_E <- function(type, K, T, S, r, N, u, d)
Binomial_E("Call", 100, 1, 100, 0.06, 3, 1.1, 1/1.1)

# General Additive Binomial Valuation of European   
Binomial_Euro <- function(type, K, T, S, r, N, sig, div)
{
  dt <- T / N
  nu <- r-div-0.5*sig^2
  dxu <- sqrt(sig^2*dt+(nu*dt)^2)
  dxd <- -dxu
  pu <- 0.5+0.5*(nu*dt/dxu)
  pd <- 1-pu
  disc <- exp(-r*dt)
  # get stock prices
  s <- c()
  s[1] <- S*exp(N*dxu)
  for (i in 2:(N+1))
  {
    s[i] <- s[i-1]*exp(dxd-dxu)
  }
  # initialise option values at maturity
  if(type=="Call")
  {
    M <- as.matrix(pmax(s-K,0))
  } else{
    M <- as.matrix(pmax(K-s,0))
  }
  # use matrix to calculate back the call option value
  for (i in 1:N)
  {
    # delete the last line in matrix M
    M1 <- M[-nrow(M),]
    # delete the first line in matrix M
    M2 <- M[-1,]
    M <- as.matrix(disc*(pu*M1+pd*M2))
    # print(M)
  }
  M[1,1]
}
# Binomial_Euro <- function(type, K, T, S, r, N, sig, div)
Binomial_Euro("Call", 100, 1, 100, 0.06, 3, 0.2, 0)

# ========================== Binomial American ==========================
# Multiplicative Binomial Tree for Amercian
Binomial_A <- function(type, K, T, S, r, N, u, d)
{
  dt <- T / N
  p <- (exp(r*dt)-d)/(u-d)
  disc <- exp(-r*dt)
  # get stock prices
  s <- c()
  s[1] <- S*u^N
  for (i in 2:(N+1))
  {
    s[i] <- s[i-1]*d/u
  }
  # initialise option values at maturity
  if(type=="Call")
  {
    M <- as.matrix(pmax(s-K,0))
  } else{
    M <- as.matrix(pmax(K-s,0))
  }
  for (i in 1:N)
  {
    # delete the last line in matrix M
    M1 <- M[-nrow(M),]
    # delete the first line in matrix M
    M2 <- M[-1,]
    M <- disc*(p*M1+(1-p)*M2)
    s <- as.matrix(s)[-1,]/d
    # judge it worth to execute before expiration or not
    if(type=="Call")
    {
      M <- as.matrix(pmax(M, s-K))
    } else{
      M <- as.matrix(pmax(M, K-s))
    }
    # print(M)
  }
  M[1,1]
}
# Binomial_A <- function(type, K, T, S, r, N, u, d)
Binomial_A("Put", 100, 1, 100, 0.06, 3, 1.1, 1/1.1)

# General Additive Binomial Valuation of Amercian
Binomial_Ameri <- function(type, K, T, S, r, N, sigma, div)
{
  dt <- T/N
  nu <- r-div-0.5*sigma^2
  dxu <- sqrt(sigma^2*dt+(nu*dt)^2)
  dxd <- -dxu
  pu <- 0.5+0.5*(nu*dt/dxu)
  pd <- 1-pu
  disc <- exp(-r*dt)
  dpu <- disc*pu
  dpd <- disc*pd
  edxdu <- exp(dxd-dxu)
  edxd <- exp(dxd)
  s <- c()
  s[1] <- S*exp(N*dxu)
  for (i in 2:(N+1))
  {
    s[i] <- s[i-1]*edxdu
  }
  if(type=="Call")
  {
    M <- as.matrix(pmax(s-K,0))
  } else{
    M <- as.matrix(pmax(K-s,0))
  }
  for (i in 1:N)
  {
    M1 <- M[-nrow(M),]
    M2 <- M[-1,]
    M <- dpu*M1+dpd*M2
    s <- as.matrix(s)[-1,]/edxd
    # judge it worth to execute before expiration or not
    if(type=="Call")
    {
      M <- as.matrix(pmax(M, s-K))
    } else{
      M <- as.matrix(pmax(M, K-s))
    }
    #print(M)
  }
  M[1,1]
}
# Binomial_Ameri <- function(type, K, T, S, r, N, sigma, div)
Binomial_Ameri("Put", 100, 1, 100, 0.06, 3, 0.2, 0)


# Additive Binomial Valuation of Amercian with a Discrete Proportional Dividend
Binomial_Ameri_Proportion <- function(type, K, T, S, r, N, sigma, div, dvh, tau)
{
  dt <- T/N
  nu <- r-div-0.5*sigma^2
  dxu <- sqrt(sigma^2*dt+(nu*dt)^2)
  dxd <- -dxu
  pu <- 0.5+0.5*(nu*dt/dxu)
  pd <- 1-pu
  disc <- exp(-r*dt)
  dpu <- disc*pu
  dpd <- disc*pd
  edxdu <- exp(dxd-dxu)
  edxd <- exp(dxd)
  # calculate # of the dividen
  n <- floor((T-tau)/(T/N))+1
  s <- c()
  s[1] <- S*exp(N*dxu)*(1-dvh)
  for (i in 2:(N+1))
  {
    s[i] <- s[i-1]*edxdu
  }
  # print(s)
  if(type=="Call")
  {
    M <- as.matrix(pmax(s-K,0))
  } else{
    M <- as.matrix(pmax(K-s,0))
  }
  for (i in 1:N)
  {
    if(i>=N-n+1)
    {
      s <- s/(1-dvh)
    }
    M1 <- M[-nrow(M),]
    M2 <- M[-1,]
    M <- dpu*M1+dpd*M2
    s <- as.matrix(s)[-1,]/edxd
    # judge it worth to execute before expiration or not
    if(type=="Call")
    {
      M <- as.matrix(pmax(M, s-K))
    } else{
      M <- as.matrix(pmax(M, K-s))
    }
    #print(M)
  }
  M[1,1]
}
# Binomial_Ameri_Proportion <- function(type, K, T, S, r, N, sigma, dvh, tau)
Binomial_Ameri_Proportion("Put", 100, 1, 100, 0.06, 3, 0.2, 0, 0.03, 2/3)

#========================== Question 3 ==================================
# get the implied vol of options
implvol
# the correlation matrix
R <- cor(impvol)
# the eigenvalues of the correlation matrix, choose a sufficient number of factors
eigenR <- eigen(R)
plot(eigenR$values,type='o',xlab='Component',ylab="Eigenvalues")

# Assume the sufficient number of factors is 2
#use the PCA method to find a final rotated factor solution
pc <- principal(implvol, nfactors=2, rotate ="varimax")
loadings(pc)
#build a factor model to predict
factor <- implvol %*% pc$loadings
f1 <- factor[,1]
f2 <- factor[,2]
lm <- lm(implvol~f1+f2)
factor_model <- lm$coefficients[1]+lm$coefficients[2]*f1+lm$coefficients[3]*f2

#========================== Question 4 ==================================
# # Python Implementation
# from sklearn import svm
# # get the daily job search activity for the last year
# jobsearch_train = []
# # get the unemployment rate for the last year
# rate_train = []
# # implement SVM
# clf = svm.SVR(kernel='rbf',C=  ,gamma=  )
# # get the daily job search activity for the recent 10 days 
# jobsearch_test = []
# # prediction
# rate_test = clf.fit(jobsearch_train, rate_train).predict(jobsearch_test)

#========================== Question 5 ==================================
# choose SP500 as benchmark
startdate <- "2014-01-01"
enddate <- "2017-01-01"
sp500 <- getSymbols("^GSPC",from=startdate,to=enddate, auto.assign = F)
combine <- sp500[,6]
# find 30 stocks with the largest weights in SP500
name <- c("AAPL","XOM","JNJ","PG","IBM","JPM","T","GE","CVX",
              "PFE","WFC","CSCO","KO","BAC","HPQ","WMT","INTC","MRK","PEP",
              "ORCL","VZ","PM","GS","ABT","SLB","QCOM","COP","C","MCD","OXY")
# combine adjusted prices of 30 stocks together
for(i in 1:length(name))
{
  price <- getSymbols(name[i],from=startdate,to=enddate, auto.assign = F)[,6]
  combine <- merge(combine,price)
}
# two years for training, one year for test
train <- combine[1:(nrow(combine)-252),]
test <- tail(combine,252)
# calculate log returns
return <- diff(log(train))[-1,]
vol <- apply(return,2,sd)
# choose volatility less than 0.01 to construct portfolio
colnum <- which(vol<0.01)
portfolio <- train[,colnum]
# weight for each stock
w <- 1/vol[colnum][-1]/sum(1/vol[colnum][-1])
w
# performance
capital <- 100000
share <- capital*c(1,w)/portfolio[1,]
v_b <- as.numeric(share[1,1])*portfolio[,1]
v_p <- portfolio[,-1] %*% t(share[,-1])
# plot
plot(ts(v_b),ylab="Value",main="S&P500 Compared with Smart Beta")
lines(ts(v_p),col=4)
legend("topleft",c("S&P500","Smart Beta"),lty=c(1,1),col=c(1,4))
# out of sample back testing
portfolio_t <- test[,colnum]
capital <- 100000
share <- capital*c(1,w)/portfolio_t[1,]
v_b <- as.numeric(share[1,1])*portfolio_t[,1]
v_p <- portfolio_t[,-1] %*% t(share[,-1])
# plot
plot(ts(v_b),ylab="Value",ylim=c(90000,120000),main="S&P500 Compared with Smart Beta Backtesting")
lines(ts(v_p),col=4)
legend("topleft",c("S&P500","Smart Beta"),lty=c(1,1),col=c(1,4))

#========================== Question 6 ==================================
EURUSD <- getSymbols('EUR/USD', from="2015-03-01", src='oanda', auto.assign = FALSE)
USDCHF <- getSymbols('USD/CHF', from="2015-03-01", src='oanda', auto.assign = FALSE)
plot(ts(EURUSD),ylim=c(0.9,1.2),ylab="Price",main="EUR/USD & USD/CHF")
lines(ts(USDCHF),col=4)
legend("topright",c("EUR/USD","USD/CHF"),lty=c(1,1),col=c(1,4),cex=0.7)

# define training set
startT  <- "2015-03-01"
endT    <- "2016-03-01"
rangeT  <- paste(startT,"::",endT,sep ="")
EURUSD_T   <- EURUSD[rangeT]
USDCHF_T   <- USDCHF[rangeT]
#define out of sample set
startO  <- "2016-03-01"
endO    <- "2017-03-01"
rangeO  <- paste(startO,"::",endO,sep ="")
EURUSD_O   <- EURUSD[rangeO]
USDCHF_O   <- USDCHF[rangeO]

# compute price difference
pd_eu <- diff(EURUSD_T)[-1]
pd_uc <- diff(USDCHF_T)[-1]
model <- lm(pd_eu ~ pd_uc)
# get the hedge ratio
hedgeratio <- as.numeric(model$coefficients[2])
# spread price
spread <- EURUSD_T-hedgeratio*USDCHF_T
mean_sp <- mean(spread)
sd_sp <- sd(spread)
# set lower and upper bound
upper <- mean_sp+sd_sp
lower <- mean_sp-sd_sp
# when the spread exceed the upper bound, sell EUR/USD and buy USD/CHF
# when the spread is smaller than the lower bound, sell USD/CHF and buy EUR/USD
sell <- which(spread>=upper)
buy <- which(spread<=lower)
