##################################
#####PART 1 - INITIALIZATION######
##################################

library(blotter)
library(quantstrat)
library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(TTR)
library(devtools)
require(DSTrading)
require(IKTrading)

#Supress warnings
options("getSymbols.warning4.0" = FALSE)
#House Cleaning
rm(list = ls(.blotter),envir = .blotter)

#REMOVAL
rm.strat(portfolio.st)
rm.strat(strategy.st)


#Set currency
currency('USD')
#Set time
Sys.setenv(TZ="UTC")

#Set stock universe ####EDIT HERE
stock.str= names(df)[-1]
symbols = names(df)[-1]

#Define the instrument type
currency(stock.str,currency='USD',multiplier=1)

#BOILERPLATE
initDate="1990-01-01"
tradeSize = 100000
initEq= tradeSize * length(symbols)
strategy.st <- portfolio.st <- account.st <- "CARRY"

initPortf(portfolio.st,symbols=stock.str,initDate = initDate)
initAcct(account.st,portfolios=portfolio.st, initEq=initEq)
initOrders(portfolio=portfolio.st, initDate = initDate)
strategy(strategy.st, store=TRUE)

stratMACROSS<- strategy(portfolio.st)

stratMACROSS <- add.indicator(strategy = stratMACROSS, name = "SMA", arguments = list(x=quote(Cl(mktdata)), n=50),label= "ma50" )
stratMACROSS <- add.indicator(strategy = stratMACROSS, name = "SMA", arguments = list(x=quote(Cl(mktdata)[,1]), n=200),label= "ma200")
stratMACROSS <- add.indicator(strategy = stratMACROSS, name = "SMA", arguments = list(x=quote(Cl(mktdata)), n=30),label= "ma30" )
stratMACROSS <- add.indicator(strategy = stratMACROSS, name = "SMA", arguments = list(x=quote(Cl(mktdata)[,1]), n=300),label= "ma300")

stratMACROSS <- add.signal(strategy = stratMACROSS,name="sigCrossover",arguments = list(columns=c("ma50","ma200"), relationship="gte"),label="ma50.gt.ma200")
stratMACROSS <- add.signal(strategy = stratMACROSS,name="sigCrossover",arguments = list(column=c("ma50","ma200"),relationship="lt"),label="ma50.lt.ma200")
stratMACROSS <- add.signal(strategy = stratMACROSS,name="sigCrossover",arguments = list(columns=c("ma30","ma300"), relationship="gte"),label="ma30.gt.ma300")
stratMACROSS <- add.signal(strategy = stratMACROSS,name="sigCrossover",arguments = list(column=c("ma30","ma300"),relationship="lt"),label="ma30.lt.ma300")
#sigCrossover, sigComparison, sigThreshold, sigAND (IK), 
#relationship gt = greater than, gte = gt equal to, lt, lte, eq 

stratMACROSS <- add.rule(strategy = stratMACROSS,name='ruleSignal', arguments = list(sigcol="ma50.gt.ma200",sigval=TRUE, orderqty=100, ordertype='market', orderside='long'),type='enter')
stratMACROSS <- add.rule(strategy = stratMACROSS,name='ruleSignal', arguments = list(sigcol="ma50.lt.ma200",sigval=TRUE, orderqty='all', ordertype='market', orderside='long'),type='exit')
stratMACROSS <- add.rule(strategy = stratMACROSS,name='ruleSignal', arguments = list(sigcol="ma30.lt.ma300",sigval=TRUE, orderqty=100, ordertype='market', orderside='short'),type='enter')
stratMACROSS <- add.rule(strategy = stratMACROSS,name='ruleSignal', arguments = list(sigcol="ma30.gt.ma300",sigval=TRUE, orderqty='all', ordertype='market', orderside='short'),type='exit')

#getSymbols(stock.str,from=initDate)
start_t<-Sys.time()
out<-applyStrategy(strategy=stratMACROSS , portfolios=portfolio.st)


#setup analytics
updatePortf(portfolio.st)
dateRange <- time(getPortfolio(portfolio.st)$summary[-1])
updateAcct(portfolio.st,dateRange)
updateEndEq(account.st)


#trade statistics
tStats <- tradeStats(Portfolios = portfolio.st, use = "trades", inclZeroDays = FALSE)
tStats[,4:ncol(tStats)] <- round(tStats[,4:ncol(tStats)],2)
print(data.frame(t(tStats[,c(1,2)])))
(aggPF <- sum(tStats$Gross.Profits)/-sum(tStats$Gross.Losses))
(aggCorrect <- mean(tStats$Perent.Positive))
(numTrades <- sum(tStats$Num.Trades))
(meanAvgWLR <- mean(tStats$Avg.WinLoss.Ratio[tStats$Avg.WinLoss.Ratio < Inf], na.rm=TRUE))


#daily statistics
dStats <- dailystats(Portfolios = portfolio.st, use="Currency")
rownames(dStats) <- gsub(".DailyEndEq","",rownames(dStats))
print(data.frame(t(dStats)))
durStats <- durationStatistics(Portfolio = portfolio.st, Symbols = sort(symbols),aggregate=FALSE)
indivDurStats <- durationStatistics(Portfolio)
print(t(durStats))
print(t(indivDurstats))

#Tools - Indicators and Plots
getSymbols(Symbols = "AAPL", from = "1998-01-01", to = "2012-12-31")
sma <- SMA(x = Cl(SPY), n = 200)
rsi <- RSI(price = Cl(SPY), n = 2)
chart_Series(SPY)
add_TA(sma,on=1,lwd=1.5,col="blue")
add_TA(rsi,lwd = 1.5, col="green")
zoom_Chart("2008-04-01::2008-12-31")


#try RSI
nRSI = 2
thresh1 = 10
thresh2 = 6

nSMAexit = 5
nSMAfilter = 200

period = 10
pctATR = 0.2
maxPct = 0.4


#functions
"lagATR" <- function(HLC, n = 14, maType, lag=1, ...){
  ATR <- ATR(HLC, n = n, maType = maType, ...)
  ATR <- lag(ATR, lag)
  out <- ATR$atr
  colnames(out) <- "atr"
  return(out)
}

"rollingAR" <- function(data, n = 14, lag = 3){
  for i in 1:(dim(data)[1] - n){
    arnow = ar(data[i:i+n,], lags = lag)
    predict(arnow,n.ahead = 1)
  }
}