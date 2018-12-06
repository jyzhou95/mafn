library(blotter)
library(quantstrat)
library(quantmod)
library(PerformanceAnalytics)
library(zoo)
library(TTR)
library(devtools)
require(DSTrading)
require(IKTrading)
library(imputeTS)
library(forecast)
library(dlm)
library(tidyverse)
library(foreach)
library(doMC)

#Supress warnings
options("getSymbols.warning4.0" = FALSE)

df_2003 = fread("mafn/statistical_arbitrage/year_2003.csv")
df_2004 = fread("mafn/statistical_arbitrage/year_2004.csv")
df_2005 = fread("mafn/statistical_arbitrage/year_2005.csv")
#df_2006 = fread("mafn/statistical_arbitrage/year_2006.csv")
#df_2007 = fread("mafn/statistical_arbitrage/year_2007.csv")
#df_2008 = fread("mafn/statistical_arbitrage/year_2008.csv")
#df_2009 = fread("mafn/statistical_arbitrage/year_2009.csv")
#df_2010 = fread("mafn/statistical_arbitrage/year_2010.csv")
#df_2011 = fread("mafn/statistical_arbitrage/year_2011.csv")
#df_2012 = fread("mafn/statistical_arbitrage/year_2012.csv")
#df_2013 = fread("mafn/statistical_arbitrage/year_2013.csv")
#df_2014 = fread("mafn/statistical_arbitrage/year_2014.csv")
#df_2015 = fread("mafn/statistical_arbitrage/year_2015.csv")
#df_2016 = fread("mafn/statistical_arbitrage/year_2016.csv")
#df_2017 = fread("mafn/statistical_arbitrage/year_2017.csv")
#df_2018 = fread("mafn/statistical_arbitrage/year_2018.csv")
dt = rbind(df_2003,df_2004,df_2005)


df_coint_2003 = fread("mafn/statistical_arbitrage/year_2003_pairs.csv")
df_coint_2004 = fread("mafn/statistical_arbitrage/year_2004_pairs.csv")
df_coint_2005 = fread("mafn/statistical_arbitrage/year_2005_pairs.csv")
df_coint_2006 = fread("mafn/statistical_arbitrage/year_2006_pairs.csv")
df_coint_2007 = fread("mafn/statistical_arbitrage/year_2007_pairs.csv")
df_coint_2008 = fread("mafn/statistical_arbitrage/year_2008_pairs.csv")
df_coint_2009 = fread("mafn/statistical_arbitrage/year_2009_pairs.csv")
df_coint_2010 = fread("mafn/statistical_arbitrage/year_2010_pairs.csv")
df_coint_2011 = fread("mafn/statistical_arbitrage/year_2011_pairs.csv")
df_coint_2012 = fread("mafn/statistical_arbitrage/year_2012_pairs.csv")
df_coint_2013 = fread("mafn/statistical_arbitrage/year_2013_pairs.csv")
df_coint_2014 = fread("mafn/statistical_arbitrage/year_2014_pairs.csv")
df_coint_2015 = fread("mafn/statistical_arbitrage/year_2015_pairs.csv")
df_coint_2016 = fread("mafn/statistical_arbitrage/year_2016_pairs.csv")
df_coint_2017 = fread("mafn/statistical_arbitrage/year_2017_pairs.csv")
df_coint_2018 = fread("mafn/statistical_arbitrage/year_2018_pairs.csv")
df = rbind(df_coint_2003,df_coint_2004,df_coint_2005,df_coint_2006,df_coint_2007,df_coint_2008,df_coint_2009,df_coint_2010,df_coint_2011,df_coint_2012,df_coint_2013,df_coint_2014,df_coint_2015,df_coint_2016,df_coint_2017,df_coint_2018)
df$symbols = paste(df$symbol1,df$symbol2,sep=".")
df_pairs = df[coint %in% 1,c(3,4,10)]

#can adjust to get number of pairs that we want
df_pairs = df_pairs[duplicated(df_pairs),]
df_pairs = df_pairs[duplicated(df_pairs),]
df_pairs = df_pairs[duplicated(df_pairs),]
df_pairs = df_pairs[duplicated(df_pairs),]
df_pairs = df_pairs[duplicated(df_pairs),]
df_pairs = df_pairs[duplicated(df_pairs),]
df_pairs = df_pairs[duplicated(df_pairs),]
df_pairs = df_pairs[duplicated(df_pairs),]
df_pairs = df_pairs[duplicated(df_pairs),]
df_pairs = unique(df_pairs)
df = df[symbols %in% df_pairs$symbols,]
df$dt = paste(df$dt, "09:30:00")

df_pairs = df_pairs[1:3,]
symbols = c()
for (i in 1:nrow(df_pairs)){
  symb1 = df_pairs$symbol1[i]
  symb2 = df_pairs$symbol2[i]
  symb = df_pairs$symbols[i]
  temp1 = dt[symbol %in% symb1,]
  temp2 = dt[symbol %in% symb2,]
  df_temp = df[symbols %in% symb,]
  temp1 = merge(temp1,df_temp,by="dt",all=TRUE)
  temp1$p_value = na.locf0(temp1$p_value)
  temp1$beta = na.locf0(temp1$beta)
  temp1$residual_se = na.locf0(temp1$residual_se)
  temp1$coint = na.locf0(temp1$coint)
  temp1 = temp1[duplicated(temp1$dt) == FALSE,]
  temp2 = temp2[duplicated(temp2$dt) == FALSE,]
  temp1 = temp1[dt %in% temp2$dt,]
  temp2 = temp2[dt %in% temp1$dt,]
  temp1 = temp1[dt %in% temp2$dt,]
  temp3 = temp1[,c("dt","p_value","beta","residual_se","coint")]
  temp3$dt = as_datetime(temp3$dt)
  temp3 = as.xts(temp3,order.by=temp3$dt)
  temp3$negresidual = -temp3$residual_se
  temp3$twosigma = 2*temp3$residual_se
  temp3$negtwosigma = -2*temp3$residual_se
  temp3$Bid = temp1$bid - temp2$ask*temp1$beta 
  temp3$Ask = temp1$ask - temp2$bid*temp1$beta
  names(temp3) <- c("p_value","beta","residual_se","coint","negresidual","twosigma","","Open","Close")#,"Bid.Open")
  assign(symb,temp3)
  symbols = c(symbols,symb)
}

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
#stock.str= names(df)[-1]
#symbols = names(df)[-1]
stock.str = symbols
#Define the instrument type
stock(stock.str,currency='USD',multiplier=1)

#BOILERPLATE
initDate="2003-01-01"
tradeSize = 100000
initEq= tradeSize * length(symbols)
strategy.st <- portfolio.st <- account.st <- "STATARB"

initPortf(portfolio.st,symbols=stock.str,initDate = initDate)
initAcct(account.st,portfolios=portfolio.st, initEq=initEq)
initOrders(portfolio=portfolio.st, initDate = initDate)
strategy(strategy.st, store=TRUE)

strategy.st<- strategy(portfolio.st)





strategy.st <- add.indicator(strategy = strategy.st, name = "SMA", arguments = list(x=quote(mktdata$residual_se), n=50),label= "ma50" )
strategy.st <- add.indicator(strategy = strategy.st, name = "SMA", arguments = list(x=quote(Cl(mktdata)), n=200),label= "ma200")
strategy.st <- add.indicator(strategy = strategy.st, name = "SMA", arguments = list(x=quote(Cl(mktdata)), n=30),label= "ma30" )
strategy.st <- add.indicator(strategy = strategy.st, name = "SMA", arguments = list(x=quote(Cl(mktdata)[,1]), n=300),label= "ma300")
#strategy.st <- add.indicator(strategy = strategy.st, name = "emptyfunc", arguments = list(x=quote(Cl(mktdata)[,1]), n=300),label= "ma300")

strategy.st <- add.signal(strategy = strategy.st,name="sigCrossover",arguments = list(columns=c("Close","negtwosigma"), relationship="gte"),label="ask.lte.2sigma")
strategy.st <- add.signal(strategy = strategy.st,name="sigCrossover",arguments = list(column=c("Open","negresidual_se"),relationship="lt"),label="ma50.lt.ma200")
strategy.st <- add.signal(strategy = strategy.st,name="sigCrossover",arguments = list(columns=c("Open","twosigma"), relationship="gte"),label="ma30.gt.ma300")
strategy.st <- add.signal(strategy = strategy.st,name="sigCrossover",arguments = list(column=c("Close","residual_se"),relationship="lt"),label="ma30.lt.ma300")
#sigCrossover, sigComparison, sigThreshold, sigAND (IK), 
#relationship gt = greater than, gte = gt equal to, lt, lte, eq 

strategy.st <- add.rule(strategy = strategy.st,name='ruleSignal', arguments = list(sigcol="ask.lte.2sigma",sigval=TRUE, orderqty = tradeSize, ordertype='market', orderside='long',prefer="Close"),type='enter')
strategy.st <- add.rule(strategy = strategy.st,name='ruleSignal', arguments = list(sigcol="ma50.lt.ma200",sigval=TRUE,  orderqty = tradeSize, ordertype='market', orderside='long',prefer="Open"),type='exit')
strategy.st <- add.rule(strategy = strategy.st,name='ruleSignal', arguments = list(sigcol="ma30.lt.ma300",sigval=TRUE,  orderqty = tradeSize, ordertype='market', orderside='short',prefer="Open"),type='enter')
strategy.st <- add.rule(strategy = strategy.st,name='ruleSignal', arguments = list(sigcol="ma30.gt.ma300",sigval=TRUE,  orderqty = tradeSize, ordertype='market', orderside='short',prefer="Close"),type='exit')

for(i in symbols){
  addPosLimit(portfolio = portfolio.st,
              symbol = i,
              timestamp = initDate,
              maxpos = tradeSize)
}

#getSymbols(stock.str,from=initDate)
start_t<-Sys.time()
out<-applyStrategy(strategy=strategy.st , portfolios=portfolio.st)
end_t <- Sys.time()

updatePortf(portfolio.st)
u = getPortfolio(portfolio.st)
dateRange <- time(getPortfolio(portfolio.st)$summary[-1])
updateAcct(portfolio.st,dateRange)
updateEndEq(account.st)
v = getAccount(account.st)
#getEndEq(account.st,"2018-01-01")