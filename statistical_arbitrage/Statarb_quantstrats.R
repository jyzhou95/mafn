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
  temp3$residual_se = 2*temp3$residual_se
  temp4 = temp3
  temp3$Bid = temp1$bid - temp2$ask*temp1$beta
  #temp3$Bi2d = temp1$bid - temp2$ask*temp1$beta
  temp4$Ask = temp1$ask - temp2$bid*temp1$beta
  #temp4$As2k = temp1$ask - temp2$bid*temp1$beta
  names(temp3) <- c("p_value","beta","residual_se","coint","Bid.Close")#,"Bid.Open")
  names(temp4) <- c("p_value","beta","residual_se","coint","Ask.Close")#,"Ask.Open")
  assign(paste(symb,"Bid",sep="."),temp3)
  assign(paste(symb,"Ask",sep="."),temp3)
  symbols = c(symbols,paste(symb,"Bid",sep="."),paste(symb,"Ask",sep="."))
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

stock.str = symbols

#Define the instrument type
stock(stock.str,currency='USD',multiplier=1)

#BOILERPLATE
initDate="2003-01-01"
tradeSize = 100000
#portfolio allocation variables
maxSize = 1000000
initEq= 50000000
strategy.st <- portfolio.st <- account.st <- "STATARB"


initPortf(portfolio.st,symbols=stock.str,initDate = initDate)
initAcct(account.st,portfolios=portfolio.st, initEq=initEq)
initOrders(portfolio=portfolio.st, initDate = initDate)
strategy(strategy.st, store=TRUE)
statarb <- strategy(portfolio.st)

statarb <- add.signal(strategy = statarb,name="sigCrossover",
                      arguments = list(columns=c("Ask.Close","residual_se"), relationship="lte"),
                      label="ask.lte.2sigma")
statarb <- add.signal(strategy = statarb,name="sigCrossover",
                      arguments = list(columns=c("Ask.Close","residual_se"), relationship="gt"),
                      label="ask.gt.2sigma")
statarb <- add.signal(strategy = statarb,name="sigCrossover",
                      arguments = list(columns=c("Bid.Close","residual_se"), relationship="gte"),
                      label="bid.gte.2sigma")
statarb <- add.signal(strategy = statarb,name="sigCrossover",
                      arguments = list(columns=c("Bid.Close","residual_se"), relationship="lt"),
                      label="bid.lt.2sigma")

statarb <- add.rule(strategy = statarb,name='ruleSignal',
                    arguments = list(sigcol="ask.lte.2sigma",sigval=TRUE, ordertype='market', orderside='long',
                    orderqty = tradeSize),type='enter')

statarb <- add.rule(strategy = statarb,name='ruleSignal',
                    arguments = list(sigcol="ask.gt.2sigma",sigval=TRUE, ordertype='market', orderside='long',
                                     orderqty='all'),type='exit')

statarb <- add.rule(strategy = statarb,name='ruleSignal',
                    arguments = list(sigcol="bid.gte.2sigma",sigval=TRUE, ordertype='market', orderside='short',
                    orderqty = tradeSize),type='enter')

statarb <- add.rule(strategy = statarb,name='ruleSignal',
                    arguments = list(sigcol="bid.lt.2sigma",sigval=TRUE, ordertype='market', orderside='short',
                                     orderqty='all'),type='exit')

start_t<-Sys.time()
out<-applyStrategy(strategy=statarb , portfolios=portfolio.st)




updatePortf(portfolio.st)
u = getPortfolio(portfolio.st)
dateRange <- time(getPortfolio(portfolio.st)$summary[-1])
updateAcct(portfolio.st,dateRange)
updateEndEq(account.st)
getEndEq(account.st,"2011-01-01")
chart.Posn(portfolio.st,"CCL.RCL.Bid")

