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
library(glue)
library(data.table)
library(lubridate)

#Supress warnings
options("getSymbols.warning4.0" = FALSE)

prev_portfolio_val <- 50000000

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
parent_dir <- getwd()

# Run backtest for every year
for (year in c(2004:2018)){
  dt <- fread(glue("D:/Desktop/tick_data/year_{year}.csv"))
  df <- fread(glue("{parent_dir}/year_{year-1}_pairs.csv"))
  
  df$symbols = paste(df$symbol1,df$symbol2,sep=".")
  df_pairs = df[coint %in% 1,c(3,4,10)]
  
  #can adjust to get number of pairs that we want
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
  initDate=paste0(year, "-01-01")
  initEq= prev_portfolio_val
  tradeSize = initEq/50
  #portfolio allocation variables
  maxSize = initEq/50
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
  prev_portfolio_val <- getEndEq(account.st,paste0(year, "-01-01"))
}

