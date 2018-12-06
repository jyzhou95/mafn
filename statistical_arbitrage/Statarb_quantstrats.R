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
  dt_raw <- fread(glue("D:/Desktop/tick_data/year_{year}.csv"))
  dt_raw[,month := month(as.Date(dt))]
  
  # When ask size is 0, set ask price to 999999.99
  dt_raw <- dt_raw[ask_size > 0 & bid_size > 0]
  
  df <- fread(glue("{parent_dir}/year_{year}_pairs.csv"))
  df <- df[coint == 1]
  
  df$symbols = paste(df$symbol1,df$symbol2,sep=".")
  
  dt <- dt_raw[month == 2]
  df_pairs_raw = df[month(as.Date(dt)) == 2,list(symbol1, symbol2, symbols)]
  
  #can adjust to get number of pairs that we want
  df = df[symbols %in% df_pairs$symbols,]
  df$dt = paste(df$dt, "09:30:00")
  
  df_pairs = df_pairs_raw
  symbols = c()
  
  for (i in 1:nrow(df_pairs)){
    symb1 = df_pairs$symbol1[i]
    symb2 = df_pairs$symbol2[i]
    symb = df_pairs$symbols[i]
    temp1 = dt[symbol %in% symb1,]
    temp2 = dt[symbol %in% symb2,]
    df_temp = df[symbols == symb]
    df_temp[,V1 := NULL]
    
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
    names(temp3) <- c("p_value","beta","residual_se","coint","negresidual","twosigma","negtwosigma","Open","Close")#,"Bid.Open")
    assign(symb,temp3)
    symbols = c(symbols,symb)
  }
  
  #House Cleaning
  rm(list = ls(.blotter),envir = .blotter)
  
  #REMOVAL
  if (exists("portfolio.st")){
    rm.strat(portfolio.st)
  }
  if (exists("strategy.st")){
    rm.strat(strategy.st)
  }
  
  
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
  init_date = "2003-01-01"
  .orderqty <- 10000
  init_equity = .orderqty*length(symbols)
  .threshold <- 0.0005
  .txnfees <- 0
  .stoploss <- 0.4 # 0.003 or 0.3%
  
  portfolio.st <- "Port.Luxor.Stop.Loss"
  account.st <- "Acct.Luxor.Stop.Loss"
  strategy.st <- "Strat.Luxor.Stop.Loss"
  
  initPortf(name = portfolio.st,
            symbols = symbols,
            initDate = init_date)
  initAcct(name = account.st,
           portfolios = portfolio.st,
           initDate = init_date,
           initEq = init_equity)
  
  initOrders(portfolio = portfolio.st,
             symbols = symbols,
             initDate = init_date)
  
  strategy(strategy.st, store = TRUE)
  
  
  add.signal(strategy.st, 
             name = "sigCrossover",
             arguments = list(columns = c("Close", "negtwosigma"),
                              relationship = "lte"),
             label = "long"
  )
  
  add.signal(strategy.st, 
             name = "sigCrossover",
             arguments = list(columns = c("Open", "negresidual_se"),
                              relationship = "gt"),
             label = "longexit"
  )
  
  add.signal(strategy.st, 
             name = "sigCrossover",
             arguments = list(columns = c("Open", "twosigma"),
                              relationship = "gte"),
             label = "short")
  
  add.signal(strategy.st, 
             name = "sigCrossover",
             arguments = list(columns = c("Close", "residual_se"),
                              relationship = "lt"),
             label = "shortexit")
  
  add.rule(strategy.st, 
           name = "ruleSignal",
           arguments = list(sigcol = "long" , 
                            sigval = TRUE,
                            replace = FALSE,
                            orderside = "long" ,
                            ordertype = "stoplimit",
                            prefer = "Close",
                            threshold = .threshold,
                            TxnFees = .txnfees,
                            orderqty = +.orderqty,
                            osFUN = osMaxPos,
                            orderset = "ocolong"),
           type = "enter",
           label = "EnterLONG")
  
  add.rule(strategy.st, 
           name = "ruleSignal",
           arguments = list(sigcol = "short", 
                            sigval = TRUE,
                            replace = FALSE,
                            orderside = "short",
                            ordertype = "stoplimit",
                            prefer = "Open",
                            threshold = .threshold,
                            TxnFees = .txnfees,
                            orderqty = -.orderqty,
                            osFUN = osMaxPos,
                            orderset = "ocoshort"),
           type = "enter",
           label = "EnterSHORT")
  
  add.rule(strategy.st, 
           name = "ruleSignal",
           arguments = list(sigcol = "longexit", 
                            sigval = TRUE,
                            replace = TRUE,
                            orderside = "long" ,
                            ordertype = "market",
                            TxnFees = .txnfees,
                            orderqty = "all",
                            orderset = "ocolong",
                            prefer = "Open"),
           type = "exit",
           label = "Exit2LONG")
  
  add.rule(strategy.st, 
           name = "ruleSignal",
           arguments = list(sigcol = "shortexit", 
                            sigval = TRUE,
                            replace = TRUE,
                            orderside = "short",
                            ordertype = "market",
                            TxnFees = .txnfees,
                            orderqty = "all",
                            orderset = "ocoshort",
                            prefer = "Close"),
           type = "exit",
           label = "Exit2SHORT")
  
  add.rule(strategy.st, 
           name = "ruleSignal",
           arguments = list(sigcol = "long" , 
                            sigval = TRUE,
                            replace = FALSE,
                            orderside = "long",
                            ordertype = "stoplimit",
                            tmult = TRUE,
                            threshold = quote(.stoploss),
                            TxnFees = .txnfees,
                            orderqty = "all",
                            orderset = "ocolong",
                            prefer = "Close"),
           type = "chain", 
           parent = "EnterLONG",
           label = "StopLossLONG",
           enabled = FALSE)
  
  add.rule(strategy.st, 
           name = "ruleSignal",
           arguments = list(sigcol = "short", 
                            sigval = TRUE,
                            replace = FALSE,
                            orderside = "short",
                            ordertype = "stoplimit",
                            tmult = TRUE,
                            threshold = quote(.stoploss),
                            TxnFees = .txnfees,
                            orderqty = "all",
                            orderset = "ocoshort",
                            prefer = "Open"),
           type = "chain", 
           parent = "EnterSHORT",
           label = "StopLossSHORT",
           enabled = FALSE)
  
  for(symbol in symbols){
    addPosLimit(portfolio = portfolio.st,
                symbol = symbol,
                timestamp = init_date,
                maxpos = .orderqty)
  }
  
  enable.rule(strategy.st, 
              type = "chain", 
              label = "StopLoss")
  
  out <-  applyStrategy(strategy.st, portfolios = portfolio.st)
  
  
  updatePortf(portfolio.st)
  u = getPortfolio(portfolio.st)
  dateRange <- time(getPortfolio(portfolio.st)$summary[-1])
  updateAcct(account.st,dateRange)
  updateEndEq(account.st)
  v = getAccount(account.st)
  dt.pnl <- data.table(v$summary)
  dt.pnl[,dt := index(v$summary)]
}

