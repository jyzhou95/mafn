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

prev_portfolio_val <- 100000

setwd(dirname(rstudioapi::getSourceEditorContext()$path))
parent_dir <- getwd()

# Run backtest for every year
for (year in c(2003:2018)){
  
  if (year > 2003){
    dt_pairs_prev <- dt_pairs
  }
  
  dt_price <- fread(glue("D:/Desktop/tick_data/year_{year}.csv"))
  dt_price[,month := month(as.Date(dt))]
  
  # When ask size is 0, set ask price to 999999.99
  dt_price <- dt_price[ask_size > 0 & bid_size > 0]
  
  dt_pairs <- fread(glue("{parent_dir}/year_{year}_pairs.csv"))
  dt_pairs <- dt_pairs[coint == 1]
  
  dt_pairs$symbols = paste(dt_pairs$symbol1,dt_pairs$symbol2,sep=".")
  dt_pairs <- unique(dt_pairs, by = c("symbols"))
  
  for (month in c(1:12)){
    
    print(Sys.time())
    print(as.Date(paste0(year, "-", month, "-01")))
    
    if (year == 2003 & month == 1){
      next
    }
    
    prev_month <- month - 1
    int.month <- month
    dt_price_curr <- dt_price[month == int.month]
    
    if (month == 1){
      prev_month <- 12
      
      # Choose top 10 by smallest test_value and avoid choosing the same stock
      dt_pairs_delete_this <- dt_pairs_prev[month(as.Date(dt)) == prev_month][order(test_value)]
      dt_pairs_temp <- data.table()
      
      for (i in 1:nrow(dt_pairs_delete_this)){
        if (nrow(dt_pairs_temp) < 10){
          if (nrow(dt_pairs_temp) > 0){
            # If we are trading the symbol we skip it
            if (length(intersect(c(dt_pairs_delete_this[i]$symbol1,
                                   dt_pairs_delete_this[i]$symbol2),
                                 c(dt_pairs_temp$symbol1,
                                   dt_pairs_temp$symbol2)))){
              # Skip it
              next
              
            } else{
              dt_pairs_temp <- rbind(dt_pairs_temp, dt_pairs_delete_this[i])
            }
            
          } else{
            dt_pairs_temp <- rbind(dt_pairs_temp, dt_pairs_delete_this[i])
          }
        } else{
          break
        }
      }
      
      dt_pairs_curr = dt_pairs_temp[month(as.Date(dt)) == prev_month][,list(symbol1, symbol2, symbols)]
    } else{
      
      # Choose top 10 by smallest test_value and avoid choosing
      dt_pairs_delete_this <- dt_pairs[month(as.Date(dt)) == prev_month][order(test_value)]
      dt_pairs_temp <- data.table()
      
      for (i in 1:nrow(dt_pairs_delete_this)){
        if (nrow(dt_pairs_temp) < 10){
          if (nrow(dt_pairs_temp) > 0){
            # If we are trading the symbol we skip it
            if (length(intersect(c(dt_pairs_delete_this[i]$symbol1,
                            dt_pairs_delete_this[i]$symbol2),
                          c(dt_pairs_temp$symbol1,
                            dt_pairs_temp$symbol2)))){
              # Skip it
              next
              
            } else{
              dt_pairs_temp <- rbind(dt_pairs_temp, dt_pairs_delete_this[i])
            }
            
          } else{
            dt_pairs_temp <- rbind(dt_pairs_temp, dt_pairs_delete_this[i])
          }
        } else{
          break
        }
      }
      
      dt_pairs_curr = dt_pairs_temp[month(as.Date(dt)) == prev_month][,list(symbol1, symbol2, symbols)]
    }
    
    symbols = c()
    
    for (i in 1:nrow(dt_pairs_curr)){
      symb1 = dt_pairs_curr$symbol1[i]
      symb2 = dt_pairs_curr$symbol2[i]
      symb = dt_pairs_curr$symbols[i]
      temp1 = dt_price_curr[symbol %in% symb1,]
      temp2 = dt_price_curr[symbol %in% symb2,]
      df_temp = dt_pairs[symbols == symb]
      df_temp[,V1 := NULL]
      
      temp1[,symbol1 := df_temp$symbol1]
      temp1[,symbol2 := df_temp$symbol2]
      temp1[,coint := df_temp$coint]
      temp1[,p_value := df_temp$p_value]
      temp1[,test_value := df_temp$test_value]
      temp1[,residual_se := df_temp$residual_se]
      temp1[,beta := df_temp$beta]
      temp1[,symbols := df_temp$symbols]
      
      
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
    init_date = as.character(as.Date(paste0(year, "-", month, "-01")))
    .orderqty <- 100
    init_equity = prev_portfolio_val
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
    end_eq <- v$summary$End.Eq
    dt.summary <- data.table(end_eq)
    dt.summary[,dt := index(end_eq)]
    dt.summary <- dt.summary[,list(dt, portfolio_val = End.Eq)]
    lst.order_book <- get.orderbook(portfolio.st)[[1]]
    
    dt.order_book <- rbindlist(lapply(1:length(lst.order_book), function(x){
      if (!is.null(lst.order_book[[x]])){
        dt.temp <- data.table(lst.order_book[[x]])
        dt.temp[,security := names(lst.order_book[x])]
        return (dt.temp[,list(dt = Order.StatusTime,
                              status = Order.Status,
                              side = Order.Side,
                              order_type = Order.Type,
                              rule = Rule,
                              security,
                              qty = Order.Qty,
                              price = Order.Price)])
      } else{
        data.table()
      }
    }))
    
    # Save results
    write.csv(dt.summary, paste0(parent_dir, "/trading_results/", "pnl_", as.Date(paste0(year, "-", month, "-01")), ".csv"))
    write.csv(dt.order_book, paste0(parent_dir, "/trading_results/", "orderbook_", as.Date(paste0(year, "-", month, "-01")), ".csv"))
    write.csv(dt_pairs_curr[,list(dt = as.Date(paste0(year, "-", month, "-01")),
                                  symbol1,
                                  symbol2)],
              paste0(parent_dir, "/trading_results/", "pairs_", as.Date(paste0(year, "-", month, "-01")), ".csv"))
    
    prev_portfolio_val <- tail(dt.summary$portfolio_val, 1)
  }
  
}

