require(quantstrat)
library(data.table)
library(glue)

funcGetStockPrice <- function(vec.symbols, start_date = Sys.Date() - 20, end_date = Sys.Date(), bln.all = FALSE, 
                              verbose = FALSE){
  dt.return.this <- rbindlist(lapply(1:length(vec.symbols), function(x){
    tmp_stock <- vec.symbols[x]
    if (verbose){
      print(tmp_stock)
      print(glue("Progress: {x/length(vec.symbols) * 100}%"))
    }
    dt.temp <- tryCatch({
      if (bln.all){
        getSymbols(Symbols = tmp_stock, auto.assign = FALSE)
      } else{
        getSymbols(Symbols = tmp_stock, from = start_date, to = end_date, auto.assign = FALSE)
      }
    },
    error = function(cond){
      return(data.table())
    }, 
    warning = function(cond){
      return (data.table())
    })
    
    if (nrow(dt.temp)){
      lst.dates <- index(dt.temp)
      dt.temp <- data.table(dt.temp)
      dt.temp$date <- lst.dates
      dt.temp$symbol <- tmp_stock
      colnames(dt.temp) <- c("open", "high", "low", "close", "volume", "adjusted_close", "dt", "symbol")
      
      # Skip stock if volume is 0 but not if it's an index
      if (any(dt.temp$volume == 0) & !grepl("%5E", tmp_stock)){
        return (data.table())
      }
      
      dt.temp$returns <- returns(dt.temp$adjusted_close) + 1
      dt.temp <- dt.temp[!is.na(returns)]
      return (dt.temp)
    } else{
      return (data.table())
    }
  }))
  return (dt.return.this)
}

suppressWarnings(rm("order_book.pair1",pos=.strategy))
suppressWarnings(rm("account.pairs", "portfolio.pair1", pos=.blotter))
suppressWarnings(rm("startDate", "endDate", "startDate", "initEq", "SD", "N", 
                    "symb1", "symb2", "portfolio1.st", "account.st", 
                    "etfVolStrat", "out1"))

startDate <- '2018-01-01'    
endDate <- '2018-11-02'
initEq <- 100000

MaxPos <- 1500  #max position in stockA; 
# max position in stock B will be max * ratio, i.e. no hard position limit in 
# Stock B
lvls <- 3  #how many times to fade; Each order's qty will = MaxPos/lvls


symb1 <- 'SPY' #change these to try other pairs
symb2 <- 'TLT' #if you change them, make sure position limits still make sense

portfolio1.st <- 'pair1'
account.st <- 'pairs'

getSymbols(c(symb1, symb2), from=startDate, to=endDate, adjust=TRUE) 

# The following function is used to make sure the timestamps of all symbols are 
# the same deletes rows where one of the stocks is missing data
alignSymbols <- function(symbols, env=.GlobalEnv) {
  # This is a simplified version of qmao::alignSymbols()
  if (length(symbols) < 2) 
    stop("Must provide at least 2 symbols")
  if (any(!is.character(symbols))) 
    stop("Symbols must be vector of character strings.")
  ff <- get(symbols[1],env=env)
  for (sym in symbols[-1]) {
    tmp.sym <- get(sym,env=env)
    ff <- merge(ff, tmp.sym, all=FALSE)
  }
  for (sym in symbols) {
    assign(sym,ff[,grep(sym, colnames(ff))], env=env)
  }
  symbols
}
alignSymbols(c(symb1, symb2))

# Define Instruments
currency("USD")
stock(symb1, currency="USD", multiplier=1)
stock(symb2, currency="USD", multiplier=1)

# Initialize Portfolio, Account, and Orders
initPortf(name=portfolio1.st, c(symb1,symb2))
initAcct(account.st, portfolios=portfolio1.st, initEq=initEq)
initOrders(portfolio=portfolio1.st)

# osFUN will need to know which symbol is leg 1 and which is leg 2 as well as 
# what the values are for MaxPos and lvls.  So, create a slot in portfolio to 
# hold this info.
pair <- c(1, 2, MaxPos, lvls)
names(pair) <- c(symb1, symb2, "MaxPos", "lvls")
.blotter[[paste('portfolio', portfolio1.st, sep='.')]]$pair <- pair

# Create initial position limits and levels by symbol
# allow 3 entries for long and short if lvls=3.
addPosLimit(portfolio=portfolio1.st, timestamp=startDate, symbol=symb1, 
            maxpos=MaxPos, longlevels=lvls, minpos=-MaxPos, shortlevels=lvls)
addPosLimit(portfolio=portfolio1.st, timestamp=startDate, symbol=symb2, 
            maxpos=MaxPos, longlevels=lvls, minpos=-MaxPos, shortlevels=lvls)

# Create a strategy object 
etfVolStrat <- strategy('etfVolStrat')

# Calculate SPY portfolio proportion using historic volatility
funcCalcPortfolioProportion <- function(startDate, endDate){
  dt.stock  <- funcGetStockPrice(c("SPY", "TLT"), as.Date("1960-01-01"), 
                                 as.Date(endDate))
  
  dt.stock$returns <- dt.stock$returns - 1
  
  dt.spy <- dt.stock[symbol == "SPY"]
  dt.tlt <- dt.stock[symbol == "TLT"]
  
  
  # Calculate 90 days annualized returns volatility
  dt.spy$spy_vol_90 <- c(rep(NA, 89), rollapply(data = dt.spy$returns,width=90,FUN=sd) * sqrt(252))
  dt.tlt$tlt_vol_90 <- c(rep(NA, 89), rollapply(data = dt.tlt$returns,width=90,FUN=sd) * sqrt(252))
  
  dt.return.this <- merge(dt.spy[,list(dt, spy_vol_90)],
                          dt.tlt[,list(dt, tlt_vol_90)],
                          by = c("dt"))
  dt.return.this <- dt.return.this[dt >= startDate]
  dt.return.this[,symbol := "SPY"]
  dt.return.this[,proportion := (1/spy_vol_90) / (1/spy_vol_90 + 1/tlt_vol_90)]
  dt.return.this <- dt.return.this[,list(dt, symbol, proportion)]
  dt.return.this.final <- rbind(dt.return.this, dt.return.this[,list(dt,
                                                                     symbol = "TLT",
                                                                     proportion = 1-proportion)])
  return (dt.return.this.final)
}
# A XTS object containing time series of asset proportion
dt.proportion <- funcCalcPortfolioProportion(startDate, endDate)

.blotter[[paste('portfolio',portfolio1.st,sep='.')]]$proportion <- dt.proportion

#and make a function to get the most recent getProportion
getProportion <- function(portfolio, timestamp) {
  portf <- getPortfolio(portfolio)
  timestamp <- format(timestamp,"%Y-%m-%d %H:%M:%S")
  # above line ensures you don't get last value of next day if using intraday 
  # data and timestamp=midnight
  toDate <- paste("::", timestamp, sep="")
  proportion <- last(portf$proportion[toDate])
  as.numeric(proportion)
}


etfVolStrat <- add.rule(strategy = etfVolStrat,
                        name='rulePctEquity',
                        arguments = list(rebalance_on = 'days',
                                         trade.percent=0.5,
                                         longlevels=1,
                                         shortlevels = 0))

# Run the backtest
out1<-applyStrategy(strategy=etfVolStrat, portfolios=portfolio1.st)

updatePortf(Portfolio=portfolio1.st,
            Dates=paste("::", as.Date(Sys.time()), sep=''))
updateAcct(account.st, Dates=paste(startDate, endDate, sep="::")) 
updateEndEq(account.st, Dates=paste(startDate, endDate, sep="::"))
getEndEq(account.st, Sys.time())



ret1 <- PortfReturns(account.st)
ret1$total <- rowSums(ret1)


if("package:PerformanceAnalytics" %in% search() || 
   require("PerformanceAnalytics",quietly=TRUE)) {
  getSymbols("SPY", from='1999-01-01')
  SPY.ret <- Return.calculate(SPY$SPY.Close)
  tmp <- merge(SPY.ret,ret1$total,all=FALSE)
  dev.new()
  charts.PerformanceSummary(ret1$total, geometric=FALSE, wealth.index=TRUE)
}