setwd(dirname(rstudioapi::getSourceEditorContext()$path))
parent_dir <- getwd()
source(glue("{parent_dir}/funcAnalyses.R"))

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

funcCalcVolatility <- function(chr.symbol, start_date, end_date, vol_lookback = 90){
  dt.stock  <- funcGetStockPrice(c(chr.symbol), as.Date("1960-01-01"), as.Date(end_date))
  dt.stock <- dt.stock[dt >= start_date]
  
  dt.stock$returns <- dt.stock$returns - 1
  
  # Calculate volatility using the previous n days annualized returns volatility
  dt.stock$vol_90 <- c(rep(NA, vol_lookback - 1), rollapply(data = dt.stock$returns,width=vol_lookback,FUN=sd) * sqrt(252))
  
  dt.stock <- dt.stock[!is.na(vol_90)]
  
  # Actual volatility is include today's
  dt.stock$pred_vol_90 <- c(tail(dt.stock$vol_90, -1), NA)
  dt.stock <- dt.stock[!is.na(pred_vol_90)]
  return (dt.stock)
}


dt.spy <- funcCalcVolatility(chr.symbol = "SPY",start_date = "2005-01-01", end_date = Sys.Date(), vol_lookback = 90)
dt.tmf <- funcCalcVolatility(chr.symbol = "TMF",start_date = "2005-01-01", end_date = Sys.Date(), vol_lookback = 90)

# Calculate portfolio allocation from volatility
dt.return.this <- merge(dt.spy[,list(dt, spy_vol_90=vol_90, spy_pred_vol_90=pred_vol_90)],
                        dt.tmf[,list(dt, tmf_vol_90=vol_90, tmf_pred_vol_90=pred_vol_90)],
                        by = c("dt"))
dt.return.this[,spy_proportion_vol_90 := (1/spy_vol_90) / (1/spy_vol_90 + 1/tmf_vol_90)]
dt.return.this[,tmf_proportion_vol_90 := (1/tmf_vol_90) / (1/spy_vol_90 + 1/tmf_vol_90)]
dt.return.this[,spy_proportion_vol_90_pred := (1/spy_pred_vol_90) / (1/spy_pred_vol_90 + 1/tmf_pred_vol_90)]
dt.return.this[,tmf_proportion_vol_90_pred := (1/tmf_pred_vol_90) / (1/spy_pred_vol_90 + 1/tmf_pred_vol_90)]


# Previous 90 days volatility
dt.weighting_prev <- rbind(dt.return.this[,list(dt, symbol = "SPY", weighting = spy_proportion_vol_90)],
                           dt.return.this[,list(dt, symbol = "TMF", weighting = tmf_proportion_vol_90)])

dt.returns_prev <- funcRunSimpleBacktestPortfolioWeighting(dt.weighting = dt.weighting_prev)

# Plot returns
require("PerformanceAnalytics",quietly=TRUE)
charts.PerformanceSummary(dt.returns_prev[,list(dt, ret = daily_ret - 1)])

SharpeRatio.annualized(dt.returns_prev[,list(dt, ret = daily_ret - 1)])




