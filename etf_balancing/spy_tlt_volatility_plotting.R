library(quantmod)
library(lattice)
library(timeSeries)
library(tseries)
library(rugarch)
library(data.table)
library(ggplot2)
library(glue)
library(parallel)
library(plotly)


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

funcCalcVolatility <- function(chr.symbol, start_date, end_date){
  dt.stock  <- funcGetStockPrice(c(chr.symbol), as.Date("1960-01-01"), as.Date(end_date))
  dt.stock <- dt.stock[dt >= start_date]
  
  dt.stock$returns <- dt.stock$returns - 1
  
  # Calculate 90 days annualized returns volatility
  dt.stock$vol_90 <- c(rep(NA, 89), rollapply(data = dt.stock$returns,width=90,FUN=sd) * sqrt(252))
  
  dt.stock <- dt.stock[!is.na(vol_90)]
  
  # Predicted volatility is yesterday's volatility
  dt.stock$pred_vol_90 <- c(NA, head(dt.stock$vol_90, -1))
  dt.stock <- dt.stock[!is.na(pred_vol_90)]
  return (dt.stock)
}

funcPlotVolatility <- function(dt.stock){
  chr.symbol <- unique(dt.stock$symbol)
  plt <- ggplot() + geom_line(data = dt.stock, aes(x = dt, y = vol_90, color = "Actual Volatility"), group = 1) + 
    geom_line(data = dt.stock, aes(x = dt, y = pred_vol_90, color = "One Day Lag Volatility"), group = 1) +
    theme_bw(base_size = 20) + ggtitle(glue("{chr.symbol} 90 Volatility")) + 
    scale_colour_manual("", 
                        breaks = c("Actual Volatility", "One Day Lag Volatility"),
                        values = c("black", "red"))
  
  # Calculate MAE
  flt.mae <- sum(abs(dt.stock$vol_90 - dt.stock$pred_vol_90))/nrow(dt.stock)
  print(glue("Mean Absolute Error: {flt.mae * 100}%"))
  
  ggplotly(plt)
}

dt.spy <- funcCalcVolatility(chr.symbol = "SPY",start_date = "2005-01-01", end_date = Sys.Date())
dt.tlt <- funcCalcVolatility(chr.symbol = "TLT",start_date = "2005-01-01", end_date = Sys.Date())

