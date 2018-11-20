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

funcGarchVol <- function(chr.symbol, start_date, end_date, verbose = TRUE){
  
  dt.stock  <- funcGetStockPrice(c(chr.symbol), as.Date("1960-01-01"), as.Date(end_date))
  
  dt.stock$returns <- dt.stock$returns - 1
  
  # Calculate 90 days annualized returns volatility
  dt.stock$vol_90 <- c(rep(NA, 89), rollapply(data = dt.stock$returns,width=90,FUN=sd) * sqrt(252))
  
  dt.stock <- dt.stock[!is.na(vol_90)]
  
  # Number of days to predict
  num_pred_days <- nrow(dt.stock[dt >= start_date])
  
  spec = ugarchspec(
    mean.model=list(armaOrder=c(0,0)),
    variance.model=list(garchOrder=c(1,1)),
    distribution="sged")
  
  garch_pred <- ugarchroll(spec,
                           n.ahead = 1,
                           forecast.length = num_pred_days,
                           data = dt.stock[dt < start_date]$returns,
                           refit.every=1, 
                           refit.window="moving",
                           solver = "hybrid")
  
  dt.return.this <- dt.stock[dt >= start_date]
  dt.return.this$pred_vol <- garch_pred@forecast$density$Sigma * sqrt(252)
  return (dt.return.this)
}

funcWriteVolatilityForecast <- function(dt.vol_forecast){
  dt.write.this <- dt.vol_forecast[,list(dt, symbol, pred_vol)]
  chr.symbol <- unique(dt.write.this$symbol)
  write.csv(x = dt.write.this, file = glue("/data_acquisition_scripts/{chr.symbol}_vol_forecast.csv"))
}



dt.spy_vol_forecast <- funcGarchVol(chr.symbol = "SPY", 
                                    start_date = "2008-01-10", 
                                    end_date = "2008-01-31")


dt.tlt_vol_forecast <- funcGarchVol(chr.symbol = "TLT", 
                                    start_date = "2008-01-01", 
                                    end_date = "2008-01-31")

dt.spy_plot <- ggplot() + geom_line(data = dt.spy_vol_forecast, 
                                    aes(x = dt, y = vol_90, group = 1), 
                                    color = "black", size = 1.5) +
  geom_line(data = dt.spy_vol_forecast, 
            aes(x = dt, y = pred_vol, group = 1), 
            color = "red", size = 1.5) + theme_bw(base_size = 15) +
  xlab("Date") + ylab("Volatility")

dt.tlt_plot <- ggplot() + geom_line(data = dt.tlt_vol_forecast, 
                                    aes(x = dt, y = vol_90, group = 1), 
                                    color = "black", size = 1.5) +
  geom_line(data = dt.tlt_vol_forecast, 
            aes(x = dt, y = pred_vol, group = 1), 
            color = "red", size = 1.5) + theme_bw(base_size = 15) +
  xlab("Date") + ylab("Volatility")

ggplotly(dt.spy_plot)
ggplotly(dt.tlt_plot)


