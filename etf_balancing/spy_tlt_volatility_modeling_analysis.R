library(quantmod)
library(lattice)
library(timeSeries)
library(tseries)
library(rugarch)
library(data.table)
library(ggplot2)
library(glue)
library(parallel)


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

funcArimaGarchVol <- function(chr.symbol, start_date, end_date, int.window, verbose = TRUE){
  
  dt.stock  <- funcGetStockPrice(c(chr.symbol), as.Date(start_date) - 4 * int.window, as.Date(end_date))
  
  # Calculate 90 days annualized returns volatility
  dt.stock$returns <- dt.stock$returns - 1
  
  dt.stock$vol_90 <- c(rep(NA, int.window-1), rollapply(data = dt.stock$returns,width=int.window,FUN=sd) * sqrt(252))
  
  dt.stock <- rbind(tail(dt.stock[dt < start_date], int.window),
                    dt.stock[dt >= start_date])
  
  # Number of days to predict
  num_pred_days <- nrow(dt.stock) - int.window - 1
  # Generate predictions
  dt.predictions <- rbindlist(lapply(1:num_pred_days, function(x){
    
    if (verbose){
      print(glue("Progress: {x / num_pred_days * 100}%"))
    }
    
    # Initialize forecast
    dt.forecast <- dt.stock[int.window+x+1]
    
    # Get our returns
    returnsOffSet <- dt.stock[x:int.window+x]$vol_90
    
    # Fit the ARIMA model
    final.aic <- Inf
    final.order <- c(0,0,0)
    
    for (p in 0:5) for (q in 0:5) {
      if ( p == 0 && q == 0) {
        next
      }
      
      arimaFit = tryCatch( arima(returnsOffSet, order=c(p, 0, q)),
                           error=function( err ) FALSE,
                           warning=function( err ) FALSE )
      
      if( !is.logical( arimaFit ) ) {
        current.aic <- AIC(arimaFit)
        if (current.aic < final.aic) {
          final.aic <- current.aic
          final.order <- c(p, 0, q)
          final.arima <- arima(returnsOffSet, order=final.order)
        }
      } else {
        next
      }
    }
    
    # Fit the GARCH model
    spec = ugarchspec(
      variance.model=list(garchOrder=c(1,1)),
      mean.model=list(armaOrder=c(final.order[1], final.order[3]), include.mean=T),
      distribution.model="sged"
    )
    fit = tryCatch(
      ugarchfit(
        spec, returnsOffSet, solver = 'hybrid'
      ), error=function(e) e, warning=function(w) w
    )
    
    # If the GARCH model does not converge, set the direction to "long" else
    # choose the correct forecast direction based on the returns prediction
    # Output the results to the screen and the forecasts vector
    if(is(fit, "warning")) {
      dt.forecast$pred_mov <- 0
      dt.forecast$forecast <- 1
    } else {
      fore = ugarchforecast(fit, n.ahead=1)
      ind = fore@forecast$seriesFor
      dt.forecast$pred_mov <- ind
      dt.forecast$forecast <- ifelse(ind > 0, 1, -1)
    }
    
    return (dt.forecast)
    
  }))
  return (dt.predictions)
}

dt.spy_vol_forecast <- funcArimaGarchVol(chr.symbol = "SPY", 
                                         start_date = "2004-01-01", 
                                         end_date = "2018-10-25", 
                                         int.window = 252)


dt.tlt_vol_forecast <- funcArimaGarchVol(chr.symbol = "TLT", 
                                         start_date = "2004-01-01", 
                                         end_date = "2018-10-25", 
                                         int.window = 252)



