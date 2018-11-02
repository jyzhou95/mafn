library(data.table)
library(quantmod)
library(tseries)
library(timeSeries)
library(glue)
library(rvest)
library(RSelenium)
library(jsonlite)
library(snow)
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

funcGetAllDailyStockPrice <- function(){
  dt.stock <- funcGetStockPrice(vec.symbols = c("SPY", "TLT"), start_date = "2005-01-01",verbose = T)
  
  # Merge on exchange and exclude NYSE (WRDS charges extra for NYSE tick data)
  write.csv(dt.stock, "D:/Desktop/mafn/data_acquisition_scripts/stock_eod_data.csv")
}