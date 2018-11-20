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
# https://stackoverflow.com/questions/23926334/how-do-i-parallelize-in-r-on-windows-example

funcGetTradingDays <- function(chr.symbol, start_date, end_date){
  dt.stock <- getSymbols(chr.symbol, from = as.Date(start_date), to = as.Date(end_date), auto.assign = F)
  vec.dates <- index(dt.stock)
  return (vec.dates)
}

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

funcGetCurrStockPrice <- function(vec.symbols){
  # Create cluster
  cl<-makeCluster(detectCores(),type="SOCK")
  # Load relevant libraries into workers
  clusterEvalQ(cl, library(data.table))
  clusterEvalQ(cl, library(quantmod))
  dt.return.this <- rbindlist(clusterApply(cl, vec.symbols, function(x){
    return(data.table(symbol = x,
                      stock_price = getQuote(x)$Last))
  }))
  stopCluster(cl)
  return (dt.return.this)
}

funcGetCurrStockPriceRH <- function(vec.symbols){
  # Create cluster
  cl<-makeCluster(detectCores(),type="SOCK")
  # Load relevant libraries into workers
  clusterEvalQ(cl, library(data.table))
  clusterEvalQ(cl, library(jsonlite))
  clusterEvalQ(cl, library(glue))
  dt.return.this <- rbindlist(clusterApply(cl, vec.symbols, function(x){
    chr.base_url <- glue("https://api.robinhood.com/quotes/?symbols={x}")
    dt.results <- data.table(fromJSON(txt=chr.base_url, simplifyDataFrame=T)$results)
    return(data.table(symbol = x,
                      stock_price = dt.results$last_trade_price))
  }))
  stopCluster(cl)
  return (dt.return.this)
  
}

funcGetAllStockEarnings <- function(chr.start_date, chr.end_date, verbose = FALSE){
  vec.dates <- seq(from = as.Date(chr.start_date), to = as.Date(chr.end_date), by = "1 day")
  # Create cluster
  cl<-makeCluster(detectCores(),type="SOCK")
  # Load relevant libraries into workers
  clusterEvalQ(cl, library(data.table))
  clusterEvalQ(cl, library(quantmod))
  clusterEvalQ(cl, library(tseries))
  clusterEvalQ(cl, library(timeSeries))
  clusterEvalQ(cl, library(glue))
  clusterEvalQ(cl, library(rvest))
  
  dt.final <- rbindlist(clusterApply(cl, vec.dates, function(x, chr.start_date, chr.end_date){
    base_url <- glue("https://finance.yahoo.com/calendar/earnings?from={chr.start_date}&to={chr.end_date}&day={x}")
    chr.raw <- read_html(base_url)
    
    # Get number of earnings
    indx.results <- as.numeric(unlist(gregexpr(pattern ='results', as.character(chr.raw))))[1]
    int.num_earnings <- as.numeric(unlist(strsplit(substr(as.character(chr.raw), start = indx.results - 13, 
                                                          stop = indx.results - 2), " of "))[2])
    
    if (int.num_earnings < 100 | is.na(int.num_earnings)){
      # Get all url on the page
      link <- chr.raw%>% html_nodes("a") %>% html_attr( "href")
      # Get quotes
      link_1 <- link[grepl("quote", link)]
      # Remove indices
      link_2 <- link_1[!grepl("\\^", link_1)]
      # No earnings on that date
      if (length(link_2) == 0){
        return (data.table())
      } else{
        # Get symbols
        vec.symbols <- substr(link_2, as.numeric(regexpr('=', link_2)) + 1, nchar(link_2))
        dt.return.this <- data.table(
          "dt" = x,
          "symbol" = vec.symbols)
        return (dt.return.this)
      }
    } else{
      # Parse through all pages
      # Get all url on the page
      link <- chr.raw%>% html_nodes("a") %>% html_attr( "href")
      # Get quotes
      link_1 <- link[grepl("quote", link)]
      # Remove indices
      link_2 <- link_1[!grepl("\\^", link_1)]
      # No earnings on that date
      if (length(link_2) == 0){
        return (data.table())
      } else{
        # Get symbols
        vec.symbols <- substr(link_2, as.numeric(regexpr('=', link_2)) + 1, nchar(link_2))
        dt.return.this.final <- data.table(
          "dt" = x,
          "symbol" = vec.symbols)
        int.pages <- floor(int.num_earnings / 100)
        
        # Iterate through the other pages
        dt.bind.this <- rbindlist(lapply(1:int.pages, function(y){
          page_url <- paste0(base_url, glue("&offset={100*y}&size=100"))
          chr.raw <- read_html(page_url)
          # Get all url on the page
          link <- chr.raw%>% html_nodes("a") %>% html_attr( "href")
          # Get quotes
          link_1 <- link[grepl("quote", link)]
          # Remove indices
          link_2 <- link_1[!grepl("\\^", link_1)]
          # No earnings on that date
          if (length(link_2) == 0){
            return (data.table())
          } else{
            # Get symbols
            vec.symbols <- substr(link_2, as.numeric(regexpr('=', link_2)) + 1, nchar(link_2))
            dt.return.this <- data.table(
              "dt" = x,
              "symbol" = vec.symbols)
            return (dt.return.this)
          }
        }))
        
        dt.return.this.final <- rbind(dt.return.this.final, dt.bind.this)
        return (dt.return.this.final)
      }
      
      
    }
  }, chr.start_date = chr.start_date, chr.end_date = chr.end_date))
  stopCluster(cl)
  
  return (dt.final)

  # Unparallelized version of code
  
  # dt.final <- rbindlist(lapply(vec.dates, function(x){
  #   if (verbose == TRUE){
  #     print(x)
  #   }
  #   base_url <- glue("https://finance.yahoo.com/calendar/earnings?from={chr.start_date}&to={chr.end_date}&day={x}")
  #   chr.raw <- read_html(base_url)
  #   # Get all url on the page
  #   link <- chr.raw%>% html_nodes("a") %>% html_attr( "href")
  #   # Get quotes
  #   link_1 <- link[grepl("quote", link)]
  #   # Remove indices
  #   link_2 <- link_1[!grepl("\\^", link_1)]
  #   # No earnings on that date
  #   if (length(link_2) == 0){
  #     return (data.table())
  #   } else{
  #     # Get symbols
  #     vec.symbols <- substr(link_2, as.numeric(regexpr('=', link_2)) + 1, nchar(link_2))
  #     dt.return.this <- data.table(
  #       "dt" = x,
  #       "symbol" = vec.symbols)
  #     return (dt.return.this)
  #   }
  # }))
}

funcFormatFile <- function(input_file_path, bln.get_all_col = FALSE){
  dt.read <- fread(input_file_path)
  colnames(dt.read) <- c("spread", "side", "qty", "symbol", "exp", "strike", "type", "mark", "probability_of_profit",
                         "max_profit", "pnl_margin", "front_vol", "back_vol", "vol_diff", "delta")
  
  # Initialize id
  dt.read$id <- 0
  
  dt.read <- dt.read[,list(id, spread, side, qty, symbol, exp, strike, type, mark, probability_of_profit, max_profit)]
  
  # format expiration date
  dt.read[,exp := as.Date(exp, format = "%d %b %y")]
  
  # format probabilities
  dt.read[,probability_of_profit := as.numeric(gsub("%", "", probability_of_profit))/100]
  dt.read[,max_profit := as.numeric(gsub("%", "", max_profit))/100]
  
  # Parse through file to assign spread name, id, mark, probability of profit, max profit
  for (i in 1:nrow(dt.read)){
    dt.temp <- copy(dt.read[i])
    
    # Skip first line
    if (i > 1){
      dt.temp_prev <- copy(dt.read[i-1])
    } else{
      dt.read[i]$id <- 1
      next
    }
    
    # no spread name assigned
    if (dt.temp$spread == ""){
      # Assign values
      # Increment id
      dt.read[i]$id <- dt.temp_prev$id
      dt.read[i]$spread <-dt.temp_prev$spread
      dt.read[i]$mark <-dt.temp_prev$mark
      dt.read[i]$probability_of_profit <-dt.temp_prev$probability_of_profit
      dt.read[i]$max_profit <- dt.temp_prev$max_profit
    } else{
      # Increment id
      dt.read[i]$id <- dt.temp_prev$id + 1
    }
  }
  
  # Handle symbols with slashes in their name
  if (nrow(dt.read[grepl("/", symbol)])){
    dt.read[grepl("/", symbol)]$symbol <- gsub("/", "-", dt.read[grepl("/", symbol)]$symbol) 
  }
  return (dt.read)
}

funcGetOptionChain <- function(vec.stocks, chr.expiration){
  lapply(vec.stocks, function(x){
    lst.option_chains <- getOptionChain(Symbols = x, Exp = chr.expiration)
  })
}

funcGetVolatility <- function(vec.symbols, verbose = F){
  # Returns past 90 day volatility
  dt.return.this <- rbindlist(lapply(1:length(vec.symbols), function(x){
    tmp_stock <- vec.symbols[x]
    if (verbose){
      print(tmp_stock)
      print(glue("Progress: {x/length(vec.symbols) * 100}%"))
    }
    dt.temp <- tryCatch({
      getSymbols(Symbols = tmp_stock, auto.assign = FALSE)
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
      
      # Skip stock if volume is 0
      if (any(dt.temp$volume == 0)){
        return (data.table())
      }
      
      dt.temp$returns <- returns(dt.temp$adjusted_close) + 1
      dt.temp <- dt.temp[!is.na(returns)]
      dt.return.temp <- data.table(stock  = tmp_stock,
                                   volatility = sd(dt.temp$returns))
      return (dt.return.temp)
    } else{
      return (data.table())
    }
  }))
}

funcCalcProfitProbabilityVerticalSpread <- function(dt.trades, exp_date){
  # Calculate probabiltiy that a vertical spread ends up in the money
  # 1. Calculate daily volatility of the stock's return using the past 90 days returns
  # 2. Calculate Z score
  # 3. Calculate percentile of zscore
  
  vec.unique_symbols <- unique(dt.trades$symbol)
  dt.volatility <- funcGetVolatility(vec.symbols = vec.unique_symbols,
                                     verbose = T)
  # Get number of days until expiration
  num.days_until_expiration <- as.numeric(exp_date - Sys.Date()) + 1
  dt.volatility$volatility <- (dt.volatility$volatility + 1)^num.days_until_expiration
  
  dt.return.this <- rbindlist(lapply(sort(unique(dt.trades$id)), function(x){
    dt.temp <- dt.trades[id == x]
    if (dt.temp$type == "CALL"){
      dt.temp[,z_score := (min(dt.temp$strike) - getQuote(unique(dt.temp$symbol))$Last) / 
                (dt.volatility[stock == unique(dt.temp$symbol)]$volatility)]
      dt.temp[,probability_of_profit := pnorm(z_score)]
    } else{
      dt.temp[,z_score := (getQuote(unique(dt.temp$symbol))$Last - max(dt.temp$strike)) / 
                (dt.volatility[stock == unique(dt.temp$symbol)]$volatility)]
      dt.temp[,probability_of_profit := pnorm(z_score)]
    }
  }))
}

funcScanOptionsMarket <- function(vec.symbols, verbose = FALSE){
  # Create cluster
  cl<-makeCluster(detectCores(),type="SOCK")
  # Load relevant libraries into workers
  clusterEvalQ(cl, library(data.table))
  clusterEvalQ(cl, library(quantmod))
  clusterEvalQ(cl, library(tseries))
  clusterEvalQ(cl, library(timeSeries))
  clusterEvalQ(cl, library(glue))
  clusterEvalQ(cl, library(rvest))
  
  dt.return.this.final <- rbindlist(clusterApply(cl, vec.symbols, function(x){
    chr.url <- glue("https://www.nasdaq.com/symbol/{x}/option-chain")
    
    dt.temp <- chr.url %>% read_html() %>% html_nodes("table") %>% .[3] %>%
      html_table()
    
    dt.temp <- data.frame(dt.temp)
    colnames(dt.temp) <- c("calls", "last_call", "change_call", "bid_call", "ask_call", "vol_call", "open_int_call", "symbol", "strike",
                           "puts", "last_put", "change_put", "bid_put", "ask_put", "vol_put", "open_int_put")
    dt.temp <- data.table(dt.temp)
    dt.calls <- dt.temp[,list(symbol, 
                              type = "CALL",
                              expiration = as.Date(calls, "%b %d, %Y"),
                              strike,
                              last = last_call,
                              change = change_call,
                              bid = bid_call,
                              ask = ask_call,
                              vol = vol_call,
                              open_int = open_int_call)]
    dt.puts <- dt.temp[,list(symbol, 
                             type = "PUT",
                             expiration = as.Date(puts, "%b %d, %Y"),
                             strike,
                             last = last_put,
                             change = change_put,
                             bid = bid_put,
                             ask = ask_put,
                             vol = vol_put,
                             open_int = open_int_put)]
    dt.return.this <- rbind(dt.calls, dt.puts)
    
    return(dt.return.this)
  }))
  
  stopCluster(cl)
  dt.return.this.final <- dt.return.this.final[expiration > Sys.Date()]
  # Handle ATT and Ford
  dt.return.this.final[symbol == "FALSE"]$symbol <- "F"
  dt.return.this.final[symbol == "TRUE"]$symbol <- "T"
  return (dt.return.this.final)
  
  # Unparallelized version of code
  # Scan NASDAQ's listed options for a given vector of symbols
  # dt.return.this.final <- rbindlist(lapply(vec.symbols, function(x){
  #   if (verbose){
  #     print(glue("Scraping option for: {x}"))
  #   }
  #   chr.url <- glue("https://www.nasdaq.com/symbol/{x}/option-chain")
  # 
  #   dt.temp <- chr.url %>% read_html() %>% html_nodes("table") %>% .[3] %>%
  #     html_table()
  # 
  #   dt.temp <- data.frame(dt.temp)
  #   colnames(dt.temp) <- c("calls", "last_call", "change_call", "bid_call", "ask_call", "vol_call", "open_int_call", "symbol", "strike",
  #                                       "puts", "last_put", "change_put", "bid_put", "ask_put", "vol_put", "open_int_put")
  #   dt.temp <- data.table(dt.temp)
  #   dt.calls <- dt.temp[,list(symbol,
  #                             type = "CALL",
  #                             expiration = as.Date(calls, "%b %d, %Y"),
  #                             strike,
  #                             last = last_call,
  #                             change = change_call,
  #                             bid = bid_call,
  #                             ask = ask_call,
  #                             vol = vol_call,
  #                             open_int = open_int_call)]
  #   dt.puts <- dt.temp[,list(symbol,
  #                            type = "PUT",
  #                            expiration = as.Date(puts, "%b %d, %Y"),
  #                            strike,
  #                            last = last_put,
  #                            change = change_put,
  #                            bid = bid_put,
  #                            ask = ask_put,
  #                            vol = vol_put,
  #                            open_int = open_int_put)]
  #   dt.return.this <- rbind(dt.calls, dt.puts)
  # 
  #   return(dt.return.this)
  # }))
  # dt.return.this.final <- dt.return.this.final[expiration > Sys.Date()]
  # # Handle ATT and Ford
  # dt.return.this.final[symbol == "FALSE"]$symbol <- "F"
  # dt.return.this.final[symbol == "TRUE"]$symbol <- "T"
  
}

funcScanOptionsMarketBarchart <- function(vec.symbols, exp_date, verbose = FALSE, sleep = 0.25){
  # Useful stack overflow
  # Identifying api/json code: https://stackoverflow.com/questions/48759011/open-webpage-select-all-copy-into-sheet
  # Parsing json with R: 
  
  # Unparallelized version
  # Replace dash with period
  vec.symbols <- gsub("-", ".", vec.symbols)
  dt.return.this <- rbindlist(lapply(vec.symbols, function(x){
    if (verbose){
      print(x)
    }
    chr.url <- glue("https://core-api.barchart.com/v1/options/chain?symbol={x}&fields=strikePrice%2ClastPrice%2CbidPrice%2CaskPrice%2Cvolume%2CoptionType%2CdaysToExpiration%2cvolatility&expirationDate={exp_date}")
    lst.raw <- fromJSON(txt=chr.url, simplifyDataFrame=T)
    dt.temp <- data.table(lst.raw$data)
    colnames(dt.temp) <- c("strike", "last", "bid", "ask", "vol", "type", "days_to_expiration", "implied_volatility")
    dt.temp[,type := toupper(type)]
    dt.temp[,symbol := x]
    dt.temp[,expiration := exp_date]
    # Necessary to prevent too many request error (HTTP 429)
    Sys.sleep(sleep)
    return(dt.temp[,list(symbol, 
                         type, 
                         expiration, 
                         strike, 
                         last, 
                         bid, 
                         ask, 
                         vol,
                         implied_volatility)])
  }))
  dt.return.this <- dt.return.this[,list(symbol,
                                         type,
                                         expiration,
                                         strike = as.numeric(strike),
                                         last = as.numeric(last),
                                         bid = as.numeric(bid),
                                         ask = as.numeric(ask),
                                         vol = as.numeric(vol),
                                         implied_volatility = as.numeric(gsub("%", "", implied_volatility))/100)]
  return(dt.return.this)
}

funcScanOptionsMarketYahoo <- function(vec.symbols, exp_date, verbose = FALSE, sleep = 0.25){
  vec.symbols <- gsub("\\.", "-", vec.symbols)
  dt.return.this <- rbindlist(lapply(vec.symbols, function(x){
    if (verbose){
      print(x)
    }
    
    dt.temp_raw <- tryCatch(getOptionChain(x, Exp = exp_date),
                            error = function(e){
                              print(glue("No option chain found for {x}"))
                              data.table()
                            })
    if (length(dt.temp_raw) == 0){
      return(dt.temp_raw)
    }
    
    
    if (nrow(data.table(dt.temp_raw$calls)) & nrow(data.table(dt.temp_raw$puts))){
      dt.temp <- rbind(data.table(dt.temp_raw$calls)[,list(type = "Call", Strike, Last, Chg, Bid, Ask, Vol)],
                       data.table(dt.temp_raw$puts)[,list(type = "Put", Strike, Last, Chg, Bid, Ask, Vol)])
    } else if (nrow(data.table(dt.temp_raw$calls))){
      dt.temp <- data.table(dt.temp_raw$calls)[,list(type = "Call", Strike, Last, Chg, Bid, Ask, Vol)]
    } else if (nrow(data.table(dt.temp_raw$puts))){
      dt.temp <- data.table(dt.temp_raw$puts)[,list(type = "Put", Strike, Last, Chg, Bid, Ask, Vol)]
    }
    
    dt.temp[,type := toupper(type)]
    dt.temp[,symbol := x]
    dt.temp[,expiration := exp_date]
    # Necessary to prevent too many request error (HTTP 429)
    Sys.sleep(sleep)
    return(dt.temp[,list(symbol, 
                         type, 
                         expiration, 
                         strike = Strike, 
                         last = Last, 
                         bid = Bid, 
                         ask = Ask, 
                         vol = Vol)])
  }))
  dt.return.this <- dt.return.this[,list(symbol,
                                         type,
                                         expiration,
                                         strike = as.numeric(strike),
                                         last = as.numeric(last),
                                         bid = as.numeric(bid),
                                         ask = as.numeric(ask),
                                         vol = as.numeric(vol))]
  return(dt.return.this)
}

funcBuildVerticalSpread <- function(dt.options, verbose = F){
  # Return a data table of possible profitable option trades
  # Column: id, spread, side, qty, symbol, exp, strike, type, mark, probability_of_profit
  dt.return.this.final <- rbindlist(lapply(unique(dt.options$symbol), function(x){
    dt.temp <- dt.options[symbol == x]
    dt.return.temp <- rbindlist(lapply(c("Call", "Put"), function(y){
      dt.temp_temp <- dt.temp[type == y]
    }))
  }))
}

funcGetMostActiveOptions <- function(bln.all_col){
  # Scrapes most active options for the latest trading day
  # URL: https://www.barchart.com/proxies/core-api/v1/quotes/get?list=options.mostActive.us&fields=symbol%2CsymbolType%2CsymbolName%2ChasOptions%2ClastPrice%2CpriceChange%2CpercentChange%2CtotalOptionsVolume%2CpercentCallOptions%2CpercentPutOptions%2CtradeTime%2CsymbolCode&meta=field.shortName%2Cfield.type%2Cfield.description&hasOptions=true&page=1&limit=100&raw=1
  chr.url <- "https://www.barchart.com/proxies/core-api/v1/quotes/get?list=options.mostActive.us&fields=symbol%2CsymbolType%2CsymbolName%2ChasOptions%2ClastPrice%2CpriceChange%2CpercentChange%2CtotalOptionsVolume%2CpercentCallOptions%2CpercentPutOptions%2CtradeTime%2CsymbolCode&meta=field.shortName%2Cfield.type%2Cfield.description&hasOptions=true&page=1&limit=100&raw=1"
  lst.raw <- fromJSON(txt=chr.url, simplifyDataFrame=T)
  if (bln.all_col){
    dt.temp <- data.table(lst.raw$data$raw)
  } else{
    dt.temp <- data.table(lst.raw$data$raw)[,list(symbol, totalOptionsVolume)]
  }
  return (dt.temp)
}

funcHistoricalEarningsDateScraper <- function(vec.symbols){
  dt.return.this <- rbindlist(lapply(vec.symbols, function(x){
    base_url <- glue("https://ycharts.com/companies/events_data/json?eventTypes=earnings&pageNum=1&securityIds={x}")
    lst.raw <- fromJSON(txt=base_url, simplifyDataFrame=T)
    dt.temp <- data.table(lst.raw$events)
    dt.temp <- dt.temp[,list(symbol = company,
                             earnings_date = as.Date(local_begin_date, "%m/%d/%Y"))]
    return(dt.temp)
  }))
  return(dt.return.this)
}

funcGetHistoricalSPYOptions <- function(curr_date, vec.expiration_dates, chr.symbol = "spy"){
  # Only contains data for the previous 181 trading days
  # Only contains data for spx, dia, spy, rut
  # # Create cluster
  # cl<-makeCluster(detectCores(),type="SOCK")
  # # Load relevant libraries into workers
  # clusterEvalQ(cl, library(data.table))
  # clusterEvalQ(cl, library(jsonlite))
  # clusterEvalQ(cl, library(glue))
  # dt.return.this <- rbindlist(clusterApply(cl, vec.expiration_dates, function(x, curr_date, chr.symbol){
  #   chr.base_url <- glue("https://www.discountoptiondata.com/freedata/getoptiondatajson?symbol={chr.symbol}&datadate={curr_date}&expirationDate={x}")
  #   dt.results <- data.table(fromJSON(txt=chr.base_url, simplifyDataFrame=T))
  #   dt.results[,EvalDate := curr_date]
  #   return(dt.results)
  # }))
  # stopCluster(cl)
  
  dt.return.this <- rbindlist(lapply(vec.expiration_dates, function(x){
    print(x)
    chr.base_url <- glue("https://www.discountoptiondata.com/freedata/getoptiondatajson?symbol={chr.symbol}&datadate={curr_date}&expirationDate={x}")
    dt.results <- data.table(fromJSON(txt=chr.base_url, simplifyDataFrame=T))
    dt.results[,EvalDate := curr_date]
    return(dt.results)
  }))
  
  dt.return.this[,AskPrice := as.numeric(AskPrice)]
  dt.return.this[,BidPrice := as.numeric(BidPrice)]
  dt.return.this[,LastPrice := as.numeric(LastPrice)]
  dt.return.this[,StrikePrice := as.numeric(StrikePrice)]
  dt.return.this[,Volume := as.numeric(Volume)]
  dt.return.this[,ImpliedVolatility := as.numeric(ImpliedVolatility)]
  dt.return.this[,Delta := as.numeric(Delta)]
  dt.return.this[,Vega := as.numeric(Vega)]
  dt.return.this[,OpenInterest := as.numeric(OpenInterest)]
  dt.return.this[,UnderlyingPrice := as.numeric(UnderlyingPrice)]
  
  return (dt.return.this)
}

funcGetHistoricalSPYExpirationDates <- function(curr_date, chr.symbol = "spy"){
  url <- "https://www.discountoptiondata.com/freedata"
  driver <- rsDriver(browser=c("chrome"))
  remDr <- driver[["client"]]
  # remDr$open()
  remDr$navigate(url)
  webElem <- remDr$findElement("id","symbol")
  webElem$sendKeysToElement(list(chr.symbol,key="enter"))
  webElem <- remDr$findElement("id","dataDate")
  webElem$sendKeysToElement(list(curr_date,key="enter"))
  Sys.sleep(1)
  webElem <- remDr$findElement(using = 'xpath', "//*[@id='expirationDate']")
  webElemTag <- webElem$selectTag()
  vec.date_list <- webElemTag$value
  remDr$closeall()
  return (vec.date_list)
}

