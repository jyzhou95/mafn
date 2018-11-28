library(data.table)
library(quantmod)
library(tseries)
library(timeSeries)
library(glue)
library(snow)
library(parallel)
library(lubridate)
library(ggplot2)

funcGetAllPossiblePairs <- function(vec.stock_list){
  dt.return.this <- data.table(data.frame(t(combn(vec.stock_list, 2))))
  dt.return.this[,id := 1:nrow(dt.return.this)]
  dt.return.this.final <- dt.return.this[,list(symbol1 = X1, symbol2 = X2)]
  return (dt.return.this.final)
}

funcCheckCointegration <- function(chr.month, dt.stocks, dt.pairs){
  # dt.stocks has the following columns: dt, symbol, price
  # dt.pairs has the following columns: symbol1, symbol2
  # dt.return.this has the following columns: dt, symbol1, symbol2, coint
  
  # Create cluster
  cl<-makeCluster(detectCores(),type="SOCK")
  # Load relevant libraries into workers
  clusterEvalQ(cl, library(data.table))
  clusterEvalQ(cl, library(quantmod))
  clusterEvalQ(cl, library(tseries))
  clusterEvalQ(cl, library(timeSeries))
  
  dt.return.this.coint <- rbindlist(clusterApply(cl, 1:nrow(dt.pairs), function(x){
    dt.pairs_temp <- dt.pairs[x]
    stock1 <- dt.pairs_temp$symbol1
    stock2 <- dt.pairs_temp$symbol2
    
    dt.stocks_temp <- dt.stocks[symbol %in% c(as.character(stock1), 
                                              as.character(stock2))]
    
    # Check to make sure that the number of rows is the same for both symbols
    dt.check <- dt.stocks_temp[,.N, by = list(symbol)]
    if (dt.check[1]$N != dt.check[2]$N){
      # Only keep rows where data exists for both symbols
      vec_dates <- intersect(dt.stocks_temp[symbol == stock1]$dt,
                             dt.stocks_temp[symbol == stock2]$dt)
      dt.stocks_temp <- dt.stocks_temp[dt %in% vec_dates]
    }
    
    flt.correlation <- cor(dt.stocks_temp[symbol == stock1]$price, 
                           dt.stocks_temp[symbol == stock2]$price, 
                           method = "pearson")
    
    # dt.plot_temp <- dt.stocks_temp
    # dt.plot_temp[,id := rep(1:(nrow(dt.plot_temp)/2),2)]
    # 
    # ggplot(cbind(dt.stocks_temp[symbol == stock1][,list(stock1_price = price)],
    #              dt.stocks_temp[symbol == stock2][,list(stock2_price = price)]),
    #        aes(x = stock1_price, y = stock2_price)) + geom_point() + theme_bw(base_size = 15)

    
    if (flt.correlation >= 0.9){
      # Do a linear regression fit to find the residual
      comb1 = lm(dt.stocks_temp[symbol == stock1]$price ~ dt.stocks_temp[symbol == stock2]$price)
      comb2 = lm(dt.stocks_temp[symbol == stock2]$price ~ dt.stocks_temp[symbol == stock1]$price)
      
      # Find lowest ADF test value
      comb1_adf <- adf.test(comb1$residuals, k=1)
      comb2_adf <- adf.test(comb2$residuals, k=1)
      
      # Use the ADF test with the lowest ADF value
      if (comb1_adf$statistic < comb2_adf$statistic){
        adf.test.p.value <- as.numeric(comb1_adf$p.value)
        adf.test.value <- as.numeric(comb1_adf$statistic)
        # Get standard error 
        residual <- summary(comb1)$sigma
        beta <- as.numeric(comb1$coefficients[2])
      } else{
        adf.test.p.value <- as.numeric(comb2_adf$p.value)
        adf.test.value <- as.numeric(comb2_adf$statistic)
        # Get standard error 
        residual <- summary(comb2)$sigma
        beta <- as.numeric(comb2$coefficients[2])
      }
      
      if (adf.test.p.value < 0.05){
        return (data.table(dt = chr.month,
                           symbol1 = stock1,
                           symbol2 = stock2,
                           coint = 1,
                           p_value = adf.test.p.value,
                           test_value = adf.test.value,
                           residual_se = residual,
                           beta = beta
        ))
      } else{
        return (data.table(dt = chr.month,
                           symbol1 = stock1,
                           symbol2 = stock2,
                           coint = 0,
                           p_value = NA,
                           test_value = NA,
                           residual_se = NA,
                           beta = NA
        ))
      }
    } else{
      return (data.table(dt = chr.month,
                         symbol1 = stock1,
                         symbol2 = stock2,
                         coint = 0,
                         p_value = NA,
                         test_value = NA,
                         residual_se = NA,
                         beta = NA))
    }
  }))
  stopCluster(cl)
  return (dt.return.this.coint)
}


vec.stock_list <- c('ORCL','TSM','ACN','UPS','RIO','HDB','GS','BLK','EL','ING','TEF',
                    'MET','CCL','CHA','COF','ET','PRU','NOK','ROP','RSG','RCL','A','BXP',
                    'PKX','ESS','PAA','KEP','MKL','WAT','MTD','IT','KOF','TIF','KSS','MLM',
                    'MAA','SQM','COG','MTN','ADS','FBR','IEX','JNPR','HNP','PKG','URI','RL',
                    'SUI','FDS','LII','CPT','CEA','TV','BG','ZNH','SLG','AIV','OHI','MAC',
                    'KRC','GIL','YPF','CRL','ACH','CBD','RS','HAE','EPR','SBS','MSM','CCJ',
                    'BPL','USM','CIEN','HIW','MTG','SMG','SKX','MMS','FR','AUO','ERJ','JBL',
                    'NFX','CLB','LHO','BVN','HR','NUS','DECK','DKS','ASGN','NEA','DNP','TCO',
                    'FUN','CNX','GEL','NVG','BYD')

dt.stock_pairs <- funcGetAllPossiblePairs(vec.stock_list)



# Read and process data here
dt.final_pairs <- rbindlist(lapply(c(2003:2018)), function(x){
  dt.data <- fread(paste0("D:/Desktop/tick_data/year_", x, ".csv"))
  
  # Remove rows without liquidity
  dt.data <- dt.data[bid_size > 0 & ask_size > 0]
  dt.data[,spread := ask/bid - 1]
  
  # Remove rows with large bid ask spread
  dt.spread_rank <- dt.data[,list(spread_threshold = quantile(spread, 0.9)), 
                            by = list(symbol)]
  dt.data <- merge(dt.data, dt.spread_rank, by = c("symbol"))
  dt.data <- dt.data[spread <= spread_threshold]
  
  dt.data_final <- dt.data[,list(dt, symbol, price =  bid + ask / 2)]
  vec.start_months <- floor_date(seq(from = as.Date(head(dt.data$dt, 1)),
                          to = as.Date(tail(dt.data$dt, 1)),
                          by = "1 month"), "month")
  vec.end_months <- vec.start_months + months(1) - 1
  
  dt.return.this <- rbindlist(lapply(1:length(vec.start_months), function(y){
    dt.temp <- dt.data_final[dt >= vec.start_months[y] & dt <= vec.end_months[y]]
    dt.final_temp <- funcCheckCointegration(chr.month = vec.start_months[y],
                                            dt.stocks = dt.temp,
                                            dt.pairs = dt.stock_pairs)
  }))
  
})









