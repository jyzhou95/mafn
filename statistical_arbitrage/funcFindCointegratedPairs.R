library(data.table)
library(quantmod)
library(tseries)
library(timeSeries)
library(glue)
library(snow)
library(parallel)

funcGetAllPossiblePairs <- function(vec.stock_list){
  dt.return.this <- data.table(data.frame(t(combn(vec.stock_list, 2))))
  dt.return.this[,id := 1:nrow(dt.return.this)]
  dt.return.this.final <- dt.return.this[,list(symbol1 = X1, symbol2 = X2)]
  return (dt.return.this.final)
}

funcCheckCointegration <- function(chr.month, dt.stocks, dt.pairs){
  # dt.stocks has the following columns: dt, symbol, returns
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
    dt.stocks_temp <- dt.stocks[symbol %in% c(as.character(dt.pairs_temp$symbol1), 
                                              as.character(dt.pairs_temp$symbol2))]
    
    flt.correlation <- cor(dt.stocks_temp[symbol == dt.pairs_temp$symbol1]$returns, 
                           dt.stocks_temp[symbol == dt.pairs_temp$symbol2]$returns, 
                           method = "pearson")
    
    if (flt.correlation >= 0.9){
      # Do a linear regression fit to find the residual
      comb1 = lm(dt.stocks_temp[symbol == dt.pairs_temp$symbol1]$returns ~ dt.stocks_temp[symbol == dt.pairs_temp$symbol2]$returns)
      comb2 = lm(dt.stocks_temp[symbol == dt.pairs_temp$symbol2]$returns ~ dt.stocks_temp[symbol == dt.pairs_temp$symbol1]$returns)
      
      # Use the ADF test with the lowest ADF value
      adf.test.p.value <- ifelse(as.numeric(adf.test(comb1$residuals, k=1)$statistic) < as.numeric(adf.test(comb2$residuals, k=1)$statistic),
                                 as.numeric(adf.test(comb1$residuals, k=1)$p.value),
                                 as.numeric(adf.test(comb2$residuals, k=1)$p.value))
      
      adf.test.value <- ifelse(as.numeric(adf.test(comb1$residuals, k=1)$statistic) < as.numeric(adf.test(comb2$residuals, k=1)$statistic),
                               as.numeric(adf.test(comb1$residuals, k=1)$statistic),
                               as.numeric(adf.test(comb2$residuals, k=1)$statistic))
      
      if (adf.test.p.value < 0.05){
        return (data.table(dt = chr.month,
                           symbol1 = dt.pairs_temp$symbol1,
                           symbol2 = dt.pairs_temp$symbol2,
                           coint = 1,
                           p_value = adf.test.p.value,
                           test_value = adf.test.value
        ))
      }
    }
  }))
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







