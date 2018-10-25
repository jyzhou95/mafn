library(data.table)
library(quantmod)
library(tseries)
library(timeSeries)
library(glue)
# Used for clustering algorithm
library(dbscan)

funcCheckCointegration <- function(dt.stocks, dt.pairs, verbose = FALSE){
  # dt.stocks has the following columns: dt, symbol, returns
  # dt.pairs has the following columns: id, stock_1, stock_2, industry
  # dt.return.this has the following column id, stock_1, stock_2
  dt.return.this.coint <- rbindlist(lapply(1:nrow(dt.pairs), function(x){
    dt.pairs_temp <- dt.pairs[x]
    dt.stocks_temp <- dt.stocks[symbol %in% c(as.character(dt.pairs_temp$stock_1), as.character(dt.pairs_temp$stock_2))]
    
    if (verbose){
      print(glue("Testing pair: {dt.pairs_temp$stock_1} and {dt.pairs_temp$stock_2}"))
      print(glue("Progress: {round(x/nrow(dt.pairs) * 100, 2)}%"))
    }
    
    flt.correlation <- cor(dt.stocks_temp[symbol == dt.pairs_temp$stock_1]$returns, 
        dt.stocks_temp[symbol == dt.pairs_temp$stock_2]$returns, 
        method = "pearson")
    
    if (flt.correlation >= 0.9){
      if (verbose){
        print(glue("Pair Correlated with correlation: {flt.correlation}"))
      }
      
      
      # Do a linear regression fit to find the residual
      comb1 = lm(dt.stocks_temp[symbol == dt.pairs_temp$stock_1]$returns ~ dt.stocks_temp[symbol == dt.pairs_temp$stock_2]$returns)
      comb2 = lm(dt.stocks_temp[symbol == dt.pairs_temp$stock_2]$returns ~ dt.stocks_temp[symbol == dt.pairs_temp$stock_1]$returns)
        
      # Use the ADF test with the lowest ADF value
      adf.test.p.value <- ifelse(as.numeric(adf.test(comb1$residuals, k=1)$statistic) < as.numeric(adf.test(comb2$residuals, k=1)$statistic),
                                 as.numeric(adf.test(comb1$residuals, k=1)$p.value),
                                 as.numeric(adf.test(comb2$residuals, k=1)$p.value))
      
      adf.test.value <- ifelse(as.numeric(adf.test(comb1$residuals, k=1)$statistic) < as.numeric(adf.test(comb2$residuals, k=1)$statistic),
                                 as.numeric(adf.test(comb1$residuals, k=1)$statistic),
                                 as.numeric(adf.test(comb2$residuals, k=1)$statistic))
        
      if (adf.test.p.value < 0.05){
        if (verbose){
          print(glue("ADF test passed with p value: {adf.test.p.value}"))
        }
        return (data.table(stock_1 = dt.pairs_temp$stock_1,
                           stock_2 = dt.pairs_temp$stock_2,
                           industry = dt.pairs_temp$industry,
                           p_value = adf.test.p.value,
                           test_value = adf.test.value
                           )
                )
        }
    }
  }))
}

funcGetPairs <- function(dt.stock_list){
  dt.return.this <- rbindlist(lapply(unique(dt.stock_list$sector), function(x){
    dt.temp <- dt.stock_list[sector == x]
    dt.final_temp <- data.table(data.frame(t(combn(dt.temp$symbol, 2))))
    dt.final_temp[,industry := x]
    colnames(dt.final_temp) <- c("stock_1", "stock_2", "industry")
    return (dt.final_temp)
  }))
  dt.return.this[,id := 1:nrow(dt.return.this)]
  dt.return.this.final <- dt.return.this[,list(id, stock_1, stock_2, industry)]
  return (dt.return.this.final)
}

SYMs <- TTR::stockSymbols()
dt.stock_list <- data.table(SYMs)
dt.stock_list <- dt.stock_list[!is.na(Sector)]
dt.stock_list <- dt.stock_list[!is.na(MarketCap)]
# Convert market capitalization number to numeric
dt.stock_list$MarketCap <- as.numeric(
  sub("\\$(\\d+(\\.\\d+)?)[A-Z]?", "\\1", dt.stock_list$MarketCap)) * 
  ifelse(gsub("[^A-Z]", "", dt.stock_list$MarketCap) == "M", 1e6,
         ifelse(gsub("[^A-Z]", "", dt.stock_list$MarketCap) == "B", 1e9, 1.0)) 
dt.stock_list <- dt.stock_list[,list(symbol = Symbol,
                                     name = Name,
                                     market_cap = MarketCap,
                                     sector = Sector)]
# Filter out miscellaneous
dt.stock_list <- dt.stock_list[sector != "Miscellaneous"]
# Only trade copmanies that have over 1 billion in market capitalization
dt.stock_list <- dt.stock_list[market_cap > 1e9]

# Unique just in case
dt.stock_list <- unique(dt.stock_list, by = c("symbol"))

# USe top 500 stocks
dt.stock_list <- head(dt.stock_list[order(market_cap, decreasing = TRUE)], 1000)

dt.stocks <- funcGetStockPrice(dt.stock_list$symbol, "2015-01-01", "2015-12-31", TRUE)

# Make sure each stock has the same rows of data
dt.count <- dt.stocks[,.N, by = symbol][order(N)]
dt.count <- dt.count[N == max(dt.count$N)] 

# Get stock pairs
dt.pairs <- funcGetPairs(dt.stock_list[symbol %in% unique(dt.count$symbol)])

dt.cointegrated_pairs <- funcCheckCointegration(dt.stocks, dt.pairs, TRUE)





