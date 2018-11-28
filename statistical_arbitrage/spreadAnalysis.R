library(data.table)
library(quantmod)
library(tseries)
library(timeSeries)
library(glue)
library(snow)
library(parallel)
library(lubridate)
library(ggplot2)
library(plotly)

dt.data <- fread(paste0("D:/Desktop/tick_data/year_2003.csv"))

# Remove rows without liquidity
dt.data <- dt.data[bid_size > 0 & ask_size > 0]
dt.data[,spread := ask/bid - 1]

vec.stock_list <- c('ORCL','TSM','ACN','UPS','RIO','HDB','GS','BLK','EL','ING','TEF',
                    'MET','CCL','CHA','COF','ET','PRU','NOK','ROP','RSG','RCL','A','BXP',
                    'PKX','ESS','PAA','KEP','MKL','WAT','MTD','IT','KOF','TIF','KSS','MLM',
                    'MAA','SQM','COG','MTN','ADS','FBR','IEX','JNPR','HNP','PKG','URI','RL',
                    'SUI','FDS','LII','CPT','CEA','TV','BG','ZNH','SLG','AIV','OHI','MAC',
                    'KRC','GIL','YPF','CRL','ACH','CBD','RS','HAE','EPR','SBS','MSM','CCJ',
                    'BPL','USM','CIEN','HIW','MTG','SMG','SKX','MMS','FR','AUO','ERJ','JBL',
                    'NFX','CLB','LHO','BVN','HR','NUS','DECK','DKS','ASGN','NEA','DNP','TCO',
                    'FUN','CNX','GEL','NVG','BYD')

# For each stock find the 1st to 99th percentile spread
dt.spread_rank <- dt.data[,list(percentile = seq(from = 0.01, to = 0.99, by = 0.01),
                                spread_val = quantile(spread, seq(from = 0.01, to = 0.99, by = 0.01))), 
                          by = list(symbol)]

a <- ggplot(dt.spread_rank[percentile < 0.91], aes(x = percentile, y = spread_val, group = symbol, color = symbol)) + geom_line() +
  theme_bw(base_size = 15) + scale_y_log10(labels = scales::percent) + xlab("Percentile") + ylab("Bid-ask spread percentage")
ggplotly(a)



