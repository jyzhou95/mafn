setwd(dirname(rstudioapi::getSourceEditorContext()$path))
setwd("..")
parent_dir <- getwd()
source(paste0(parent_dir, "/etf_balancing/funcUSefulFunctions.R"))

funcMarkToMarket <- function(dt.positions){
  dt.positions$dt <- as.Date(dt.positions$dt)
  # Get EOD price for all symbol that's not cash
  chr.start_date <- as.Date(min(dt.positions$dt)) - 1
  vec.stocks <- unique(dt.positions$symbol)
  chr.end_date <- as.Date(max(dt.positions$dt)) + 1
  vec.stocks <- vec.stocks[vec.stocks != "CASH"]
  dt.stock_price <- funcGetStockPrice(vec.stocks, start_date = chr.start_date, end_date = chr.end_date)[,list(dt, symbol, adjusted_close)]
  dt.final <- merge(dt.positions, dt.stock_price, by = c("dt", "symbol"), all.x = TRUE)
  dt.final[symbol == "CASH"]$adjusted_close <- 1
  dt.final[,position := qty * adjusted_close]
  dt.return.this <- dt.final[,list(position = sum(position)), by = list(dt)]
  return (dt.return.this)
}