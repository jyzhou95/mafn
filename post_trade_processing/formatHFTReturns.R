library(data.table)
library(glue)


# Iterate over each year's PNL and calculate returns
vec.years <- c(2004:2017)

dt.returns_masters <- rbindlist(lapply(vec.years, function(x){
  print(x)
  dt.temp <- fread(glue("D:/Desktop/mafn/statistical_arbitrage/trading_results/pnl_{x}.csv"))
  if (x > 2004){
    dt.temp <- tail(dt.temp, -1)
  }
  dt.temp$dt <- as.Date(dt.temp$dt)
  dt.return.this <- merge(dt.temp[,list(V1 = max(V1)), by = list(dt)], dt.temp, by = c("dt", "V1"))
  dt.return.this <- dt.return.this[,list(dt, portfolio_val)]
  return (dt.return.this)
}))

dt.returns_masters$daily_ret <- c(NA, tail(dt.returns_masters$portfolio_val, -1)/head(dt.returns_masters$portfolio_val, -1))
dt.returns_masters <- dt.returns_masters[,list(dt, daily_ret)]
dt.returns_masters <- dt.returns_masters[!is.na(daily_ret)]
dt.returns_masters[,cum_ret := cumprod(daily_ret)]

write.csv(dt.returns_masters, "D:/Desktop/mafn/post_trade_processing/pairs_trading.csv")

