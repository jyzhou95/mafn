library(ggplot2)
library(plotly)
library(sde)
library(data.table)
library(glue)

source("D:/Desktop/mafn/etf_balancing/funcUsefulFunctions.R")

funcInitialAnalsis <- function(dt.returns){
  # Plot first cut returns
  # require("PerformanceAnalytics",quietly=TRUE)
  # charts.PerformanceSummary(as.xts(dt.returns[,list(dt, ret = daily_ret - 1)]))
  
  #QQ plot of returns
  # vec.ret <- dt.returns$daily_ret - 1
  # y     <- quantile(vec.ret, c(0.25, 0.75), type=5) # Find the 1st and 3rd quartiles
  # x     <- qnorm( c(0.25, 0.75))                 # Find the matching normal values on the x-axis
  # slope <- diff(y) / diff(x)                     # Compute the line slope
  # int   <- y[1] - slope * x[1]                   # Compute the line intercept
  # 
  # ggplot(dt.returns, aes(sample = daily_ret - 1)) + stat_qq() + theme_bw(base_size = 15) +
  #   geom_abline(intercept=int, slope=slope) + ggtitle("QQ plot of daily returns")
  
  ggplot() + 
    geom_line(data = dt.returns, aes(x = dt, y = cum_ret-1, color = type)) + 
    xlab("Date") + ylab("Cumulative Returns") + scale_y_continuous(labels = scales::percent) +
    theme_bw(base_size = 25) +
    ggtitle("Strategy returns from 2004 to 2017") + scale_color_brewer(palette = "Set1")
    
  
}

funcHeatMap <- function(dt.returns){
  dt.returns <- dt.returns[type == "strategy"][,list(dt, daily_ret)]
  data <- dt.returns
  data <- as.xts(data)
  
  # Compute the monthly log return
  require('quantmod')                                     # Load quantmod
  ret <- monthlyReturn(data, type='log', leading=TRUE)    # Compute the monthly returns
  
  # Create a matrix mat object from ret       
  mat <- matrix(ret*100, ncol=12, byrow=TRUE, dimnames=NULL)      # Create a R matrix
  year <- seq(from=2004, to=2017, by=1)                           # Vector containing the years
  month <- c('Jan','Feb','Mar', 'Apr','May','Jun','Jul','Aug','Sep','Oct','Nov','Dec')    # Vector containing the year months
  colnames(mat) <- month  # Give columns names
  rownames(mat) <- year   # Give rows names
  
  # Transform the data in mat into a long series using the melt() function
  require(reshape2)           # Load reshape2
  molten <- melt(mat)         # Transform the series into a long series
  
  # Define the color palette to be used in the hetmap
  LtoM <-colorRampPalette(c('red', 'yellow' ))            # The spectrum of colors for the lowest returns
  Mid <- "snow3"                                          # Snow3 is the color for the (approximatedly) median value
  MtoH <-colorRampPalette(c('lightgreen', 'darkgreen'))   # The spectrum of colors for the highest values
  
  # Plot the heatmap                                          # Load ggplot2      
  hm <- ggplot(data=molten, aes(x=factor(Var2, levels=month.abb),
                                y=Var1, fill=value)) + geom_raster()                        # Draw the heatmap using geom_raster()
  hm <- hm + scale_fill_gradient2(low=LtoM(100), mid=Mid, high=MtoH(100))     # Colors, please!
  hm <- hm + labs(fill='Return (%)')                                          # The legend
  hm <- hm + geom_text(aes(label=paste(sprintf("%.1f %%", value))), size=4)   # Write the monthly returns in hte heatmap
  hm <- hm + scale_y_continuous(breaks=2004:2017)                             # Deal with the y-axis
  hm <- hm + xlab(label=NULL) +   ylab(label=NULL)                            # No axis names
  hm <- hm + theme_bw(base_size = 25)                                                       # No background grey grid
  hm <- hm + theme(axis.text.x=element_text(size=10, hjust=0, vjust=0.4,
                                            angle=90))                                                  # Beautify the x-axis
  hm <- hm + ggtitle(label='Monthly return of strategy')  # Add a title
  hm + coord_flip()
}

funcMarketReturns <- function(dt.final_returns){
  dt.returns <- merge(dt.final_returns[type == "market"][,list(dt, market_ret = daily_ret)],
                            dt.final_returns[type == "strategy"][,list(dt, strategy_ret = daily_ret)],
                            by = c("dt"))
  
  dt.returns[,performance := ifelse(strategy_ret < market_ret,
                                    "underperformance",
                                    "overperformance")]
  ggplot() + 
    geom_point(data = dt.returns, aes(x = market_ret - 1, y = strategy_ret - 1, color = performance), alpha = 0.5) +
    geom_vline(xintercept = 0) + 
    geom_hline(yintercept = 0) + 
    geom_abline(slope = 1, linetype = "dashed") +
    theme_bw(base_size = 25) +
    ggtitle("Daily strategy returns vs market returns ") + 
    scale_x_continuous(labels = scales::percent) + 
    scale_y_continuous(labels = scales::percent) +
    xlab("Market Returns") + ylab("Strategy Returns") +
    scale_color_manual(values=c("#53b987", "#eb4d5c"))
  
  print(nrow(dt.returns[strategy_ret > market_ret]) / nrow(dt.returns))
  print(nrow(dt.returns[strategy_ret < market_ret]) / nrow(dt.returns))
}

funcRunReturnsSimulation <- function(dt.returns, start_date, num_simu = 10000){
  mu <- mean(log(dt.returns[dt < start_date]$daily_ret))
  sigma <- sd(dt.returns[dt < start_date]$daily_ret - 1)
  n <- nrow(dt.returns[dt >= start_date])
  init_price <- tail(dt.returns[dt < start_date]$cum_ret, 1)
  vec_dates <- dt.returns[dt >= start_date]$dt 
    
  dt.return.this <- rbindlist(lapply(1:num_simu, function(x){
    print(glue("Progress: {x/num_simu * 100}%"))
    dt.simulated_returns <- data.table(sim = x,
                                       dt = vec_dates,
                                       simu_ret = init_price * cumprod(exp(rnorm(n, mu, sigma))))
    return (dt.simulated_returns)
  }))
  return (dt.return.this)
}

funcOutSample <- function(dt.returns){
  # In sample: 2003-01-01 to 2017-01-01
  # Out of sample: 2017-01-01 to present
  dt.final <- dt.returns
  dt.final[,type := ifelse(dt <= "2017-01-01", "in-sample", "out-sample")]
  
  # Calculate simulated returns
  dt.simulated_returns <- funcRunReturnsSimulation(dt.returns, start_date = "2017-01-01", 1000)
  
  ggplot() +
    geom_line(data = dt.simulated_returns, aes(x = as.Date(dt), y = simu_ret, group = sim), color = "lightblue", alpha = 0.5) +
    geom_line(data = dt.final, aes(x = as.Date(dt), y = cum_ret, color = type, group = 1), size = 1) +
    theme_bw(base_size = 15) + 
    scale_colour_manual(values = c("#3B434C", "#EB4D5C")) + 
    xlab("Date") + ylab("Cumulative return (%)")
  
}

dt.spy <- funcGetStockPrice(vec.symbols = "SPY", start_date = "2003-12-31", end_date = "2018-01-01")[,list(dt, daily_ret = returns)]
dt.spy[,cum_ret := cumprod(daily_ret)]
dt.spy[,type := "market"]
dt.spy[,dt := as.Date(dt)]

# Read returns data for ETF balancing
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
parent_dir <- getwd()
dt.returns_etf_trading <- fread(paste0(parent_dir, "/etf_balancing.csv"))[,list(dt, daily_ret, cum_ret)]
dt.returns_etf_trading$dt <- as.Date(dt.returns_etf_trading$dt)
dt.returns_etf_trading[,type := "strategy"]
dt.returns_etf_trading <- rbind(dt.returns_etf_trading, dt.spy)

dt.returns_pairs_trading <- fread(paste0(parent_dir, "/pairs_trading.csv"))[,list(dt, daily_ret, cum_ret)]
dt.returns_pairs_trading$dt <- as.Date(dt.returns_pairs_trading$dt)
dt.returns_pairs_trading[,type := "strategy"]
dt.returns_pairs_trading <- rbind(dt.returns_pairs_trading, dt.spy)

SharpeRatio.annualized(dt.returns_etf_trading[type == "strategy",list(dt, ret = daily_ret - 1)])
maxDrawdown(as.xts(dt.returns_etf_trading[type == "strategy",list(dt, ret = daily_ret - 1)]))
tail(dt.returns_etf_trading[type == "strategy"]$cum_ret, 1)^(1/14)


SharpeRatio.annualized(dt.returns_pairs_trading[type == "strategy",list(dt, ret = daily_ret - 1)])
maxDrawdown(as.xts(dt.returns_pairs_trading[type == "strategy",list(dt, ret = daily_ret - 1)]))
tail(dt.returns_pairs_trading[type == "strategy"]$cum_ret, 1)^(1/14)

# Calculate correlation between returns
cor(dt.returns_etf_trading$daily_ret,
    dt.returns_pairs_trading$daily_ret)

dt.final_returns <- merge(dt.returns_etf_trading[type == "strategy",list(dt, etf_returns=daily_ret)], 
                          dt.returns_pairs_trading[type == "strategy",list(dt, pairs_trading_returns = daily_ret)], by = c("dt"))
 
ggplot(dt.final_returns, aes(x = etf_returns - 1, y = pairs_trading_returns -1)) + 
  geom_point() + theme_bw(base_size = 25) +
  geom_smooth(method = "lm", se = FALSE) +
  xlab("ETF Balancing Daily Returns") + ylab("Pairs Trading Daily Returns") + scale_x_continuous(labels = scales::percent) +
  scale_y_continuous(labels = scales::percent)


dt.final_returns$combined_ret <- (dt.final_returns$etf_returns * 0.9 + dt.final_returns$pairs_trading_returns * 0.1)
dt.final_returns$combined_ret_cum <- cumprod(dt.final_returns$combined_ret)
dt.final_returns <- dt.final_returns[,list(dt, 
                                           daily_ret = combined_ret, 
                                           cum_ret = combined_ret_cum)]

dt.final_returns[,type := "strategy"]
dt.final_returns <- rbind(dt.spy, dt.final_returns)

SharpeRatio.annualized(dt.final_returns[type == "strategy",list(dt, ret = daily_ret - 1)])
maxDrawdown(as.xts(dt.final_returns[type == "strategy",list(dt, ret = daily_ret - 1)]))
tail(dt.final_returns$cum_ret, 1)^(1/14)

# Run analyses
funcInitialAnalsis(dt.returns_etf_trading)
funcHeatMap(dt.returns_etf_trading)
funcInitialAnalsis(dt.returns_pairs_trading)
funcHeatMap(dt.returns_pairs_trading)
funcInitialAnalsis(dt.final_returns)
funcHeatMap(dt.final_returns)


cor(dt.returns_etf_trading[type == "market"]$daily_ret,
    dt.returns_etf_trading[type == "strategy"]$daily_ret)
cor(dt.returns_pairs_trading[type == "market"]$daily_ret,
    dt.returns_pairs_trading[type == "strategy"]$daily_ret)
cor(dt.final_returns[type == "market"]$daily_ret,
    dt.final_returns[type == "strategy"]$daily_ret)

# Drawdown analysis for final returns
dt.final_strategy_returns <- dt.final_returns[type == "strategy"][,list(dt, returns = daily_ret)]
dt.final_strategy_returns$returns <- dt.final_strategy_returns$returns - 1

timeSeries_final_strategy_returns <- timeSeries(as.xts(dt.final_strategy_returns))

ts.drawdown <- drawdowns(timeSeries_final_strategy_returns)
dt.drawdown <- data.table(ts.drawdown)
dt.drawdown[,dt := as.Date(rownames(ts.drawdown))]
dt.drawdown <- dt.drawdown[,list(dt, returns)]

#Find largest drawdowns
t <- table.Drawdowns(as.xts(dt.final_strategy_returns))
#Seperate into vecotrs 
s <- c(t[1,1], t[2,1], t[3,1], t[4,1], t[5,1])
e <- c(t[1,2], t[2,2], t[3,2], t[4,2], t[5,2])

#Plot rectangles on price chart
rect <- data.frame(xmin=s, xmax=e, ymin=-Inf, ymax=Inf)


ggplot() +
  geom_line(data = dt.drawdown, aes(x = dt, y = returns), group = 1, color = "#eb4d5c") +
  geom_rect(data=rect, aes(xmin=xmin, xmax=xmax, ymin=ymin, ymax=ymax), color="yellow",fill = "blue", alpha=.1, inherit.aes = FALSE) +
  theme_bw(base_size = 20) + xlab("Date") + ylab("Drawdown %") +
  scale_y_continuous(labels = scales::percent)





