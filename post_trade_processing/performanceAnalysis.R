library(ggplot2)
library(plotly)
library(sde)
library(data.table)
library(glue)

funcInitialAnalsis <- function(dt.returns){
  # Plot first cut returns
  require("PerformanceAnalytics",quietly=TRUE)
  charts.PerformanceSummary(dt.returns[,list(dt, ret = daily_ret - 1)])
  
  #QQ plot of returns
  vec.ret <- dt.returns$daily_ret - 1
  y     <- quantile(vec.ret, c(0.25, 0.75), type=5) # Find the 1st and 3rd quartiles
  x     <- qnorm( c(0.25, 0.75))                 # Find the matching normal values on the x-axis
  slope <- diff(y) / diff(x)                     # Compute the line slope
  int   <- y[1] - slope * x[1]                   # Compute the line intercept
  
  ggplot(dt.returns, aes(sample = daily_ret - 1)) + stat_qq() + theme_bw(base_size = 15) +
    geom_abline(intercept=int, slope=slope) + ggtitle("QQ plot of daily returns")
  
  # Monthly returns heatmap
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

# Read returns data
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
parent_dir <- getwd()
dt.returns <- fread(paste0(parent_dir, "/etf_balancing.csv"))[,list(dt, daily_ret, cum_ret)]
  