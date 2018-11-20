library(data.table)
library(quantmod)
library(tseries)
library(timeSeries)
library(glue)
library(rvest)
library(V8)
library(jsonlite)
library(snow)
library(parallel)
library(corrplot)
library(RColorBrewer)
library(Rsolnp)
library(lubridate)
library(ggplot2)
library(plotly)

# Get correct directory
setwd(dirname(rstudioapi::getSourceEditorContext()$path))
parent_dir <- getwd()
source(glue("{parent_dir}/funcUsefulFunctions.R"))



# Contains miscellaneous analyses
funcRunDailyReturnMonteCarlo <- function(symbol, flt.hypothetical_return = 0.05, num_days = 1, bln.plot_all_curves = T){
  dt.returns <- funcGetStockPrice(vec.symbols = symbol,
                                  start_date = "2000-01-01")
  dt.returns[,returns := returns - 1]
  dt.returns[,percentage_returns := returns * 100]
  
  # Create lognormal distribution
  dt.returns[,returns_log := log(returns + 1)]
  mean_returns_log <- mean(dt.returns$returns_log)
  sd_returns_log <- sd(dt.returns$returns_log)
  
  # Get our simulated returns
  
  dt_simulated_returns_all <- rbindlist(lapply(1:20000, 
                                               function(x){
                                                 lst.temp <- exp(rnorm(n = num_days, mean = mean_returns_log, sd = sd_returns_log)) 
                                                 data.table(id = x,
                                                            time = 1:num_days,
                                                            "all_returns" = lst.temp - 1,
                                                            "cum_returns" = cumprod(lst.temp) - 1)
                                               }
  )
  )
  
  
  dt.simulated_returns <- dt_simulated_returns_all[,list(returns = prod(all_returns + 1) - 1),
                                                   by = list(id)]
  dt.simulated_returns[,percentage_returns := returns * 100]
  
  # Calculate probability that simulated returns exceeds our threshold return
  flt.prob <- ifelse(flt.hypothetical_return < 0,
                     nrow(dt.simulated_returns[percentage_returns > flt.hypothetical_return * 100]) / nrow(dt.simulated_returns),
                     nrow(dt.simulated_returns[percentage_returns < flt.hypothetical_return * 100]) / nrow(dt.simulated_returns)
  )
  
  my_plot <- ggplot(dt.simulated_returns, aes(returns)) + geom_density(size = 2) + 
    geom_vline(xintercept = flt.hypothetical_return, size = 1, color = "red") +
    xlab("EOD returns") + ylab("Density") + theme_bw(base_size = 20) + 
    scale_x_continuous(labels = scales::percent) +
    ggtitle(paste0(symbol, " ", num_days, " days simulated return distribution",
                   "\nProbability of within range: ", round(flt.prob, 3)))
  
  # Adding shading
  df.simulated_returns_within_range <- ggplot_build(my_plot)$data[[1]]
  
  if (flt.hypothetical_return < 0){
    df.simulated_returns_within_range <- subset(df.simulated_returns_within_range, x > flt.hypothetical_return)
  } else{
    df.simulated_returns_within_range <- subset(df.simulated_returns_within_range, x < flt.hypothetical_return)
  }
  
  my_plot <- my_plot + geom_area(data = df.simulated_returns_within_range, aes(x=x,y=y),
                                 fill = "lightblue")
  
  
  # Show all simulations
  if (bln.plot_all_curves){
    dt_simulated_returns_all <- rbind(dt_simulated_returns_all,
                                      data.table(id = 1:20000,
                                                 time = 0,
                                                 all_returns = 0,
                                                 cum_returns = 0))
    plt.all_simulations <- ggplot(dt_simulated_returns_all, aes(x = time, y = cum_returns, color = id, group = id)) + geom_line(size = 1) +
      theme_bw(base_size = 20) + xlab("Time forward (days)") + ylab("Returns") + scale_y_continuous(labels = scales::percent) +
      ggtitle(paste0(symbol, " ", num_days, " days simulated returns"))
  } else{
    plt.all_simulations <- ggplot()
  }
  
  
  return (list(my_plot,
               plt.all_simulations))
}

funcRunEarningsAnalyses <- function(symbol, num_days_since_earnings = 1, bln.after_hours = T){
  dt.earnings <- funcHistoricalEarningsDateScraper(symbol)
  dt.earnings <- dt.earnings[earnings_date <= Sys.Date()]
  
  dt.stock_price <- funcGetStockPrice(symbol, start_date = min(dt.earnings$earnings_date)-1)
  
  # So unfortunately ycharts doesn't tell us whether or not a company's earnings announcement
  # occurs post or pre market
  dt.earnings_drift <- rbindlist(lapply(1:nrow(dt.earnings), function(i){
    dt.temp <- dt.stock_price[dt >= dt.earnings[i]$earnings_date]
    dt.return <- head(dt.temp, num_days_since_earnings+1)
    if (nrow(dt.return) < num_days_since_earnings){
      return (data.table())
    }
    dt.return[,cum_prod_return := cumprod(returns)]
    dt.return[,days_since_earnings := 0:num_days_since_earnings]
    return(dt.return)
  }))
  dt.earnings_drift[,quarters := paste(year(as.Date(dt)), quarters(as.Date(dt)))]
  # Boxplot
  plt1 <- ggplot(dt.earnings_drift, aes(x = as.character(days_since_earnings), y = returns)) + geom_boxplot() + theme_bw(base_size = 20) +
    xlab("Days Since Earnings") + ggtitle(glue("{symbol}'s stock movement after earnings"))
  
  # Distribution of outcomes
  plt2 <- ggplot(dt.earnings_drift[days_since_earnings == num_days_since_earnings], aes(cum_prod_return - 1)) + geom_density(size = 2) + 
    geom_vline(xintercept = median(dt.earnings_drift[days_since_earnings == num_days_since_earnings]$cum_prod_return - 1), color = "red", size = 1) +
    theme_bw(base_size = 20) + xlab("Percentage returns") + 
    ggtitle(glue("{symbol}'s price movement after earnings ({num_days_since_earnings} days out)")) + 
    scale_x_continuous(labels = scales::percent)
  
  # Plot historical earnings data
  if (bln.after_hours){
    dt.earnings_drift <- dt.earnings_drift[days_since_earnings == 1]
  } else{
    dt.earnings_drift <- dt.earnings_drift[days_since_earnings == 0]
  }
  plt3 <- ggplot() +
    geom_bar(data = dt.earnings_drift, aes(x = quarters, y = returns - 1, fill = ifelse(returns >= 1, "#53b987", "#eb4d5c")), 
             stat = "identity", position = "dodge") + 
    geom_hline(yintercept = 0) + theme_bw(base_size = 20) + scale_fill_manual(values=c("#53b987", "#eb4d5c")) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1), legend.position="none") + 
    xlab("Quarters") + ylab("Returns") + scale_y_continuous(labels = scales::percent) +
    ggtitle(glue("Earnings movement day of or day after for {symbol}"))
  
  return(list(plt1, plt2, plt3))
}

funcStockCorMat <- function(vec.symbols){
  # vec.symbols <- c("SPY", "GLD", "TLT", "EEM", "VNQ")
  dt.returns <- funcGetStockPrice(vec.symbols = vec.symbols,
                                  start_date = "2000-01-01")
  # Get maximum of minimum dates for each symbol to determine the most limiting date
  max_dt <- max(dt.returns[,list(earliest_dt = min(dt)), by = list(symbol)]$earliest_dt)
  
  # Filter to make sure each symbol has the same number of rows
  dt.returns <- dt.returns[dt >= max_dt]
  
  # Calculate cumulative returns
  dt.cum_returns <- dt.returns[,list(dt, cum_returns = cumprod(returns)), by = list(symbol)]
  # Add first point
  dt.cum_returns <- rbind(dt.cum_returns,
                          data.table(dt = max_dt - 1,
                                     symbol = vec.symbols,
                                     cum_returns = 1))
  
  # Transform data to wide
  dt.returns.wide <- dcast.data.table(dt.returns[,list(dt,symbol,returns)], dt ~ symbol)
  # Remove date column
  dt.returns.wide[,dt := NULL]
  
  cor.mat <- cor(as.matrix(dt.returns.wide))
  
  col <- colorRampPalette(c("#BB4444", "#EE9988", "#FFFFFF", "#77AADD", "#4477AA"))
  
  
  plt.corr <- corrplot(cor.mat, method="color", col=col(200),  
                       type="upper", 
                       addCoef.col = "black", # Add coefficient of correlation
                       tl.col="black", tl.srt=45 #Text label color and rotation
  )
  
  plt.tseries <- ggplot(dt.cum_returns, aes(x = dt, y = cum_returns, group = symbol, color = symbol)) + geom_line(size = 1) +
    theme_bw(base_size = 20) + xlab("Date") + ylab("Returns") + scale_y_log10(labels = scales::percent) + scale_colour_brewer(palette = "Set1")
  
  return(list(plt.corr, plt.tseries))
  
}


################################################# Useful functions for running portfolio optimization #################################################
calculateSharpe <- function(lst.param, dt.raw_data){
  vec.ret <- as.numeric(as.matrix(dt.raw_data) %*% as.matrix(unlist(lst.param)))
  sharpe_ratio = (mean(vec.ret) - 1) / sd(vec.ret) * sqrt(252)
  return (sharpe_ratio * -1)
}

equal <- function(x, dt.raw_data) {
  return(sum(x))
}

funcFindOptimalWeighting <- function(dt.portfolio, lower_bound_weighting, upper_bound_weighting){
  
  lst.optim_param <- solnp(rep(1/ncol(dt.portfolio), ncol(dt.portfolio)), # Equal weighting for initial value
                           calculateSharpe, 
                           eqfun=equal,
                           eqB=1,
                           LB = rep(lower_bound_weighting, ncol(dt.portfolio)),
                           UB = rep(upper_bound_weighting, ncol(dt.portfolio)),
                           dt.raw_data = dt.portfolio)
  return(lst.optim_param)
}

funcRunMarkowitz <- function(vec.symbols, start_date = "2000-01-01", end_date = Sys.Date(), 
                             lower_bound_weighting = 0, upper_bound_weighting = 1){
  # Calculate efficient frontier based on Sharpe Ratio given a list of stocks
  dt.returns <- funcGetStockPrice(vec.symbols = vec.symbols,
                                  start_date = start_date,
                                  end_date = end_date)
  # Get maximum of minimum dates for each symbol to determine the most limiting date
  max_dt <- max(dt.returns[,list(earliest_dt = min(dt)), by = list(symbol)]$earliest_dt)
  
  # Filter to make sure each symbol has the same number of rows
  dt.returns <- dt.returns[dt >= max_dt]
  # Transform data to wide
  dt.returns.wide <- dcast.data.table(dt.returns[,list(dt,symbol,returns)], dt ~ symbol)
  dt.returns.wide[,dt := NULL]
  lst.optim <- funcFindOptimalWeighting(dt.returns.wide, lower_bound_weighting, upper_bound_weighting)
  vec.param <- lst.optim$pars
  flt.optimized_sharpe <- tail(lst.optim$values, 1) * -1
  dt.param <- data.table(training_start = start_date,
                         training_end = end_date,
                         symbol = colnames(dt.returns.wide),
                         weighting = vec.param)
  return (list(dt.param,
               flt.optimized_sharpe))
}

funcBruteForceFrontier <- function(dt.returns, num_simu = 100000){
  # Generate 10000 portfolios for fun
  dt.frontier <- rbindlist(lapply(1:num_simu, function(x){
    print(glue("Progress: {x / num_simu * 100} %"))
    # Generate random numbers that add up to 1
    rand_weights <- runif(ncol(dt.returns))
    vec.tmp_weigths <- rand_weights / sum(rand_weights)
    vec.tmp_returns <- as.numeric(as.matrix(dt.returns) %*% as.matrix(unlist(vec.tmp_weigths)))
    return (data.table(id = x,
                       ev = mean(vec.tmp_returns) - 1,
                       risk = sd(vec.tmp_returns)))
  }))
  dt.frontier[,sharpe_ratio := ev / risk * sqrt(252)]
  plt.frontier <- ggplot(dt.frontier[ev > 0], aes(x = risk * sqrt(252), y = ev * 252, color = sharpe_ratio)) + geom_point() + 
    scale_x_continuous(labels = scales::percent) + scale_y_continuous(labels = scales::percent) + scale_colour_viridis_c() +
    theme_bw(base_size = 20) + xlab("Risk") + ylab("Returns")
}

funcRunSimpleBacktestPortfolioWeighting <- function(dt.weighting, leverage = 1){
  # Runs a simple backtest for a strategy that involves portfolio rebalancing 
  # Weighting contains these columns: dt, symbol, weighting
  start_dt <- min(dt.weighting$dt)
  end_dt <- max(dt.weighting$dt)
  vec.symbols <- unique(dt.weighting$symbol)
  dt.all_price <- funcGetStockPrice(vec.symbols, start_dt, end_dt)[,list(dt, symbol, returns)]
  dt.merged <- merge(dt.all_price, dt.weighting, by = c("dt", "symbol"))
  dt.merged[,returns_adjusted := weighting * ((returns - 1) * leverage + 1)]
  dt.return.this <- dt.merged[,list(daily_ret = sum(returns_adjusted)), by = list(dt)]
  dt.return.this <- dt.return.this[,cum_ret := cumprod(daily_ret)]
  return (dt.return.this)
}


################################################# Options analyses #################################################

# Helper functions
funcPlotOptionsData <- function(dt.options_data){
  dt.options_data[,moneyness := log(UnderlyingPrice/StrikePrice)]
  dt.options_data[,days_to_expiration := as.numeric(as.Date(ExpirationDate) - as.Date(EvalDate))]
  
  call_iv <- plot_ly(dt.options_data[PutCall == "call"], 
                             x = ~days_to_expiration,
                             y = ~moneyness,
                             z = ~ImpliedVolatility,
                             mode = "markers",
                             type = "scatter3d",
                             marker = list(color = ~ImpliedVolatility, 
                                           colorscale = "Jet", showscale = TRUE)) %>%
                             layout(scene = list(xaxis = list(title = 'Time To Maturity'),
                                                 yaxis = list(title = 'log Moneyness log(S/K)'),
                                                 zaxis = list(title = 'Implied Volatility')))
  
  put_iv <- plot_ly(dt.options_data[PutCall == "put"], 
                             x = ~days_to_expiration,
                             y = ~moneyness,
                             z = ~ImpliedVolatility,
                             mode = "markers",
                             type = "scatter3d",
                             marker = list(color = ~ImpliedVolatility, 
                                           colorscale = "Jet", showscale = TRUE)) %>%
    layout(scene = list(xaxis = list(title = 'Time To Maturity'),
                        yaxis = list(title = 'log Moneyness log(S/K)'),
                        zaxis = list(title = 'Implied Volatility')))
  
  return (call_iv, put_iv)
  
  
}

funcFitVolSurface <- function()

# Run analyses functions
funcRunInitialAnalyses <- function(chr.symbol, curr_date = "most_recent"){
  if (curr_date == "most_recent"){
    curr_date <- tail(funcGetTradingDays(chr.symbol, Sys.Date() - 10, Sys.Date()), 1)
  }
  vec.expiration_dates <- funcGetHistoricalSPYExpirationDates(curr_date, tolower(chr.symbol))
  dt.options_data <- funcGetHistoricalSPYOptions(curr_date,vec.expiration_dates,tolower(chr.symbol))
  # funcPlotOptionsData(dt.options_data)
}





