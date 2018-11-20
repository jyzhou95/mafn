library(ggplot2)
library(plotly)

# Plot first cut returns
require("PerformanceAnalytics",quietly=TRUE)
charts.PerformanceSummary(dt.returns[,list(dt, ret = daily_ret - 1)])

# In sample: 2003-01-01 to 2017-01-01
# Out of sample: 2017-01-01 to present

ggplot(dt.returns, aes(x = dt, y = cum_ret)) + geom_line() + 
