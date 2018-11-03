library(data.table)
library(xts)

# Takes in a data.table or data.frame and returns a xts for OHLC data
funcConvertToXtsOHLC <- function(dt.input_table){
  
  dt.input_table <- dt.input_table[,c("open","high","low",
                                      "close", "volume", "adjusted_close",
                                      "dt")]
  vec.dates <- dt.input_table$dt
  dt.input_table <- data.frame(dt.input_table[,c("open","high","low",
                                                 "close", "volume", "adjusted_close")])
  
  rownames(dt.input_table) <- vec.dates
  
  dt.return.this <- as.xts(as.matrix(dt.input_table))
  return (dt.return.this)
}



