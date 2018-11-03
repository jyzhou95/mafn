# Take an order book object outputted from quantstrat and outputs a CSV 
# with the following columns:
library(data.table)
library(snow)
library(parallel)


funcWriteCsvFromOrderBook <- function(obj, file_name){
  num_stocks <- length(obj[[1]])
  
  dt.write.this <- rbindlist(lapply(1:num_stocks, function(X){
    dt_temp <- data.table(obj[[x]][[1]])
    chr_stock <- names(obj[[1]][x])
    dt_temp <- dt_temp[,list(time = Order.StatusTime,
                             symbol = chr_stock,
                             direction = Order.Side,
                             trade = Rule,
                             qty = Order.Qty,
                             price = Order.Price)] 
  }))
  
  dt.write.this <- data.frame(dt.write.this)
  
  write.csv(dt.write.this, file_name)
}

funcWriteCsvFromOrderBookParallelized <- function(obj, file_name){
  num_stocks <- length(obj[[1]])
  # Create cluster
  cl<-makeCluster(detectCores(),type="SOCK")
  # Load relevant libraries into workers
  clusterEvalQ(cl, library(data.table))
  dt.write.this <- rbindlist(clusterApply(cl, 1:num_stocks, function(x){
    dt_temp <- obj[[x]][[1]]
    chr_stock <- names(obj[[x]])
    dt_temp <- dt_temp[,list(time = Order.StatusTime,
                             symbol = chr_stock,
                             direction = Order.Side,
                             trade = Rule,
                             qty = Order.Qty,
                             price = Order.Price)] 
    return (dt_temp)
  }))
  stopCluster(cl)
  
  dt.write.this <- data.frame(dt.write.this)
  
  write.csv(dt.write.this, file_name)
}

