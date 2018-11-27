library(data.table)

# Get list of daily quotes files and output aggregate files into one year
vec_years <- c(2003:2018)
for (year in vec_years){
  vec_files <- list.files(paste0("D:/Desktop/tick_data/year_", year))
  if (year < 2015){
    dt.return.this <- fread(paste0("D:/Desktop/tick_data/year_", year, "/", vec_files))
    dt.return.this <- dt.return.this[,list(dt = paste(DATE, itime), 
                                           symbol = SYMBOL, 
                                           bid = BID, 
                                           ask = OFR,
                                           bid_size = BIDSIZ,
                                           ask_size = OFRSIZ)]
    
    # Format time
    dt.return.this[,dt := as.POSIXct(dt, format = "%Y%m%d %H:%M:%OS")]
    
  } else{
    dt.return.this <- rbindlist(lapply(vec_files, function(x){
      print(x)
      dt.temp <- fread(paste0("D:/Desktop/tick_data/year_", year, "/", x))
      dt.temp <- dt.temp[,list(dt = paste(DATE, itime),
                               symbol = SYM_ROOT, 
                               bid = BID, 
                               ask = ASK,
                               bid_size = BIDSIZ,
                               ask_size = ASKSIZ)]
      dt.temp[,dt := as.POSIXct(dt, format = "%Y%m%d %H:%M:%OS")]
      return (dt.temp)
    }))
  }
  # Write file
  write.csv(x = dt.return.this, file = paste0("D:/Desktop/tick_data/year_", year, ".csv"))
}
