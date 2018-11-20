library(data.table)

# Get list of daily quotes files and output aggregate files into one year
vec_years <- c(2015:2018)
for (year in vec_years){
  vec_files <- list.files(paste0("D:/Desktop/tick_data/year_", year))
  dt.return.this <- rbindlist(lapply(vec_files, function(x){
    dt.temp <- fread(x)
    return (dt.temp)
  }))
  # Write file
  write.csv(x = dt.return.this, file = paste0("D:/Desktop/tick_data/year_", year, ".csv"))
}
