library(glue)
library(tidyquant)
library(data.table)
library(XLConnect)
library(quantmod)
library(lubridate)

# [jyz2111@wrds-cloud-login1-h taqms]$ cd cq

# [jyz2111@wrds-cloud-login1-h cq]$ ls -U | tail -4
# ix_cqm_20181115.sas7bdat
# cqm_20181116.sas7bdat
# cqm_20181116.sas7bndx
# ix_cqm_20181116.sas7bdat

# [jyz2111@wrds-cloud-login1-h cq]$ pwd
# /wrds/nyse/sasdata/taqms/cq


dt_symbol_list <- data.table(TTR::stockSymbols(exchange = "NYSE"))
dt_symbol_list <- dt_symbol_list[!is.na(MarketCap)]
dt_symbol_list <- dt_symbol_list[!is.na(IPOyear) & IPOyear < 2003]


toInteger <- function(income){
  amt <- as.numeric(gsub("[A-Z,$]", "", income))
  multiplier <- substring(income, nchar(income))
  multiplier <- dplyr::case_when(multiplier == "M" ~ 1e6,
                                 multiplier == "B" ~ 1e9,
                                 TRUE ~ 1) # you can add on other conditions for more suffixes
  amt*multiplier
}

funcGetTradingDate <- function(start_date, end_date){
  vec.dates <- index(getSymbols(Symbols = "SPY", from = start_date, to = end_date, auto.assign = F))
  return (vec.dates)
}

# Get top 100 stocks
dt_symbol_list$MarketCap <- toInteger(dt_symbol_list$MarketCap)

dt_symbol_list_final <- head(dt_symbol_list[order(MarketCap, decreasing = TRUE)], 100)

vec.dates <- funcGetTradingDate("2015-01-01", Sys.Date())

for (dt in as.character(vec.dates)){
  int_year <- year(dt)
  str_dt <- gsub("-", "", dt)
  chr_base <- glue("libname taq'/wrds/nyse/sasdata/taqms/cq'; *After 2014 use this;
                   options source;
                   
                   %let start_time = '9:30:00't; * starting time;
                   %let interval_seconds = 60; * interval is 15*60 seconds, 30 min;
                   
                   * Time is between 9:30am and 4:00pm, retrieving SYMBOL DATE TIME and PRICE;
                   data my_temp;
                   set taq.cqm_{str_dt}:;
                   where sym_root in ('ORCL','TSM','ACN','UPS','RIO','HDB','GS','BLK','EL','ING','TEF',
                   'MET','CCL','CHA','COF','ET','PRU','NOK','ROP','RSG','RCL','A','BXP',
                   'PKX','ESS','PAA','KEP','MKL','WAT','MTD','IT','KOF','TIF','KSS','MLM',
                   'MAA','SQM','COG','MTN','ADS','FBR','IEX','JNPR','HNP','PKG','URI','RL',
                   'SUI','FDS','LII','CPT','CEA','TV','BG','ZNH','SLG','AIV','OHI','MAC',
                   'KRC','GIL','YPF','CRL','ACH','CBD','RS','HAE','EPR','SBS','MSM','CCJ',
                   'BPL','USM','CIEN','HIW','MTG','SMG','SKX','MMS','FR','AUO','ERJ','JBL',
                   'NFX','CLB','LHO','BVN','HR','NUS','DECK','DKS','ASGN','NEA','DNP','TCO',
                   'FUN','CNX','GEL','NVG','BYD') and time_m between '9:30:00't and '16:00:00't;
                   run;
                   
                   proc sort; by sym_root date time_m; run;
                   
                   data xtemp2;
                   set my_temp;
                   by sym_root date time_m;
                   format itime rtime time12.;
                   if first.sym_rootl=1 or first.date=1 then do;
                   rtime=time_m;
                   iprice=bid;
                   oprice=ask;
                   itime= &start_time;
                   end;
                   if time_m >= itime then do; *Interval reached;
                   output; *rtime and iprice hold the last observation values;
                   itime = itime + &interval_seconds;
                   do while(time_m >= itime); *need to fill in all time intervals;
                   output;
                   itime = itime + &interval_seconds;
                   end;
                   end;
                   rtime=time_m;
                   iprice=bid;
                   oprice=ask;
                   retain itime rtime iprice oprice; *Carry time and price values forward;
                   *keep symbol date itime iprice rtime;
                   run;
                   
                   Title 'Final output -- XX min interval';
                   
                   proc export data= xtemp2
                   outfile= '/home/columbia/jyz2111/sas_test_files/tick_data_folder/year_{int_year}/{dt}.csv'
                   dbms=CSV REPLACE;
                   putnames=YES;
                   run;
                   ")
  # Write SAS code
  # write_file(chr_base, path = paste0("C:/Users/jyzho/Documents/GitHub/mafn/data_acquisition_scripts/scraper", int_year, ".sas"))
  write_file(chr_base, path = paste0("D:/Desktop/mafn/data_acquisition_scripts/post_2014/scraper", dt, ".sas"))
}

# my_sas=$(find ~/sas_test_files/post_2014_scraper/ -type f -name "*.sas")
# for file in $my_sas
# do
# qsas "$file"
# sleep 3m
# done
