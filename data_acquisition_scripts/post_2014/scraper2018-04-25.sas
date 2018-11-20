libname taq'/wrds/nyse/sasdata/taqms/cq'; *After 2014 use this;
options source;

                   %let start_time = '9:30:00't; * starting time;
%let interval_seconds = 60; * interval is 15*60 seconds, 30 min;

                   * Time is between 9:30am and 4:00pm, retrieving SYMBOL DATE TIME and PRICE;
data my_temp;
set taq.cqm_20180425:;
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
outfile= '/home/columbia/jyz2111/sas_test_files/tick_data_folder/year_2018/2018-04-25.csv'
dbms=CSV REPLACE;
putnames=YES;
run;