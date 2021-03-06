libname taq'/wrds/nyse/sasdata/taqms/cq'; *After 2014 use this;
options source;

%let start_time = '9:30:00't; * starting time;
%let interval_seconds = 60; * interval is 15*60 seconds, 30 min;

* Extract all data available in 2009, and 2 stocks (GE and C);
* Time is between 9:30am and 4:00pm, retrieving SYMBOL DATE TIME and PRICE;
data my_temp;
	set taq.cqm_20181002:;
	where symbol in ('GE') and time between '9:30:00't and '16:00:00't;
run;

proc sort; by symbol date time; run;

data xtemp2;
	set my_temp;
	by symbol date time;
	format itime rtime time12.;
	if first.symbol=1 or first.date=1 then do;
		*Initialize time and price when new symbol or date starts;
		rtime=time;
		iprice=bid;
		oprice=ofr;
		itime= &start_time;
	end;
	if time >= itime then do; *Interval reached;
		output; *rtime and iprice hold the last observation values;
		itime = itime + &interval_seconds;
		do while(time >= itime); *need to fill in all time intervals;
			output;
			itime = itime + &interval_seconds;
		end;
	end;
	rtime=time;
	iprice=bid;
	oprice=ofr;
	retain itime rtime iprice oprice; *Carry time and price values forward;
	*keep symbol date itime iprice rtime;
run;

Title "Final output -- XX min interval";

proc export data= xtemp2
		outfile= '/home/columbia/jyz2111/sas_test_files/tick_data_folder/2018/ge_2018.csv'
		dbms=CSV REPLACE;
	putnames=YES;
run;

