* Remote work analysis

cd "/Users/Garima/Dropbox (MIT)/remote_work"

import delimited "gcs-apr5_w_states.csv", clear

gen qnum=1 if q=="I continue to commute to work"
replace qnum=2 if q=="I have recently been furloughed or laid-off"
replace qnum=3 if q=="Used to commute, now work from home"
replace qnum=4 if q=="Used to work from home and still do"
replace qnum=5 if q=="Used to work from home, but now I commute"
replace qnum=6 if q=="None of the above / Not working for pay"

drop if qnum==6

gen commute_before=(qnum==1 | qnum==2 | qnum==3)
gen wfh_before=(qnum==4 | qnum==5)
gen wfh_now=(qnum==3 | qnum==4)
gen switched_to_remote1=(qnum==3)
gen still_commute=(qnum==1)
gen laid_off=(qnum==2)

bys state: egen switched_to_remote_s=sum(switched_to_remote1)
bys state: egen switched_to_remote_frac_s=mean(switched_to_remote1)

bys state: egen commute_before_s=sum(commute_before)
bys state: egen commute_before_frac_s=mean(commute_before)

bys state: egen wfh_before_s=sum(wfh_before)
bys state: egen wfh_before_frac_s=mean(wfh_before)

bys state: egen still_commute_s=sum(still_commute)
bys state: egen still_commute_frac_s=mean(still_commute)

bys state: egen laid_off_s=sum(laid_off)
bys state: egen laid_off_frac_s=mean(laid_off)

bys state: egen wfh_now_s=sum(wfh_now)
bys state: egen wfh_now_frac_s=mean(wfh_now)

duplicates drop state, force

keep state *_s
ren state statecode
tempfile survey
save `survey', replace

* Merging with COVID cases till date

import excel "statewide_covid_asofApril42020.xlsx", clear firstrow
merge 1:1 statecode using `survey'
keep if _merge==3
drop _merge

lab var switched_to_remote_frac_s "Used to commute, now work from home"
lab var still_commute_frac_s "I continue to commute to work"
lab var laid_off_frac_s "I have recently been furloughed or laid-off"
lab var wfh_before_frac_s "Fraction that worked from home before COVID"

gen lncaseper100k=ln(cases_per100k)
lab var lncaseper100k "Log cases per 100k population"

reg switched_to_remote_frac_s lncaseper100k wfh_before_frac_s, r
outreg2 using "covid_remotework_apr5_control.doc", replace label ctitle("Used to commute, now work from home")

reg still_commute_frac_s lncaseper100k wfh_before_frac_s, r
outreg2 using "covid_remotework_apr5_control.doc", append label ctitle("I continue to commute to work")

reg laid_off_frac_s lncaseper100k wfh_before_frac_s, r
outreg2 using "covid_remotework_apr5_control.doc", append label ctitle("I have recently been furloughed or laid-off")


reg switched_to_remote_frac_s lncaseper100k, r

outreg2 using "covid_remotework_apr5.tex", replace label ctitle("Used to commute, now work from home")

reg still_commute_frac_s lncaseper100k, r
outreg2 using "covid_remotework_apr5.tex", append label ctitle("I continue to commute to work")

reg laid_off_frac_s lncaseper100k, r
outreg2 using "covid_remotework_apr5.tex", append label ctitle("I have recently been furloughed or laid-off")

scatter laid_off_frac_s lncaseper100k

* Merging with information about remoteness of work
