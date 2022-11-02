log using "Replication_Assignment.log",replace
*Running master file that creates global variables 
global path="C:\MA Economics\Data Analysis_STATA\Replication_Assignment\Material\data\new_data"

global Data="$path/Data"
global Results="$path/Results"
global Tables="$Results/Tables"
global Figures="$Results/Figures"
global Do="$path/Do"

global pilotstart=date("1April2012","DMY")
global recent = date("1August2012","DMY")
global ObservationPeriod = date("1August2011","DMY")
global announcement = date("15June2012","DMY")
global cpsmslaunch = date("8September2012","DMY")
global cpsmsend = date("1April2013","DMY")
global cpsmsempty = date("18September2012","DMY")
global strike1start = date("1January2012","DMY")
global strike1end = date("1February2012","DMY")
global strike2start = date("15December2012","DMY")
global strike2end = date("28Jan2013","DMY")
global cpsmsfill = date("11December2012","DMY")
global treatpilotlaunch = date("31Aug2012","DMY")
global start=date("1Sept2012","DMY")
global end=date("1April2013","DMY")
global startofdata= date("1July2011","DMY")
global endofdata= date("1Apr2014","DMY")
global InterventionStart=date("1Sept2012", "DMY")
global InterventionEnd=date("1Apr2013", "DMY")

global HHControls="HHHindu HHCaste* HHHouseMud  HHLandHolding HHHeadMale HHHeadLiterate HHSize HHNbAdults HHControlsMissing"
global MukhiyaControls="i.MukhiyaReligion i.MukhiyaCaste MukhiyaFemale MukhiyaEdu* MukhiyaAge* MukhiyaFamily2001 MukhiyaFamily2006"
global EmployeeControls="C_* _IDes*"
global DistrictControls="PCA_* MIS_*"  
global PanchayatControls="NREGA_exp_labor_2011"



******************************************************************

*setiing up working directory
cd"C:\MA Economics\Data Analysis_STATA\Replication_Assignment\Material\data\Data"
******************************************************************



*ANSWER 3

*Creating Graph 1 
*Average daily expenditure by Gram Panchayat in Treatment and ControlGroup

use  "CPSMS Data by Date.dta", clear
drop if missing(Treatment)
collapse(mean) GPDailyDebitAmount GPDailyCreditAmount [weight=NumberGP], by(Treatment date) fast
global middle=date("15Dec2012", "DMY")+3
su GPDailyDebitAmount
global low=`r(max)'*.05

twoway (lowess GPDailyDebitAmount date if Treatment==0, bwidth(.05) lpattern(dash)) ///
(lowess GPDailyDebitAmount date if Treatment==1, bwidth(.05) xline( $cpsmslaunch $cpsmsend)  ///
	text($low $middle "INTERVENTION" "PERIOD", place(s) color(red) size(small)) graphregion(color(white)) bgcolor(white) ///
 yscale(range(0 10000)) xscale(range($startofdata $endofdata)) legend(label(1 "Control") label(2 "Treatment")) xtitle("") ///
 note("Source: CPSMS data on GP savings accounts.") graphregion(color(white)) bgcolor(white)   ytitle("GP Daily Debit Amount"))
graph export "Figure1 data on GP savings account.pdf", replace

*****************************************************************



*ANSWER 6

*Creating Table to show the change in expenditure by Gram Panchayat on both labour and material in the three fiscal years divided according to pre-reform, during reform and post reform period 
use "MIS Sample Data by Year.dta", clear

egen BlockId=group(DistrictName_off BlockName_off)
egen DistrictId=group(DistrictName_off)

global Table1="T3 Spending (nrega-nic-in) "

local counter 0
foreach outcome in  ExpLabour ExpMaterial {
foreach year in 2011 2012 2013{
local ++ counter

quietly sum `outcome' if Treatment==0 & Year==`year'
local ControlMean = r(mean)
di `ControlMean'
areg `outcome' Treatment if Year==`year', cluster(BlockId) absorb(DistrictId)
local Effect = _b[Treatment]
if `counter'==1 outreg2 using "Table1.tex",  replace  addtext(District FE, Yes)  keep(Treatment) nonotes noaster nocons ///
adds("Mean in Control", `ControlMean')
else outreg2 Treatment   using "Table1.tex", excel  addtext(District FE, Yes)  keep(Treatment) nonotes noaster nocons ///
adds("Mean in Control", `ControlMean')
	}
}


******************************************************************



*ANSWER 9

*Creating Table to show match of employees in the database of SECC and MGNREGS workers database using job card 

use "Job Card Match rate with census at Panchayat Level", clear

su count_hh_1p count_hh_2p
xi i.DistrictName_off
egen BlockId=group(DistrictName_off BlockName_off)


global Table2="T6 Treatment effect on Match Rate"


local counter=0
foreach var of varlist match_percent_hh_1p match_percent_hh_1p_period4 match_percent_hh_1p_period6  ///
 match_percent_hh_2p match_percent_hh_2p_period5 match_percent_hh_2p_period6 {
local ++counter
quietly sum `var' if Treatment==0   
local ControlMean = r(mean)
di `ControlMean'
reg  `var' Treatment _I*  , cluster(BlockId)
if `counter'==1 outreg2  using "Table2.tex", keep(Treatment) ///
replace  addtext(District FE, Yes, HH Controls, No, Weighted, No)  nonotes noaster nocons ///
adds("Mean in Control", `ControlMean')
else outreg2  using "Table2.tex",  keep(Treatment) ///
  addtext(District FE, Yes, HH Controls, No, Weighted, No)  nonotes noaster nocons ///
adds("Mean in Control", `ControlMean')
}
outreg2  using "Table2.tex", excel keep(Treatment) ///
  addtext(District FE, Yes, HH Controls, No, Weighted, No)  nonotes noaster nocons ///
adds("Mean in Control", `ControlMean')

*****************************************************************



*ANSWER 12

*Creating Graph in DiD 
use "EFMS for Analysis District", clear
xtset DistrictId Year
global outvar "*EFMS*"
global Weight ""
global controls= ""


egen StateId=group(StateNREGA)

replace EFMS_Labour=EFMSStartWage==Year 
replace EFMS_Material=EFMSStartMaterial==Year 

foreach item in Labour Material {
foreach time in  "F6." "F5." "F4." "F3." "F2." "L1." "L2." "L3." "L4." "L5." "L6."{
local name=subinstr("`time'",".","",.)
gen `name'EFMS_`item'=`time'EFMS_`item' 
replace `name'EFMS_`item'=0 if missing(`name'EFMS_`item')
}

display "Doing `time'"
 eststo : xtreg Exp`item' F?EFMS_`item' EFMS_`item' L?EFMS_`item'  _IYear* $Weight,fe vce(cluster StateId) 
 }


esttab using "Table with District lags and forwards.csv", replace /// 
noomitted nogap se nostar wide noparentheses nonumbers
eststo clear


insheet using "Table with District lags and forwards.csv", names clear comma


drop if inlist(v1, "_cons", "N", "Standard errors in second column")

rename v1 RHS 
keep if regexm(RHS,"EFMS")==1

gen Time=0 if RHS=="EFMS_Labour" | RHS=="EFMS_Material"
forvalues x=1/5{
replace Time=-`x' if RHS=="F`x'EFMS_Labour" | RHS=="F`x'EFMS_Material"
replace Time=`x' if RHS=="L`x'EFMS_Labour" | RHS=="L`x'EFMS_Material"
}

rename explabour coef_labor
rename v3 se_labor
rename expmaterial coef_material
rename v5 se_material

drop RHS

local new=_N+1
set obs `new'
replace Time=-1 in `new'
foreach var of varlist coef* se*{
replace `var'=0 in `new'
}

foreach item in labor material{
gen bound_10th_`item'=coef_`item'-1.64*se_`item'
gen bound_90th_`item'=coef_`item'+1.64*se_`item'
gen bound_5th_`item'=coef_`item'-1.96*se_`item'
gen bound_95th_`item'=coef_`item'+1.96*se_`item'
}

local counter="a"
foreach item in labor material{
graph twoway (scatter coef_`item' Time) || (rcap bound_5th_`item' bound_95th_`item' Time), xtitle("") xlabel(-4 -3 -2 -1 0 1 2 3 4 , ///
valuelabel labsize(small)) ytitle("Expenditures on `item'") xtitle("Year relative to e-FMS implementation") legend(off) ///
graphregion(color(white)) note("Estimated coefficients and 95% confidence intervals from a linear regression of MGNREGS" ///
 "expenditures (reported in nrega.nic.in) on a dummy equal to one for the year of e-FMS" ///
 "implementation, including five lags and four forwards. The red vertical line indicates" ///
 "the year of e-FMS implementation.") xline(-0.5)
graph export "$Figures/F4`counter'_DiD_EFMS_`item'.pdf", replace
local counter="b"

}
log close

