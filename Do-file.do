//clear all
set scheme s1color

//set up time variables
tostring date, generate(Date)

gen t = date
lab var t "date"
gen month = month(t)
gen year = year(t)
gen day = day(t)

//Create reg_id
encode name_region, gen(reg_id)

tsset reg_id t, daily

sort reg_id t

// Generate the variable trav_gath:
gen trav_gath = (travl_ban_intl_in + no_gathering)/2

//Lag the variables
foreach var in school_closure business_closure trav_gath work_from_home travel_ban_local facial_masks avg_temp {
	g `var'_copy = `var'
	g `var'_lag = L6.`var'
	replace `var'_lag = 0 if `var'_lag == .
		
}

//Main regression:	
reghdfe D_log_confirmed_cases school_closure_lag business_closure_lag trav_gath_lag work_from_home_lag travel_ban_local_lag facial_masks_lag avg_temp_lag test_reg_*, absorb(i.reg_id i.dow, savefe) cluster(t) resid

//Outreg results from regression:
cd "C:\Users\Jens\Documents\Kandidat\2. semester\Covid\Output" 
outreg2 using mainreg_result, sideway noparen nodepvar word replace label ///
title(Figure 2, "Daily growth rate of cumulative confirmed cases") ///
stats(coef se pval) dec(3) ctitle("Coefficient"; "Std Error"; "P-value") nocons nonotes addnote("*** p<0.01, ** p<0.05, * p<0.1" "") ///

cap erase "results/tables/reg_results/IRN_estimates_table.txt"

// saving coefs -> DEN HER SKAL DU LIGE OPDATERE; NÅR DU ÆNDRER PÅ ANTALLET AF VARIABLE
tempfile results_file
postfile results str18 adm0 str50 policy beta se using `results_file', replace
foreach var in "school_closure_lag" "business_closure_lag" "trav_gath_lag" "work_from_home_lag" "travel_ban_local_lag" "facial_masks_lag"{
	post results ("DKK") ("`var'") (round(_b[`var'], 0.001)) (round(_se[`var'], 0.001)) 
 }

// Predict the actual development in case of restrictions:
predictnl y_actual = xb() + __hdfe1__ + __hdfe2__ if e(sample), ci(lb_y_actual ub_y_actual) 


// estimating magnitude of treatment effects for each obs:
gen treatment = ///
school_closure_lag * _b[school_closure_lag] + ///
business_closure_lag * _b[business_closure_lag] + ///
trav_gath_lag * _b[trav_gath_lag] + ///
work_from_home_lag * _b[work_from_home_lag] + ///
travel_ban_local_lag * _b[travel_ban_local_lag] + ///
facial_masks_lag * _b[facial_masks_lag] ///
if e(sample) 

// predicting counterfactual growth for each obs
predictnl y_counter = test_reg_12mar2020 * _b[test_reg_12mar2020] + test_reg_21apr2020 * _b[test_reg_21apr2020] + test_reg_12may2020 * _b[test_reg_12may2020] +  ///
_b[_cons] + __hdfe1__ + __hdfe2__ if e(sample), ci(lb_counter ub_counter) 

// Calculate the combined effect:
lincom school_closure_lag + business_closure_lag + trav_gath_lag + work_from_home_lag + travel_ban_local_lag + facial_masks_lag 
post results ("DKK") ("comb. policy") (round(r(estimate), 0.001)) (round(r(se), 0.001)) 

// No negative growth in cumulative cases: Replace with 0, if this is the case.
foreach var of varlist y_actual y_counter lb_y_actual ub_y_actual lb_counter ub_counter{
	replace `var' = 0 if `var'<0 & `var'!=.
}

//Save the growth rates
cd "C:\Users\Jens\Documents\Kandidat\2. semester\Covid\Output" 

outsheet name_region t y_actual lb_y_actual ub_y_actual y_counter lb_counter ub_counter ///
using y_actual.xls if e(sample), comma replace
