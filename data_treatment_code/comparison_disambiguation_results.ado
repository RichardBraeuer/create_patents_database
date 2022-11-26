
capture program drop find_id_similarities
program find_id_similarities
syntax varlist [if] [in] [, title(string)]

display `"`varlist'"'
local count = 0
foreach var in `varlist'{
	local count = `count' + 1
	local var`count' = "hlp_`var'"
	display "`var`count''"
	egen hlp_`var' = group(`var')
}


preserve
gen keepvar = 1
foreach var of varlist hlp_* {
replace keepvar= 0 if `var'==.
}

collapse (max) hlp_* if keepvar ==1
foreach var of varlist hlp_* {
replace `var'= `var'/1000000
}

ds hlp_*
graph bar `r(varlist)' , bargap(150)  //legend(label(1 "original data") label( 2 "John's Result") label(3 "John's code adjusted") label(4 "Lai's code adjusted"))
graph export "${dir_for_analysis_outcomes}/`title' ids_different_alogorithms.png", replace


restore

forvalues var_count1 = 1/`count' {
	forvalues var_count2 = 1/`count' {
		if "`var`var_count1''" != "`var`var_count2''" {
		display `"variable 1: `var`var_count1''; variable 2: `var`var_count2''"'
		
		sort `var`var_count1'' `var`var_count2''
	
		by `var`var_count1'' `var`var_count2'': gen count_distinct_values_v2 = _n 									if `var`var_count1''!= . & `var`var_count2'' != .
		by `var`var_count1'' : replace count_distinct_values_v2=count_distinct_values_v2==1							if `var`var_count1''!= . & `var`var_count2'' != .
		by `var`var_count1'' : egen dis_`var_count1'_`var_count2'=total(count_distinct_values_v2)					if `var`var_count1''!= . & `var`var_count2'' != .

		drop count_distinct_values_v2
		
		
		
		sort dis_`var_count1'_`var_count2' `variable_1'
		cumul dis_`var_count1'_`var_count2', gen(cumulative_distribution)

		graph twoway (line cumulative_distribution dis_`var_count1'_`var_count2') if dis_`var_count1'_`var_count2' < 20 
		graph export "${dir_for_analysis_outcomes}/`title' cdf_dis_`var`var_count1''_`var`var_count2''_og_id_weighted.png", replace
		drop cumulative_distribution
		
		preserve 
		keep `var`var_count1'' dis_`var_count1'_`var_count2'
		duplicates drop
		
		sort dis_`var_count1'_`var_count2' `variable_1'
		cumul dis_`var_count1'_`var_count2', gen(cumulative_distribution)

		graph twoway (line cumulative_distribution dis_`var_count1'_`var_count2') if dis_`var_count1'_`var_count2' < 20 
		graph export "${dir_for_analysis_outcomes}/`title' cdf_dis_`var`var_count1''_`var`var_count2''.png", replace
		drop cumulative_distribution
		restore
		
		
		}
	
	
	}
}

end

