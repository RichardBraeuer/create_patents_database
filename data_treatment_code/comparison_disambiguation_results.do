global test = "" //1:100

global name_data = "gdr_relevant_data"
//contains e.g. the file patents_inv.csv created by the download code
global dir_with_og_data = "s:/PROJEKTE/Epo_micro_data/data/"
//contains the disambiguation results from PatentsView, e.g. the subfolder "invenors"
global dir_with_PatentsView_results = "s:/PROJEKTE/Epo_micro_data/data/data_preparation/${name_data}/PatentsView disambiguation/"
//contains the report of the analysis 
global dir_for_analysis_outcomes = "s:/PROJEKTE/Epo_micro_data/data/data_preparation/${name_data}/PatentsView disambiguation/"
//contains the disambiguated assembled data set
global dir_for_data_outcomes = "s:/PROJEKTE/Epo_micro_data/data/data_preparation/${name_data}/"
//contains the syntax
global dir_for_syntax = "s:/PROJEKTE/Epo_micro_data/do/data_treatment_code/"

global designations_subresults = "results_gdr_liberal results_gdr_conservative"


do "${dir_for_syntax}/comparison_disambiguation_results.ado"


//-------------------------\\
//------assemble data set--\\
//-------------------------\\
{
foreach directory in dir_with_og_data dir_with_PatentsView_results dir_for_analysis_outcomes dir_for_data_outcomes {
	capture mkdir `directory'
}

foreach subresult in $designations_subresults {
display "`subresult'"
clear
import delimited  "${dir_with_PatentsView_results}/inventors/`subresult'/disambiguation.tsv" , rowrange($test) delimiter(tab)
capture tostring(v1), gen(og_id)
capture rename v1 og_id
rename v2 id_`subresult'
keep og_id id_`subresult'
drop if id_`subresult'=="" | og_id==""

bysort og_id: gen N = _N
bysort og_id: gen n = _n
count if N>1
if `r(N)' >0{
br if N>1
keep if n==1
}
drop n N
sort og_id id_`subresult'
save  "${dir_with_PatentsView_results}/inventors/`subresult'/disambiguation.dta", replace
}


clear
local count = 0
foreach subresult in $designations_subresults {
	if `count' == 0{
		use "${dir_with_PatentsView_results}/inventors/`subresult'/disambiguation.dta",
		local count = `count'+1
	}
	if `count' != 0{
		merge 1:1 og_id using "${dir_with_PatentsView_results}/inventors/`subresult'/disambiguation.dta", nogen
	}
	
}


export delimited "${dir_for_data_outcomes}/PatentsView_identifiers.csv", replace
}



//-------------------------\\
//-----actual analysis-----\\
//-------------------------\\
{
	
import delimited "${dir_for_data_outcomes}/PatentsView_identifiers.csv", clear


find_id_similarities id_results_gdr_liberal id_results_gdr_conservative, title(lib_vs_con)
}