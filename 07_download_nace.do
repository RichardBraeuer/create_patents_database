set more off
clear

global MY_PATH "s:\\PROJEKTE\\Epo_micro_data"
global DATA_PATH "$MY_PATH\\data"
global DO_PATH "$MY_PATH\\do"

timer clear 7
timer on 7

* IPC-Classes from all appln_ids within patent family
//------------------------------------------------------------------------------

#delimit ;

odbc load, exec("select *
				from tls229_appln_nace2")
																							  
dsn("patstat2019b") clear ;

#delimit cr

timer off 7
quietly timer list 7
display  
display "Total Time: " %3.2f r(t7) " sec. / " %3.2f r(t7)/60 " min. / " %3.2f r(t7)/3600 " hours"

export delimited "$DATA_PATH\\nace2.csv", replace