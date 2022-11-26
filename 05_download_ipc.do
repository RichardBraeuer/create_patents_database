set more off
clear

global MY_PATH "s:\\PROJEKTE\\Epo_micro_data"
global DATA_PATH "$MY_PATH\\data"
global DO_PATH "$MY_PATH\\do"

timer clear 5
timer on 5

* IPC-Classes from all appln_ids within patent family
//------------------------------------------------------------------------------

#delimit ;

odbc load, exec("select distinct a.docdb_family_id,
								 a.appln_id,
								 i.ipc_class_symbol          
				 from tls201_appln as a
					inner join tls209_appln_ipc as i on a.appln_id = i.appln_id")
																							  
dsn("patstat2019b") clear ;

#delimit cr

timer off 5
quietly timer list 5
display  
display "Total Time: " %3.2f r(t5) " sec. / " %3.2f r(t5)/60 " min. / " %3.2f r(t5)/3600 " hours"

export delimited "$DATA_PATH\\ipc.csv", replace