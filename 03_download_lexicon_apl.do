set more off
clear

global MY_PATH "s:\\PROJEKTE\\Epo_micro_data"
global DATA_PATH "$MY_PATH\\data"
global DO_PATH "$MY_PATH\\do"

timer clear 3
timer on 3

* lexicon applicant information
//------------------------------------------------------------------------------

#delimit ;

odbc load, exec("select 
					t.person_id,
					t.person_name,
					t.person_address,
					t.nuts,
					t.person_ctry_code,
					t.han_id,
					e.hrm_l2_id,
					e.sector
				from (select distinct 
						p.person_id,
						p.person_name,
						p.person_address,
						p.nuts,
						p.person_ctry_code,
						p.han_id
					  from tls207_pers_appln as pa
						inner join tls206_person as p on pa.person_id = p.person_id
					  where pa.applt_seq_nr > 0) as t
					inner join [EEE-PPAT_2019b] as e on e.person_id = t.person_id")
																							  
dsn("patstat2019b") clear ;

#delimit cr

timer off 3
quietly timer list 3
display  
display "Total Time: " %3.2f r(t3) " sec. / " %3.2f r(t3)/60 " min. / " %3.2f r(t3)/3600 " hours"


rename person_id apl_person_id
rename person_name apl_name
rename person_address apl_address
rename nuts apl_nuts
rename person_ctry_code apl_ctry
rename han_id apl_han_id
rename hrm_l2_id apl_eee_hrm_id
rename sector apl_eee_sector


export delimited "$DATA_PATH\\lexicon_apl.csv", replace