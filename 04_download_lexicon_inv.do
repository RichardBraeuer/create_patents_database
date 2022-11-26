set more off
clear

global MY_PATH "s:\\PROJEKTE\\Epo_micro_data"
global DATA_PATH "$MY_PATH\\data"
global DO_PATH "$MY_PATH\\do"

timer clear 4
timer on 4

* lexicon inventor information
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
					  where pa.invt_seq_nr > 0) as t
					inner join [EEE-PPAT_2019b] as e on e.person_id = t.person_id")
																							  
dsn("patstat2019b") clear ;

#delimit cr

timer off 4
quietly timer list 4
display  
display "Total Time: " %3.2f r(t4) " sec. / " %3.2f r(t4)/60 " min. / " %3.2f r(t4)/3600 " hours"


rename person_id inv_person_id
rename person_name inv_name
rename person_address inv_address
rename nuts inv_nuts
rename person_ctry_code inv_ctry
rename han_id inv_han_id
rename hrm_l2_id inv_eee_hrm_id
rename sector inv_eee_sector


export delimited "$DATA_PATH\\lexicon_inv.csv", replace