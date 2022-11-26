set more off
clear

global MY_PATH "s:\\PROJEKTE\\Epo_micro_data"
global DATA_PATH "$MY_PATH\\data"
global DO_PATH "$MY_PATH\\do"

timer clear 1
timer on 1

* all patent applications with applicant information
//------------------------------------------------------------------------------

#delimit ;

odbc load, exec("select distinct
					a.appln_id,
					a.appln_nr_epodoc,
					a.appln_auth,
					a.appln_filing_year,
					a.docdb_family_id,
					p.person_id,
					p.han_id,
					e.hrm_l2_id,
					p.person_ctry_code
				from tls201_appln as a
					inner join tls207_pers_appln as pa on a.appln_id = pa.appln_id
					inner join tls206_person as p on pa.person_id = p.person_id
					left join [EEE-PPAT_2019b] as e on p.person_id = e.person_id
				where pa.applt_seq_nr > 0")
																							  
dsn("patstat2019b") clear ;

#delimit cr

timer off 1
quietly timer list 1
display  
display "Total Time: " %3.2f r(t1) " sec. / " %3.2f r(t1)/60 " min. / " %3.2f r(t1)/3600 " hours"

rename person_id apl_person_id
rename han_id apl_han_id
rename hrm_l2_id apl_eee_hrm_id
rename person_ctry_code apl_ctry


export delimited "$DATA_PATH\\patents_apl.csv", replace