set more off
clear

global MY_PATH "s:\\PROJEKTE\\Epo_micro_data"
global DATA_PATH "$MY_PATH\\data"
global DO_PATH "$MY_PATH\\do"

timer clear 6
timer on 6

* all citing applications ("citing_" prefix) with it's cited applications (if exist)
//------------------------------------------------------------------------------

#delimit ;

odbc load, exec("select distinct 
					a.docdb_family_id as citing_docdb_family_id,
					a.appln_id as citing_appln_id,
					a.appln_filing_date as citing_appln_filing_date,
					a2.docdb_family_id,
					a2.appln_id,
					a2.appln_filing_date,
					a2.nb_citing_docdb_fam
				from tls201_appln as a
					inner join tls211_pat_publn as p on a.appln_id = p.appln_id
					left join tls212_citation as c on c.pat_publn_id = p.pat_publn_id
					left join tls211_pat_publn as p2 on c.cited_pat_publn_id = p2.pat_publn_id
					left join tls201_appln as a2 on p2.appln_id = a2.appln_id")
																									  
dsn("patstat2019b") clear ;

#delimit cr

timer off 6
quietly timer list 6
display  
display "Total Time: " %3.2f r(t6) " sec. / " %3.2f r(t6)/60 " min. / " %3.2f r(t6)/3600 " hours"


export delimited "$DATA_PATH\\patent_citations.csv", replace