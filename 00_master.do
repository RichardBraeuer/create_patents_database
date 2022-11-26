set more off
clear

global MY_PATH "s:\\PROJEKTE\\Epo_micro_data"
global DATA_PATH "$MY_PATH\\data"
global DO_PATH "$MY_PATH\\do"

timer clear 10
timer on 10


log close _all
log using "$MY_PATH\\download.log", replace

do "$DO_PATH\\01_download_patents_apl.do"
do "$DO_PATH\\02_download_patents_inv.do"
do "$DO_PATH\\03_download_lexicon_apl.do"
do "$DO_PATH\\04_download_lexicon_inv.do"
do "$DO_PATH\\05_download_ipc.do"
do "$DO_PATH\\06_download_citations.do"
do "$DO_PATH\\07_download_nace.do"

timer off 10
quietly timer list 10
display  
display "Total Time: " %3.2f r(t10) " sec. / " %3.2f r(t10)/60 " min. / " %3.2f r(t10)/3600 " hours"

log close _all