
location <- "IWH"
#Path to code base
#---#---#---#---#---#---#
if (location == "IWH"){
  syntax_path <- "s:/PROJEKTE/Epo_micro_data/do/"
}
if (location == "chicago_server"){
  syntax_path <- "/share/akcigitusptolab/code/richard/prepare EPO data/"
}

#options and launch for chapter 01_localize_entries
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
source(paste0(syntax_path,"/data_treatment_code/00b_data_treatment_setup.R"))
redo_chunks <- FALSE
redo_localizations <- FALSE
use_old_resuts <-FALSE
source(paste0(syntax_path,"/data_treatment_code/01_localize_entries.R"))


#options and launch for chapter 02_match_inventor_names_to_wikipedia
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
source(paste0(syntax_path,"/data_treatment_code/00b_data_treatment_setup.R"))
match_wikipedia_top_inventors <- "no"
rematch_wikipedia_top_inventors <- "no"
source(paste0(syntax_path,"/data_treatment_code/02_match_inventor_names_to_wikipedia.R"))


#options and launch for chapter 03_split_names_into_words_and_clean
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
source(paste0(syntax_path,"/data_treatment_code/00b_data_treatment_setup.R"))
create_list_of_names <- "no"
source(paste0(syntax_path,"/data_treatment_code/03_split_names_into_words_and_clean.R"))


#options and launch for chapter 04_prepare_for_PatentsView.R
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
source(paste0(syntax_path,"/data_treatment_code/00b_data_treatment_setup.R"))
source(paste0(syntax_path,"/data_treatment_code/04_prepare_for_PatentsView.R"))


#options and launch for chapter 05_find_ipc_class_communities.R
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
source(paste0(syntax_path,"/data_treatment_code/00b_data_treatment_setup.R"))
find_ipc_class_communites="YES"
node_variable <- "ipc_class_symbol_4"
source(paste0(syntax_path,"/data_treatment_code/05_find_ipc_class_communities.R"))

#options and launch for chapter 06_create_communities_for_ids.R
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
source(paste0(syntax_path,"/data_treatment_code/00b_data_treatment_setup.R"))
inventor_statistics<-"YES"
node_variable <- "ipc_class_symbol_4"
source(paste0(syntax_path,"/data_treatment_code/06_create_communities_for_ids.R"))

#options and launch for chapter 07_describe_original_patstat_data.R
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
source(paste0(syntax_path,"/data_treatment_code/00b_data_treatment_setup.R"))
describe_patent_data="YES"
source(paste0(syntax_path,"/data_treatment_code/07_describe_original_patstat_data.R"))

#options and launch for chapter 08_create_data_for_analysis.R
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
source(paste0(syntax_path,"/data_treatment_code/00b_data_treatment_setup.R"))
link_to_registry<-TRUE
source(paste0(syntax_path,"/data_treatment_code/08a_record_link_apl_gdr_firm_reg.R"))

source(paste0(syntax_path,"/data_treatment_code/00b_data_treatment_setup.R"))
link_to_registry<-TRUE
source(paste0(syntax_path,"/data_treatment_code/08b_record_link_apl_tha_firms.R"))


#options and launch for chapter 09_create_data_for_analysis.R
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
source(paste0(syntax_path,"/data_treatment_code/00b_data_treatment_setup.R"))
create_final_data<-TRUE
source(paste0(syntax_path,"/data_treatment_code/09_create_data_for_analysis.R"))

#options and launch for chapter 09a_create_smaller_data_sets.R
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
source(paste0(syntax_path,"/data_treatment_code/00b_data_treatment_setup.R"))
create_final_data<-TRUE
version_description <- "teams_Q2_2023"
source(paste0(syntax_path,"/data_treatment_code/09a_create_smaller_data_sets.R"))
## 
# library(parallel)
# library(foreach)
# cl <- parallel::makeCluster(2)
# doParallel::registerDoParallel(cl)
# system.time(
#   foreach(i = seq_len(10)) %do% {
#     sqrt(i)
#   }
# )
# 
# library("future.apply")
# plan(sequential)
# plan(multisession,workers=20)
# future_lapply(X=1:1000,FUN=sqrt)
