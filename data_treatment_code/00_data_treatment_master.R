library(beepr)
options(error = function() {beep(10)})

rm(list=setdiff(ls(), "syntax_path"))
gc()

set.seed(45678)


location = "IWH"

#Path to the data base
#---#---#---#---#---#---#
  #path_to_raw_downloaded_data <- "/share/akcigitusptolab/data/epo/original data/"
  #path_to_output_data <- "/share/akcigitusptolab/data/epo/"
  path_to_raw_downloaded_data <- "s:/PROJEKTE/Epo_micro_data/data/"
  path_to_output_data <- "s:/PROJEKTE/Epo_micro_data/data/data_preparation/"
  
#Path to supporting info from THA
#---#---#---#---#---#---#---#---#
  path_tha_project_data <- "s:/PROJEKTE/Treuhand/data//"

#Path to code base
#---#---#---#---#---#---#
  syntax_path <- "s:/PROJEKTE/Epo_micro_data/do/"
  syntax_path_tools = "s:/PROJEKTE/Treuhand/do/tools/"
  path_tools_data ="s:/PROJEKTE/Treuhand/data/temp/tools/"
  source(paste(syntax_path_tools,"/localities_data/functions_for_localization.R",sep=""))
  source(paste(syntax_path_tools,"/name_matching/name_matching_functions.R",sep=""))
  

#name of current data
#---#---#---#---#---#---#
  #this is to distinguish several slices of the EPO 
  #data used for different projects. If left as ""
  #the entire data is used
  
data_name <- "ussr_relevant_data"
data_name_short <- "ussr"
id_variable_to_use <- "inventor_id"

#options and launch for chapter 01_localize_entries
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
redo_chunks <- FALSE
redo_localizations <- FALSE
#source(paste0(syntax_path,"/data_treatment_code/01_localize_entries.R"))
#options and launch for chapter 01_localize_inventors
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
#source(paste0(syntax_path,"/data_treatment_code/02_match_inventor_names_to_wikipedia.R"))
#options and launch for chapter 01_localize_inventors
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
#source(paste0(syntax_path,"/data_treatment_code/03_split_names_into_words_and_clean.R"))
#options and launch for chapter 01_localize_inventors
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
#source(paste0(syntax_path,"/data_treatment_code/04_prepare_for_PatentsView.R"))
#options and launch for chapter 01_localize_inventors
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
find_ipc_class_communites="YES"
source(paste0(syntax_path,"/data_treatment_code/05_find_ipc_class_communities.R"))

#options and launch for chapter 06_create_communities_for_ids.R
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
inventor_statistics<-"YES"
source(paste0(syntax_path,"/data_treatment_code/06_create_communities_for_ids.R"))

#options and launch for chapter 07_describe_original_patstat_data.R
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
describe_patent_data="YES"
source(paste0(syntax_path,"/data_treatment_code/07_describe_original_patstat_data.R"))

#options and launch for chapter 08_create_data_for_analysis.R
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
link_to_registry<-TRUE
source(paste0(syntax_path,"/data_treatment_code/08a_record_link_apl_gdr_firm_reg.R"))

#options and launch for chapter 08_create_data_for_analysis.R
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
create_final_data<-TRUE
source(paste0(syntax_path,"/data_treatment_code/09_create_data_for_analysis.R"))
