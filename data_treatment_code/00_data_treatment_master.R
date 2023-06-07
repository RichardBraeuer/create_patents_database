


rm(list=setdiff(ls(), "syntax_path"))
gc()

location <- "chicago_server"

set.seed(45678)


library("data.table")

#Path to the data base
#---#---#---#---#---#---#
if (location == "chicago_server"){
  path_to_raw_downloaded_data <- "/share/akcigitusptolab/data/epo/original data/"
  path_to_output_data <- "/share/akcigitusptolab/data/epo/"
}
if (location == "IWH"){
  path_to_raw_downloaded_data <- "s:/PROJEKTE/Epo_micro_data/data/"
  path_to_output_data <- "s:/PROJEKTE/Epo_micro_data/data/"
}
  
#Path to supporting info from THA
#---#---#---#---#---#---#---#---#
if (location == "IWH"){
  path_tha_project_data <- "s:/PROJEKTE/Treuhand/data//"
}

#Path to code base
#---#---#---#---#---#---#
if (location == "IWH"){
  syntax_path <- "s:/PROJEKTE/Epo_micro_data/do/"
  syntax_path_tools = "s:/PROJEKTE/Treuhand/do/tools/"
  path_tools_data ="s:/PROJEKTE/Treuhand/data/temp/tools/"
  source(paste(syntax_path_tools,"/localities_data/functions_for_localization.R",sep=""))
  source(paste(syntax_path_tools,"/name_matching/name_matching_functions.R",sep=""))
}
if (location == "chicago_server"){
  syntax_path <- "/share/akcigitusptolab/code/richard/prepare EPO data/"
  syntax_path_tools = "/share/akcigitusptolab/code/richard/tools/"
  path_tools_data ="/share/akcigitusptolab/data/tools/"
  source(paste(syntax_path_tools,"/localities_data/functions_for_localization.R",sep=""))
  source(paste(syntax_path_tools,"/name_matching/name_matching_functions.R",sep=""))
}

#name of current data
#---#---#---#---#---#---#
  #this is to distinguish several slices of the EPO 
  #data used for different projects. If left as ""
  #the entire data is used
  
data_name <-""  #"inv_matching_relevant_data"
data_name_short <-""  #"inv_matching"
id_variable_to_use <- "inventor_id"
version_of_localizations <- "2023_01_05"


#inv matching country groups
#---#---#---#---#---#---#---#---#
countries_inv_matching_data <- data.table(
  
  appln_auth = c('FR', 'ES', 'GB', 'IT', 'PT',
                 'SE', 'NL', 'BE', 'DK', 'GR',
                 'DE', 'LU', 'US', 'JP',
                 'EE', 'LT', 'LV', 'PL', 'SK',
                 'SI', 'HU', 'CZ', 'RU','UA', 'KR',
                 'TW', 'EP', 'IE', 'FI', 'ZA'),
  country_name = c("France","Spain","Great Britain","Italy","Portugal",
                   "Schweden","Netherlands","Belgium","Denmark","Greece",
                   "Germany","Luxembourg","United States","Japan",
                   "Estonia","Lithuania","Latvia","Poland","Slovakia",
                   "Slovenia","Hungary","Czech Republic","Russia","Ukraine","South Korea",
                   "Taiwan","Europe","Ireland","Finland","South Africa"),
  country_group=c("Europe","Europe","Europe","Europe","Europe",
                  "Europe","Europe","Europe","Europe","Europe",
                  "Europe","Europe","USA","Japan",
                  "Former Communist Bloc","Former Communist Bloc","Former Communist Bloc","Former Communist Bloc","Former Communist Bloc",
                  "Former Communist Bloc","Former Communist Bloc","Former Communist Bloc","Former Communist Bloc","Former Communist Bloc","Korea & Taiwan",
                  "Korea & Taiwan","Europe","Europe","Europe","South Africa")
  
)


#options and launch for chapter 01_localize_entries
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
redo_chunks <- FALSE
redo_localizations <- FALSE
use_old_resuts <-FALSE
source(paste0(syntax_path,"/data_treatment_code/01_localize_entries.R"))


#options and launch for chapter 02_match_inventor_names_to_wikipedia
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
match_wikipedia_top_inventors <- "no"
rematch_wikipedia_top_inventors <- "no"
source(paste0(syntax_path,"/data_treatment_code/02_match_inventor_names_to_wikipedia.R"))


#options and launch for chapter 03_split_names_into_words_and_clean
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
create_list_of_names <- "no"
source(paste0(syntax_path,"/data_treatment_code/03_split_names_into_words_and_clean.R"))


#options and launch for chapter 04_prepare_for_PatentsView.R
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
source(paste0(syntax_path,"/data_treatment_code/04_prepare_for_PatentsView.R"))


#options and launch for chapter 05_find_ipc_class_communities.R
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
find_ipc_class_communites="YES"
node_variable <- "ipc_class_symbol_4"
source(paste0(syntax_path,"/data_treatment_code/05_find_ipc_class_communities.R"))

#options and launch for chapter 06_create_communities_for_ids.R
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
inventor_statistics<-"YES"
node_variable <- "ipc_class_symbol_4"
source(paste0(syntax_path,"/data_treatment_code/06_create_communities_for_ids.R"))

#options and launch for chapter 07_describe_original_patstat_data.R
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
describe_patent_data="no"
source(paste0(syntax_path,"/data_treatment_code/07_describe_original_patstat_data.R"))

#options and launch for chapter 08_create_data_for_analysis.R
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
link_to_registry<-TRUE
source(paste0(syntax_path,"/data_treatment_code/08a_record_link_apl_gdr_firm_reg.R"))

#options and launch for chapter 08_create_data_for_analysis.R
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
create_final_data<-TRUE
source(paste0(syntax_path,"/data_treatment_code/09_create_data_for_analysis.R"))

#options and launch for chapter 08_create_data_for_analysis.R
#--#--#--#--#---#--#--#--#--#---#---#---#---#---#---#
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
