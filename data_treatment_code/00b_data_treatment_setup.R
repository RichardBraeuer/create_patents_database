rm(list=setdiff(ls(), "location"))
gc()


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


