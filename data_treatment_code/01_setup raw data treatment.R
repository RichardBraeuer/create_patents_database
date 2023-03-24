
library(data.table)
library(readstata13)
library(future)
library(future.apply)
library(plotly)
library(ggplot2)
library(ggridges)


options(error = function() {beep(10)})


set.seed(45678)


starting_quarter <- 9



#IWH paths
path = "s:/PROJEKTE/Treuhand/data/temp/gdr_patent_data/inventor project/fused data/"
#path = "s:/PROJEKTE/Compnet/Bartelsman Braeuer/Simulation/"

#RBR personal laptop path
#path = "D:/inventor_firm_empirics/Simulation/"
#path = "C:/inventor_firm_empirics/"

data_source <- paste(path ,"/data/treated_employer_employee_file.csv", sep="") 
#data_source_raw_data <- paste(path_temp_data_all,"gdr_patent_data/inventor project/fused data/data/Downloaded raw data.csv",sep="")
syntax_path_inventor_code = "s:/PROJEKTE/Inventor_Matching_PATSTAT/"


#define data handling tasks to be executed
######################################################


load_name_handling_functions <- "YES"

#02 wikipedia
match_wikipedia_top_inventors <- "YES"
rematch_wikipedia_top_inventors <- "YES"

#03 clean names
create_list_of_names <- "YES"

#04 describe patent data
describe_patent_data <- "YES"

#05 clean data
generate_cleaned_data <- "YES"
#06 create tech clusters
find_ipc_class_communites <- "YES"
#07 record_link words in names with each other (spelling mistakes)
include_record_linked_ids <- "YES"
actual_record_linkage <- "no"

#08 inventor statistics for ipc classes and clusters
create_inventor_ipc_classes <- "YES"
analyze_inventor_ipc_classes <- "YES"
create_inventor_communities <- "YES"
analyze_inventor_communities <- "YES"

#09 assemble final data set
unify_apl_and_inv_ids <- "YES"
patstat_unification <- "no"
create_core_data_set <- "YES"
enrich_unified_lexicon <- "YES"
select_finished_file <- "YES"


#Leave 2 cores for other applications, to prevent crash of server cluster
used_cores <- max(min(12, availableCores()  - 2 ),1)



###############################################################################################
path_to_save_results <- paste(path ,"/Results/", sep="")
path_to_save_data <- paste(path ,"/data/", sep="")


meta_data_on_time_periods <- data.table(
  
  quarter=c(1:11),
  start_year = c(1800,1914,1919,1929,1939,1946,1950,1961,1974,1989,2003),
  end_year = c(1913,1919,1929,1939,1945,1949,1960,1973,1988,2002,2019)  
  
)
setkey(meta_data_on_time_periods,quarter)


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




wt.sd <- function(vector_of_values,vector_of_weights,...){
  
  weighted_mean <- weighted.mean(vector_of_values,vector_of_weights)
  sum_weights <- (sum(vector_of_weights))
  if (sum_weights > 1.05){
    sum_weights <- sum_weights - 1
  }
    
    
  return(
    (sum((vector_of_weights/sum_weights) * (vector_of_values - weighted_mean)^2))^0.5
         )
  
  
}



name_technology_clusters <- function(data_table){
  data_table[community==12,technology_cluster:="Medical Technology"]
  data_table[community==7,technology_cluster:="Foodstuff"]
  data_table[community==4,technology_cluster:="Chemistry"]
  data_table[community==5,technology_cluster:="Fibers & Plastics"]
  data_table[community==18,technology_cluster:="Printing"]
  data_table[community==3,technology_cluster:="Combustion"]
  data_table[community==15,technology_cluster:="Machinery"]
  data_table[community==10,technology_cluster:="Measurement"]  
  data_table[community==16,technology_cluster:="Time Measurement"]
  data_table[community==11,technology_cluster:="Optics"]
  data_table[community==20,technology_cluster:="Misc."]
  data_table[community==1,technology_cluster:="Computing"]
  data_table[community==17,technology_cluster:="Electrical Motors & Sound Equip."]
  data_table[community==2,technology_cluster:="S.-conductors"]
  data_table[community=="all",technology_cluster:="all"]
}

###############################################################################################


if (load_name_handling_functions =="YES"){
  #Definition of all necessary functions for this chapter
  ###################    
  
  
  
  
  #Function to extract all words from a properly prepared data table of names
  #Is not standalone, is a subfunction of "extract_list_of_name_components"
  extract_name_components <- function(column_to_extract, data_table_of_function,id_variables) {
    #cbind groups the id variable and the name-component together
    result <- cbind(data_table_of_function[,id_variables,with=FALSE], #column of all ids, as in the original data set
                    data_table_of_function[, tstrsplit(treated_name, " ", fixed=TRUE, fill= "")[eval(column_to_extract)]  ]  # column of the "column_to_extract"-test word of every name
    )[V1 !="" & V1 !=" "]  #discard empty words
    result[,nr_word := column_to_extract]
    setnames(result,"V1","name_component") #name the new column containing words
    return(result)
  }
  
  ##Function to split a name according to ","
  #Is to substract adresses etc from the name
  extract_name_parts <- function(column_to_extract, data_table_of_function) {
    #cbind groups the id variable and the name-component together
    result <- cbind(data_table_of_function[,1], #column of all ids, as in the original data set
                    data_table_of_function[, tstrsplit(name, ",", fixed=TRUE, fill= "")[eval(column_to_extract)]  ]  # column of the "column_to_extract"-test word of every name
    )[V1 !="" & V1 !=" "]  #discard empty words
    setnames(result,"V1","name_component") #name the new column containing words
    return(result)
  }  
  
  extract_list_of_name_components <- function(data_table_of_names, id_variables, variable_containing_names) {
    #prepare a name for manual matching of names
    data_table_of_function <- copy(data_table_of_names[,c(id_variables,variable_containing_names),with=FALSE])
      
    data_table_of_function[,treated_name:=paste(" ", tolower(get(variable_containing_names)) , " ", sep = "")]
    
    data_table_of_function[,treated_name:=gsub(pattern="#", replacement = " ",treated_name , fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern="'", replacement = " ",treated_name , fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern="jr.", replacement = " ",treated_name , fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern="jr", replacement = " ",treated_name , fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern=".", replacement = " . ",treated_name , fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern=",", replacement = " , ",treated_name, fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern=";", replacement = " ", treated_name, fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern="+", replacement = " ", treated_name, fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern="(", replacement = " ", treated_name, fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern=")", replacement = " ", treated_name, fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern="&", replacement = " ", treated_name, fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern="[0-9]", replacement = " ", treated_name, fixed = FALSE)]
    #data_table_of_function[,treated_name:=gsub(pattern="\\b[a-z]\\W+", replacement = " ",treated_name,fixed = FALSE)]
    
    for (i in 1:10) {
      data_table_of_function[,treated_name:=gsub(pattern="  ", replacement = " ", treated_name, fixed = TRUE)]
      data_table_of_function[,treated_name:=gsub(pattern=", . ", replacement = ", ", treated_name, fixed = TRUE)]
      data_table_of_function[,treated_name:=gsub(pattern=", , ", replacement = ", ", treated_name, fixed = TRUE)]
      data_table_of_function[,treated_name:=gsub(pattern=". . ", replacement = ". ", treated_name, fixed = TRUE)]
      data_table_of_function[,treated_name:=gsub(pattern=". , ", replacement = ". ", treated_name, fixed = TRUE)]
    }
    
    for (i in 1:10) {
      data_table_of_function[,treated_name:=gsub(pattern="  ", replacement = " ", treated_name, fixed = TRUE)]
      data_table_of_function[,treated_name:=gsub(pattern=",", replacement = " ", treated_name, fixed = TRUE)]
      data_table_of_function[,treated_name:=gsub(pattern=".", replacement = " ", treated_name, fixed = TRUE)]
    }
    
    
    #count how many different words are in a name at maximum
    number_of_name_components <- data_table_of_function[, length(tstrsplit(treated_name, " ", fixed=TRUE, fill= ""))]
    
    
    #create a list of 
    return( 
      rbindlist(
        lapply(1:number_of_name_components,
               extract_name_components,
               data_table_of_function = data_table_of_function,
               id_variables=id_variables)
      )  
    )
    
  }
  
  extract_list_of_company_name_components <- function(data_table_of_names, id_variable, variable_containing_names) {
    #prepare a name for manual matching of names
    data_table_of_function <- data_table_of_names[,list(id_variable = get(id_variable), treated_name=paste(" ", tolower(get(variable_containing_names)) , " ", sep = ""))]
    setnames(data_table_of_function,"id_variable",eval(id_variable))
    
    data_table_of_function[,treated_name:=gsub(pattern="#", replacement = " ",treated_name , fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern="'", replacement = " ",treated_name , fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern="jr.", replacement = " ",treated_name , fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern="jr", replacement = " ",treated_name , fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern=",", replacement = " , ",treated_name, fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern="&", replacement = " & ",treated_name, fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern=";", replacement = " ", treated_name, fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern="+", replacement = " ", treated_name, fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern="(", replacement = " ", treated_name, fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern=")", replacement = " ", treated_name, fixed = TRUE)]
    
    for (i in 1:10) {
      data_table_of_function[,treated_name:=gsub(pattern="  ", replacement = " ", treated_name, fixed = TRUE)]
      data_table_of_function[,treated_name:=gsub(pattern=", . ", replacement = ", ", treated_name, fixed = TRUE)]
      data_table_of_function[,treated_name:=gsub(pattern=", , ", replacement = ", ", treated_name, fixed = TRUE)]
      data_table_of_function[,treated_name:=gsub(pattern=". . ", replacement = ". ", treated_name, fixed = TRUE)]
      data_table_of_function[,treated_name:=gsub(pattern=". , ", replacement = ". ", treated_name, fixed = TRUE)]
    }
    
    for (i in 1:10) {
      data_table_of_function[,treated_name:=gsub(pattern="  ", replacement = " ", treated_name, fixed = TRUE)]
      data_table_of_function[,treated_name:=gsub(pattern=",", replacement = " ", treated_name, fixed = TRUE)]
    }
    
    
    #count how many different words are in a name at maximum
    number_of_name_components <- data_table_of_function[, length(tstrsplit(treated_name, " ", fixed=TRUE, fill= ""))]
    
    
    #create a list of 
    return( 
      rbindlist(
        lapply(1:number_of_name_components,extract_name_components, data_table_of_function = data_table_of_function)
      )  
    )
    
  }
  
  
  # Function to clean numerical year-year entries
  extract_years <- function(data_table_of_years, id_variable, variable_containing_names) {
    #prepare a name for manual matching of names
    data_table_of_function <- data_table_of_years[,list(id_variable = get(id_variable), treated_name=paste(" ", get(variable_containing_names) , " ", sep = ""))]
    setnames(data_table_of_function,"id_variable",eval(id_variable))
    
    data_table_of_function[,treated_name:=gsub(pattern="-", replacement = " ",treated_name , fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern="-", replacement = " ",treated_name , fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern="#", replacement = " ",treated_name , fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern="'", replacement = " ",treated_name , fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern="jr.", replacement = " ",treated_name , fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern="jr", replacement = " ",treated_name , fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern=".", replacement = " . ",treated_name , fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern=",", replacement = " , ",treated_name, fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern=";", replacement = " ", treated_name, fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern="+", replacement = " ", treated_name, fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern="(", replacement = " ", treated_name, fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern=")", replacement = " ", treated_name, fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern="&", replacement = " ", treated_name, fixed = TRUE)]
    data_table_of_function[,treated_name:=gsub(pattern="[A-Z]", replacement = " ",treated_name,fixed = FALSE)]
    data_table_of_function[,treated_name:=gsub(pattern="[a-z]", replacement = " ",treated_name,fixed = FALSE)]
    
    for (i in 1:10) {
      data_table_of_function[,treated_name:=gsub(pattern="  ", replacement = " ", treated_name, fixed = TRUE)]
      data_table_of_function[,treated_name:=gsub(pattern=", . ", replacement = ", ", treated_name, fixed = TRUE)]
      data_table_of_function[,treated_name:=gsub(pattern=", , ", replacement = ", ", treated_name, fixed = TRUE)]
      data_table_of_function[,treated_name:=gsub(pattern=". . ", replacement = ". ", treated_name, fixed = TRUE)]
      data_table_of_function[,treated_name:=gsub(pattern=". , ", replacement = ". ", treated_name, fixed = TRUE)]
    }
    data_table_of_function[,treated_name:=gsub(pattern=" , $", replacement = " ",treated_name )]
    data_table_of_function[,treated_name:=gsub(pattern=" . $", replacement = " ",treated_name )]      
    
    for (i in 1:10) {
      data_table_of_function[,treated_name:=gsub(pattern="  ", replacement = " ", treated_name, fixed = TRUE)]
      data_table_of_function[,treated_name:=gsub(pattern=",", replacement = " ", treated_name, fixed = TRUE)]
      data_table_of_function[,treated_name:=gsub(pattern=".", replacement = " ", treated_name, fixed = TRUE)]
    }
    
    
    
    #create a list of 
    return( 
      data_table_of_function[,treated_name]
    )
    
  }
  
  
  
  ###################
}




#create folders
suppressWarnings(dir.create(paste(path,"/Results/" , sep = "")))
suppressWarnings(dir.create(paste(path,"/Results/treatment_raw_data/" , sep = "")))
suppressWarnings(dir.create(paste(path, "/data/", sep="")))
suppressWarnings(dir.create(paste(path,"/data/data_preparation/" , sep = "")))
suppressWarnings(dir.create(paste(path,"/data/data_preparation/" , sep = "")))
suppressWarnings(dir.create(paste(path,"/Results/treatment_raw_data/ipc_communities/" , sep = "")))