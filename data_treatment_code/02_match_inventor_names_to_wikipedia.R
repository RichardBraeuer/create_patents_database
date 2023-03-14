###############################################
#+++++++match top inventors wikipedia+++++++++#
###############################################

if (match_wikipedia_top_inventors =="YES"){
  if (rematch_wikipedia_top_inventors =="YES") {
    
    
    
    #---------------------------------
    #read in names from PATSTAT
    #---------------------------------
    names_observed_patstat <- fread(file=paste(path_to_raw_downloaded_data,"/lexicon_inv.csv",sep=""))
    names_observed_patstat      <- names_observed_patstat[names_observed_patstat[, .I[1], by = inv_eee_hrm_id]$V1]
    
    list_of_name_components <- extract_list_of_name_components(
      data_table_of_names= names_observed_patstat,
      id_variable = "inv_eee_hrm_id",
      variable_containing_names = "inv_name"
    )
    
    
    setkey(list_of_name_components,inv_eee_hrm_id)
    unique(names_observed_patstat[,list(inv_eee_hrm_id)])
    
    #---------------------------------
    #read in lists inventors from wiki
    #---------------------------------
    list_of_top_inventors <- fread(file=paste(path_to_raw_downloaded_data,"/data_preparation/wikipedia_list_top_inventors.csv",sep=""))[,1:5]
    setnames(list_of_top_inventors,1:5,c("inv_name","nr_of_patents", "country", "years", "topic"))
    
    
    library(readstata13)
    list_of_known_inventors <- read.dta13(file=paste(path_to_raw_downloaded_data,"/data_preparation/wikipedia_inventor_list.dta",sep=""),encoding = 'UTF-8')
    setnames(list_of_known_inventors,1:2,c("inv_name", "years"))     
    
    data_table_of_inventors_wiki <- rbind(list_of_top_inventors,list_of_known_inventors, fill=TRUE)
    
    
    data_table_of_inventors_wiki[,list(years)]
    
    data_table_of_inventors_wiki[,cleaned_years := extract_years(
      data_table_of_inventors_wiki, 
      "inv_name", 
      "years"
    )
    ]
    
    data_table_of_inventors_wiki <- data_table_of_inventors_wiki[, first_year :=(tstrsplit(cleaned_years, " ", fixed=TRUE, fill= "")[2]) ]
    data_table_of_inventors_wiki[nchar(first_year)==4,first_year_first_digits:=as.numeric(substring(first_year,1,2))]
    data_table_of_inventors_wiki[nchar(first_year)==3,first_year_first_digits:=as.numeric(substring(first_year,1,1))]
    data_table_of_inventors_wiki<- data_table_of_inventors_wiki[first_year_first_digits>=19]
    rm(list_of_top_inventors,list_of_known_inventors)
    
    
    
    name_components_wikipedia <- extract_list_of_name_components(
      data_table_of_names= data_table_of_inventors_wiki,
      id_variable = "inv_name",
      variable_containing_names = "inv_name"
    )
    
    name_components_wikipedia[,not_initial:=nchar(name_component)>1]
    name_components_wikipedia[,number_of_name_components_wiki:=.N,by="inv_name"]
    setkey(name_components_wikipedia,name_component)
    
    #----------------------------------------------
    #Prune the list of name_components from PATSTAT
    #----------------------------------------------
    
    
    # sort list of name components to easily access name_components
    setkey(list_of_name_components,name_component) 
    
    #get all ids that have at least one name_component from the list
    actually_necessary_inv_ids <- unique(
      list_of_name_components[list(unique(name_components_wikipedia[,name_component]))][is.na(inv_eee_hrm_id)==0,inv_eee_hrm_id]
    )
    
    
    #sort data again, to easily access IDs
    setkey(list_of_name_components,inv_eee_hrm_id)
    
    #eliminate all id that do not have a single name component in common with the merge-list
    list_of_name_components_for_match <- list_of_name_components[list(actually_necessary_inv_ids),]
    list_of_name_components_for_match[,number_of_name_components_patstat:=.N, by="inv_eee_hrm_id"]
    
    # sort list of name components to easily access name_components
    setkey(list_of_name_components_for_match,name_component) 
    
    
    #---------------
    #Actual matching
    #---------------
    
    
    
    matched_name_components <- unique(merge(
      list_of_name_components_for_match[nchar(name_component)>1],
      name_components_wikipedia[nchar(name_component)>1],
      by="name_component",
      allow.cartesian = TRUE)
    )
    
    
    nrow(matched_name_components) 
    
    
    already_matched_name_components <-  matched_name_components[,list(inv_eee_hrm_id  , name_component, match_status = 1)]
    unmatched_name_components <- merge(list_of_name_components_for_match,already_matched_name_components, by=c("inv_eee_hrm_id" , "name_component" ),all.x=TRUE)
    unmatched_name_components <- unmatched_name_components[is.na(match_status)==1]
    
    
    matched_initials <-  unique(merge(
      unmatched_name_components[,initials_of_name_component := substring(name_component,1,1)],
      name_components_wikipedia[nchar(name_component)==1],
      by.x="initials_of_name_component",
      by.y="name_component",
      allow.cartesian = TRUE)
    )
    
    matched_initials$match_status <- NULL  
    data_matched_by_names<-rbindlist(list(matched_name_components,
                                          matched_initials),
                                     use.names = TRUE,
                                     fill = TRUE)
    
    
    nrow(data_matched_by_names) - nrow(matched_initials) 
    
    data_matched_by_names <- merge(names_observed_patstat[,list(inv_eee_hrm_id,inv_name_in_patstat=inv_name)],
                                   data_matched_by_names,
                                   by="inv_eee_hrm_id"
    )
    setkey(data_matched_by_names,inv_eee_hrm_id,inv_name)
    
    rm(matched_name_components , matched_initials , names_observed_patstat ,  list_of_name_components_for_match, list_of_name_components )
    gc()
    data_matched_by_names[,Number_of_matched_non_initials:=sum(not_initial),by=c("inv_eee_hrm_id","inv_name")][,
                                                                                                               Number_of_matched_initials:=.N - Number_of_matched_non_initials,by=c("inv_eee_hrm_id","inv_name")]
    
    
    data_matched_by_names[not_initial==FALSE]
    
    
    
    matched_sample <- unique(
      data_matched_by_names[ Number_of_matched_non_initials+Number_of_matched_initials==number_of_name_components_wiki & Number_of_matched_non_initials>=2][,
                                                                                                                                                            list(inv_eee_hrm_id,inv_name,inv_name_in_patstat,number_of_name_components_wiki, number_of_name_components_patstat, Number_of_matched_non_initials,Number_of_matched_initials)]
    )
    
    matched_sample[Number_of_matched_initials==1]
    matched_sample[Number_of_matched_initials!=0]
    
    setkey(matched_sample,inv_name,inv_name_in_patstat,Number_of_matched_non_initials)
    
    
    
    fwrite(matched_sample,   file = paste(path_to_raw_downloaded_data,"/data_preparation/matching_results_automatic_top_inventors.csv",sep=""))
    #library("xlsx")
    #write.xlsx(matched_sample, paste(path_to_raw_downloaded_data,"/data_preparation/matching_results_automatic_top_inventors.xlsx",sep="") ) 
    rm(matched_sample, already_matched_name_components, data_matched_by_names, data_table_of_inventors_wiki, name_components_wikipedia, unmatched_name_components)
    gc()
    #---------------------
    #Close chapter rematch
    #--------------------
  }
  
  
  
  # Read in the data on which wikipedia names have been found after manually correcting it
  #---------------------------------------------------------------------------------------
  matches_of_top_inventors_manually_checked <- read.csv(file= paste(path_to_raw_downloaded_data,"/data_preparation/matching_results_manual_top_inventors.csv",sep=""), sep = ";" , header = TRUE) 
  matches_of_top_inventors_manually_checked <- as.data.table(matches_of_top_inventors_manually_checked)[,list(inv_eee_hrm_id,inv_name_wiki=paste(" ", inv_name," ", sep=""), Problem)]
  matches_of_top_inventors_manually_checked[,inv_name_wiki := gsub(pattern="  ", replacement=" ", x=inv_name_wiki)]
  matches_of_top_inventors_manually_checked[,inv_name_wiki := gsub(pattern="  ", replacement=" ", x=inv_name_wiki)]
  matches_of_top_inventors_manually_checked <- unique(matches_of_top_inventors_manually_checked)
  
  matches_of_top_inventors_manually_checked[inv_name_wiki==" Shunpei Yamazaki " |  inv_name_wiki==" Shunpei Yamazaki "]
  
  
  # Read in the data from PATSTAT
  #-----------------------------
  
  #read in names lexikon
  names_observed_patstat <- fread(file=paste(path_to_raw_downloaded_data,"/lexicon_inv.csv",sep=""))
  
  
  names_observed_patstat[inv_eee_hrm_id == 1887234]
  
  # Match both
  #-----------
  
  top_inventors_manually_checked_patent_data <-        merge(matches_of_top_inventors_manually_checked, 
                                                             names_observed_patstat, 
                                                             by = "inv_eee_hrm_id", 
                                                             all.x = TRUE) 
  
  top_inventors_manually_checked_patent_data[inv_name_wiki=="Shunpei Yamazaki"]
  
  
  nrow(top_inventors_manually_checked_patent_data[is.na(inv_eee_hrm_id)==1])
  length(unique(top_inventors_manually_checked_patent_data[is.na(inv_eee_hrm_id)==0 & Problem != 1,inv_eee_hrm_id]))
  length(unique(top_inventors_manually_checked_patent_data[is.na(inv_eee_hrm_id)==0 & Problem != 1,inv_name_wiki]))
  
  rm(names_observed_patstat,matches_of_top_inventors_manually_checked)
  
  top_inventors_manually_checked_patent_data[inv_name_wiki==" Shunpei Yamazaki " |  inv_name_wiki==" Shunpei Yamazaki "]
  
  
  
  # Load patent data of best inventors
  #-----------------------------------
  
  observed_inventor_patents <- unique(fread(file = paste(path_to_raw_downloaded_data, "/", "patents_inv",".csv", sep=""))[,list(inv_eee_hrm_id, docdb_family_id)])[
    ,list(nr_patent_families=.N),by=c("inv_eee_hrm_id")][
      order(nr_patent_families)]
  
  evaluation_magerman_2006 <- merge(top_inventors_manually_checked_patent_data[is.na(inv_eee_hrm_id)==0 & Problem != 1,,],
                                    observed_inventor_patents,
                                    by = "inv_eee_hrm_id",
                                    all.x = TRUE) 
  
  evaluation_magerman_2006[inv_name_wiki == "Shunpei Yamazaki "]
  
  test <- evaluation_magerman_2006[,list(max_patent_families = max(nr_patent_families)),by=c("inv_name_wiki")]
  test <- unique(evaluation_magerman_2006[,list(inv_name_wiki, inv_eee_hrm_id, nr_patent_families)])[,list(sum_patent_families = sum(nr_patent_families)),by=c("inv_name_wiki")]
  
  
  
  evaluation_magerman_2006 <- unique(merge (evaluation_magerman_2006[,list(max_patent_families = max(nr_patent_families)),by=c("inv_name_wiki")],
                                            unique(evaluation_magerman_2006[,list(inv_name_wiki, inv_eee_hrm_id, nr_patent_families)])[,list(sum_patent_families = sum(nr_patent_families)),by=c("inv_name_wiki")],
                                            by="inv_name_wiki") )[order(sum_patent_families)][,percentage_hit_normally := max_patent_families/sum_patent_families]
  
  var(evaluation_magerman_2006[ max_patent_families >= 50 | sum_patent_families >= 50 ,percentage_hit_normally])
  mean(evaluation_magerman_2006[max_patent_families >= 50 | sum_patent_families >= 50 ,percentage_hit_normally])
  
  hist(unique(evaluation_magerman_2006)[,percentage_hit_normally] )
  print(paste(evaluation_magerman_2006[409,inv_name_wiki], evaluation_magerman_2006[410,inv_name_wiki]))
  view_potentially_invalid_names <- merge(observed_inventor_patents[nr_patent_families>= 300],top_inventors_manually_checked_patent_data[,list(inv_eee_hrm_id,inv_name_wiki)], by = "inv_eee_hrm_id", all.x = TRUE)
  view_potentially_invalid_names <- merge(view_potentially_invalid_names, fread(file=paste(path_to_raw_downloaded_data,"/lexicon_inv.csv",sep="")), by = "inv_eee_hrm_id", all.x = TRUE)
  
  fwrite(evaluation_magerman_2006, file=paste(path_to_raw_downloaded_data, "/data_preparation/", "evaluation_magerman_2006_indentifiers_top_inventors",".csv", sep=""))
  fwrite(top_inventors_manually_checked_patent_data, file=paste(path_to_raw_downloaded_data, "/data_preparation/", "top_inventors_manually_checked_with_PATSTAT_data",".csv", sep=""))
  
}
