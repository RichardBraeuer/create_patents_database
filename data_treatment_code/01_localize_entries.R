


max_number_of_observation_chunk <- 10000
#numbr of observations to be localized in one go



for (prefix in c("inv"
                 ,
                 "apl"
                 )) {
  
  

long_name <- "inventor"
if (prefix=="apl"){
  long_name <- "applicant"
}


#housekeeping: creating the target folder and file
#---#---#---#---#---#---#---#---#---#---#---#---#
    suppressWarnings(dir.create(paste(path_to_raw_downloaded_data,"/data_preparation/localization_",long_name,"s/", sep = "")))
    if (file.exists(paste(path_to_raw_downloaded_data,"/data_preparation/localization_",long_name,"s/localization_data.csv", sep=""))==FALSE){
      
      old_results_empty <-     data.table(numeric(),numeric(),numeric(),numeric(),numeric())
      setnames(old_results_empty,
               old=c("V1","V2","V3","V4","V5"),
               new=c(eval(paste0(prefix,"_person_id")),"pair_city_region","dummy_ex_gdr","latitude","longitude"))
      fwrite(
        old_results_empty,
        paste(path_to_raw_downloaded_data,"/data_preparation/localization_",long_name,"s/localization_data.csv", sep=""))
    }
  
#read in the complete data set and result of previous attempts
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
  current_lexicon <- fread(file =paste(path_to_raw_downloaded_data,"lexicon_",prefix,".csv", sep=""),
        encoding="UTF-8")
  
  old_results <- merge(
    fread(paste(path_to_raw_downloaded_data,"/data_preparation/localization_",long_name,"s/localization_data.csv", sep=""),
                                 encoding="UTF-8")[,c(paste0(prefix,"_person_id") ,"pair_city_region","dummy_ex_gdr"),with=FALSE],
    fread(file=paste(path_tools_data,"/localities_data/city_data.csv",sep=""),
                     encoding = "UTF-8"),
    by="pair_city_region",
    all.x=TRUE)
  setkeyv(old_results,
          cols=paste(prefix,"_person_id",sep=""))
  

  original_patent <- fread(paste(path_to_raw_downloaded_data,"patents_",prefix,".csv",sep=""),
                               encoding="UTF-8")
  setkeyv(original_patent,paste0(prefix,"_person_id"))
  
  data_most_common_auth <- original_patent[,.N,by=c(paste0(prefix,"_person_id"),"appln_auth")]
  setorderv(data_most_common_auth,
           c(paste0(prefix,"_person_id"),"N"),
           c(1,-1))


#prepare data for loop that will create localization 
#---#---#---#---#---#---#---#---#---#---#---#---#---#
  #These are only to sort the data in a sensible way
  current_lexicon <- merge(current_lexicon,
                           data_most_common_auth[data_most_common_auth[, .I[1], by=c(paste0(prefix,"_person_id"))]$V1][,list(most_common_appln_auth =appln_auth ),by=eval(paste0(prefix,"_person_id"))],
                           by=paste0(prefix,"_person_id"),
                           all.x = TRUE)
  
  current_lexicon <- merge(current_lexicon,
                           original_patent[,list(min_year=min(appln_filing_year)),by=eval(paste0(prefix,"_person_id"))],
                           by=paste0(prefix,"_person_id"),
                           all.x = TRUE)
  
  
  #split the data into chunks (too big to work on in one go)
  current_lexicon[,localization_ctry:=get(paste0(prefix,"_ctry"))]
  current_lexicon[is.na(localization_ctry)|localization_ctry=="",localization_ctry:=most_common_appln_auth]
  setkeyv(current_lexicon,
          c("localization_ctry","min_year",eval(paste0(prefix,"_eee_hrm_id"))))
  current_lexicon[,rank:=seq_len(.N), by=c("localization_ctry")]
  current_lexicon[,fraction:=ceiling(seq_len(.N)/max_number_of_observation_chunk), by=c("localization_ctry")]
  current_lexicon[,fraction:=max(fraction,na.rm=TRUE),by=c(paste(prefix,"_person_id",sep=""))]
  current_lexicon[,chunck:=paste(localization_ctry,fraction,sep="_")]


  list_of_chunks <- unique(current_lexicon[,list(chunck)])
  
  
  setkeyv(current_lexicon,
          cols=c("chunck"#,paste(prefix,"_person_id",sep="")
          ))
  
  
  
  
  #Just some views of the data
  # current_lexicon[,.N,by=c("localization_ctry","fraction")]
  # 
  # current_lexicon[get(paste(prefix,"_address",sep=""))!="",.N,by=c("localization_ctry")][N>1000][order(get("localization_ctry"))]
  # current_lexicon[,.N,by=c("localization_ctry")][N>1000][order(get("localization_ctry"))]
  # current_lexicon[localization_ctry=="DD",.N,by=c("fraction")]
  # 
  
  list_of_inv_persons_to_test <- data.table("V1"=c(988546,1362112,2022387,418302,4217945,5177365,2422,
                                                              44986488,
                                                              745503,
                                                              44986489,
                                                              274679,
                                                              615539,
                                                              1306860,
                                                              1324248,
                                                              54422,
                                                              54424,
                                                              1467133,
                                                              5004695,
                                                              54509742,
                                                              1702022))
  
  setnames(list_of_inv_persons_to_test,old="V1",new=paste(prefix,"_person_id",sep=""))
  
  print(merge(current_lexicon,
              list_of_inv_persons_to_test,
              by=eval(paste(prefix,"_person_id",sep=""))))
  

#localize id addresses
#---#---#---#---#---#---#---#
{
  
  #loop through all chunks
  for (current_chunck in list_of_chunks[,chunck]) {
  #if (substr(current_chunck,1,2)=="DD"| substr(current_chunck,1,2) =="DE") {
    print(paste(prefix,": localizing chunk ", current_chunck," (",which(current_chunck==list_of_chunks[,chunck])," out of ",nrow(list_of_chunks), ")",sep=""))
    
    
    if (file.exists(file =paste(path_to_raw_downloaded_data,"/data_preparation/localization_",long_name,"s/localization_chunk_",current_chunck,".csv", sep=""))==FALSE|redo_chunks==TRUE) {
            
            
            
            current_data <- current_lexicon[list_of_chunks[current_chunck==chunck]]
            list_inv_current_data <- current_data[,c(paste(prefix,"_person_id",sep="")),with=FALSE]
            overall_obs <- nrow(current_data)
            
            #select the names+adresses that are promising
            #count number of commas, as in Bräuer, Richard, Muenchen
            current_data[,count_commas:=lengths(regmatches(current_data[,get(paste(prefix,"_name",sep=""))], gregexpr("[;,]", current_data[,get(paste(prefix,"_name",sep=""))])))]
            #get the number of first comma and first space
            #This is to identify Richard Bräuer, Muenchen (first space long before first comma)
            current_data[,first_comma:=regexpr("[;,]", current_data[,get(paste(prefix,"_name",sep=""))])]
            current_data[,first_space:=regexpr(" ", current_data[,get(paste(prefix,"_name",sep=""))])]
            
            
            current_data <- current_data[
              #either have an entry in the address field
              ((get(paste(prefix,"_address",sep=""))!="" & !is.na(get(paste(prefix,"_address",sep="")))) )
              |
              #or have so many commas in the name that we expect a city name somewhere
              (count_commas>=2)
              |
              # only one comma, but not the first seperator, so maybe "Richard Bräuer, Muenchen"
              (count_commas==1 & (first_comma>4+first_space & first_space>=1))
              ]
        
            
            current_old_results <- old_results[list_inv_current_data,nomatch=0]

            setkeyv(current_data,
                    cols=paste(prefix,"_person_id",sep=""))
            setkeyv(current_old_results,
                    cols=paste(prefix,"_person_id",sep=""))            

            if(redo_localizations==FALSE){
            current_data[!current_old_results[,paste(prefix,"_person_id",sep=""),with=FALSE],still_to_localize:=TRUE]
            current_data[is.na(still_to_localize),still_to_localize:=FALSE]
            current_data <- current_data[still_to_localize==TRUE]
            setkeyv(current_data,
                    cols=paste(prefix,"_person_id",sep=""))
            }else{
            current_data[,still_to_localize:=TRUE]
            }

            
            #print(current_data[list_of_inv_persons_to_test,nomatch=0][order(get(paste(prefix,"_person_id",sep="")))])
            #print(old_results[merge(list_of_inv_persons_to_test,
            #                  current_data[,c(paste(prefix,"_person_id",sep="")),with=FALSE],
            #                  by=paste(prefix,"_person_id",sep="")),nomatch=0][order(get(paste(prefix,"_person_id",sep="")))])
            nr_of_localization_attempts <- nrow(current_data[still_to_localize==TRUE]) +  nrow(current_old_results)
            

            results_localization <- localize_strings(data_table = current_data[still_to_localize==TRUE], #or to see problems: [list_of_inv_persons_to_test,nomatch=0],
                                   vector_of_names_of_variables_to_localize = c(paste(prefix,"_address",sep=""),paste(prefix,"_name",sep="")),
                                   vector_variables_country_codes = c(paste(prefix,"_ctry",sep="")),
                                   id_variables = c(paste(prefix,"_person_id",sep="")),
                                   threshold_nr_words_to_consider = 0,
                                   extract_country_codes="no",
                                   extract_firstname_info="no",
                                   extract_city_localization="YES",
                                   threshold_acceptance=0.5,
                                   debug_mode = FALSE
                  )
            if (nrow(results_localization)>0){
            localized_chunk <- rbindlist(list(
              results_localization[,c(paste(prefix,"_person_id",sep=""),
                                      "pair_city_region",
                                      "dummy_ex_gdr",
                                      "latitude",
                                      "longitude"),with=FALSE]
              ,
              current_old_results[!results_localization[,c(paste(prefix,"_person_id",sep="")),with=FALSE]][
                !is.na(pair_city_region)|!is.na(dummy_ex_gdr),c(paste(prefix,"_person_id",sep=""),
                                                                "pair_city_region",
                                                                "dummy_ex_gdr",
                                                                "latitude",
                                                                "longitude"),with=FALSE]
            ))
            }else{
              localized_chunk <- current_old_results[
                !is.na(pair_city_region)|!is.na(dummy_ex_gdr),c(paste(prefix,"_person_id",sep=""),
                                                                "pair_city_region",
                                                                "dummy_ex_gdr",
                                                                "latitude",
                                                                "longitude"),with=FALSE]
            }
            
            #print(merge(localized_chunk,
            #            list_of_inv_persons_to_test,
            #            by=eval(paste(prefix,"_person_id",sep=""))))
           
            print(paste(round((nr_of_localization_attempts/overall_obs),digits=2)*100,"% were attempted (of ",overall_obs,")",sep=""))
            print(paste(round((nrow(localized_chunk[!is.na(latitude)])/nr_of_localization_attempts)*100,digits=2),"% of these successfully localized"))
            rm(results_localization)
            fwrite(localized_chunk,
                   file =paste(path_to_raw_downloaded_data,"/data_preparation/localization_",long_name,"s/localization_chunk_",current_chunck,".csv", sep=""))
            rm(localized_chunk)
            
            
    }
  #}
  }
  
  
  localized_data <- rbindlist(
    lapply(X=paste(path_to_raw_downloaded_data,"/data_preparation/localization_",long_name,"s/localization_chunk_",list_of_chunks[,chunck],".csv", sep=""),
         FUN = fread)
    )
  
  fwrite(localized_data[,c(paste(prefix,"_person_id",sep=""),"pair_city_region","dummy_ex_gdr","latitude","longitude"),with=FALSE],
         paste(path_to_raw_downloaded_data,"/data_preparation/localization_",long_name,"s/localization_data.csv", sep=""))
  
  
result_localized_data <- fread(paste(path_to_raw_downloaded_data,"/data_preparation/localization_",long_name,"s/localization_data.csv", sep=""))
result_localized_data[,lapply(.SD,typeof)]
  

}
}