


localize_chunk <- function(current_data,prefix,split_further=FALSE) {
  
  current_chunck <- current_data[1,chunck]
  attempt<-FALSE
  if (file.exists(file =paste(path_to_raw_downloaded_data,"/data_preparation/localization_",long_name,"s/localization_chunk_",current_chunck,".csv", sep=""))==FALSE|redo_chunks==TRUE) {
  attempt<-TRUE
  print(paste(prefix,": localizing chunk ", current_chunck,sep=""))
  
  #list_inv_current_data <- current_data[,c(paste(prefix,"_person_id",sep="")),with=FALSE]
  overall_obs <- nrow(current_data)
  
  
  setkeyv(current_data,
          cols=paste(prefix,"_person_id",sep=""))


  nr_of_localization_attempts <- nrow(current_data[])
  
  
  if (split_further==TRUE){
  current_data[,chunk2:= ceiling(seq_len(.N)/(max_number_of_observation_chunk/100)),
                  by=c("chunck")]
  current_data[,chunk2:=as.character(chunk2)]
  current_data[,chunk2:=paste(current_chunck,chunk2,sep="_")]
  }
  if (split_further==FALSE){
    current_data[,chunk2:=current_chunck]
  }

  print(split_further)
  print(colnames(current_data))
  print(current_data[,.N,by="chunk2"])
  for (current_chunk2 in current_data[,.N,by="chunk2"][,chunk2]) {
  print(current_chunk2)
  if (file.exists(paste(path_to_raw_downloaded_data,"/data_preparation/localization_",long_name,"s/localization_chunk_",current_chunk2,".csv", sep=""))==FALSE){

  results_localization <- localize_strings(current_data[chunk2==current_chunk2], #or to see problems: [list_of_inv_persons_to_test,nomatch=0],
                                           vector_of_names_of_variables_to_localize = c(paste(prefix,"_address",sep=""),paste(prefix,"_name",sep="")),
                                           vector_variables_country_codes = c(paste(prefix,"_ctry",sep="")),
                                           version_of_localizations = version_of_localizations,
                                           #vector_varnames_region_codes =c(paste(prefix,"_nuts",sep="")),
                                           vector_varnames_regions = c(paste(prefix,"_nuts",sep=""),paste(prefix,"_address",sep=""),paste(prefix,"_name",sep="")),
                                           id_variables = c(paste(prefix,"_person_id",sep="")),
                                           threshold_nr_words_to_consider = 0,
                                           extract_country_codes="no",
                                           extract_firstname_info="no",
                                           extract_city_localization="YES",
                                           threshold_acceptance=0.5,
                                           debug_mode = FALSE,
                                           verbose=FALSE,
                                           only_fitting_blocks =FALSE,
                                           select_most_populous=TRUE,
  )

  if (nrow(results_localization)==0){
    results_localization <- as.data.table(
      setnames(data.frame(matrix(ncol = length(c(paste(prefix,"_person_id",sep=""),
                                                 "pair_city_region",
                                                 "dummy_ex_gdr",
                                                 "country_code_city",
                                                 "NUTS2_equivalent_code",
                                                 "county_code",
                                                 "latitude",
                                                 "longitude")),
                                 nrow = 0)), c(paste(prefix,"_person_id",sep=""),
                                               "pair_city_region",
                                               "dummy_ex_gdr",
                                               "country_code_city",
                                               "NUTS2_equivalent_code",
                                               "county_code",
                                               "latitude",
                                               "longitude"))
    )
  }
  
  
  results_localization[is.na(NUTS2_equivalent_code),NUTS2_equivalent_code :=extracted_region ]

  
  localized_chunk <- results_localization[,c(paste(prefix,"_person_id",sep=""),
                                               "pair_city_region",
                                               "dummy_ex_gdr",
                                               "country_code_city",
                                               "NUTS2_equivalent_code",
                                               "county_code",
                                               "latitude",
                                               "longitude"),with=FALSE]
  
  

  #print(paste(round((nr_of_localization_attempts/overall_obs),digits=2)*100,"% were attempted (of ",overall_obs,")",sep=""))
  print(paste(round((nrow(localized_chunk[!is.na(latitude)])/nr_of_localization_attempts)*100,digits=2),"% of attempted successfully localized"))
  rm(results_localization)
  fwrite(localized_chunk,
         file =paste(path_to_raw_downloaded_data,"/data_preparation/localization_",long_name,"s/localization_chunk_",current_chunk2,".csv", sep=""),
         encoding="UTF-8")
  rm(localized_chunk)
  }
  }
  
  
  localized_data_complete_chunk <- rbindlist(
    lapply(X=paste(path_to_raw_downloaded_data,"/data_preparation/localization_",long_name,"s/localization_chunk_",current_data[,.N,by="chunk2"][,chunk2],".csv", sep=""),
           FUN = fread,
           encoding="UTF-8"),
    use.names=TRUE,fill=TRUE
  )
  
  fwrite(localized_data_complete_chunk,
         file =paste(path_to_raw_downloaded_data,"/data_preparation/localization_",long_name,"s/localization_chunk_",current_chunck,".csv", sep=""),
         encoding="UTF-8")
  rm(localized_data_complete_chunk)
  
  
  }

  return(attempt)
  
  }







max_number_of_observation_chunk <- 100000
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
{
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
}
#read in the complete data set and result of previous attempts
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
{
  current_lexicon <- fread(file =paste(path_to_raw_downloaded_data,"lexicon_",prefix,".csv", sep=""),
        encoding="UTF-8")
  
  old_results <- merge(
    fread(paste(path_to_raw_downloaded_data,"/data_preparation/localization_",long_name,"s/localization_data.csv", sep=""),
                                 encoding="UTF-8")[,c(paste0(prefix,"_person_id") ,"pair_city_region","dummy_ex_gdr"),with=FALSE],
    fread(file=paste(path_tools_data,"/localities_data/city_data_",version_of_localizations,".csv",sep=""),
                     encoding = "UTF-8"),
    by=c("pair_city_region","dummy_ex_gdr"),
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

}
#prepare data for loop that will create localization 
#---#---#---#---#---#---#---#---#---#---#---#---#---#
{
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
  
  
  
  
  #select the names+adresses that are promising
  #count number of commas, as in Bräuer, Richard, Muenchen
  current_lexicon[,count_commas:=lengths(regmatches(current_lexicon[,get(paste(prefix,"_name",sep=""))], gregexpr("[;,]", current_lexicon[,get(paste(prefix,"_name",sep=""))])))]
  #get the number of first comma and first space
  #This is to identify Richard Bräuer, Muenchen (first space long before first comma)
  current_lexicon[,first_comma:=regexpr("[;,]", current_lexicon[,get(paste(prefix,"_name",sep=""))])]
  current_lexicon[,first_space:=regexpr(" ", current_lexicon[,get(paste(prefix,"_name",sep=""))])]
  
  
  
  if(redo_localizations==FALSE){
    current_lexicon[!old_results[!is.na(pair_city_region),
                                 paste(prefix,"_person_id",sep=""),with=FALSE],
                    still_to_localize:=TRUE]
    current_lexicon[is.na(still_to_localize),still_to_localize:=FALSE]
    setkeyv(current_lexicon,
            cols=paste(prefix,"_person_id",sep=""))
  }else{
    current_lexicon[,still_to_localize:=TRUE]
  }
  
  
  current_lexicon[
    #either have an entry in the address field
    ((get(paste(prefix,"_address",sep=""))!="" & !is.na(get(paste(prefix,"_address",sep="")))) )
    |
      #or have so many commas in the name that we expect a city name somewhere
      (count_commas>=2)
    |
      # only one comma, but not the first seperator, so maybe "Richard Bräuer, Muenchen"
      (count_commas==1 & (first_comma>4+first_space & first_space>=1))
    ,worthwhile_to_localize :=TRUE
  ]
  
  
  
  setkeyv(current_lexicon,
          cols=c("chunck"#,paste(prefix,"_person_id",sep="")
          ))
  
  
}

#localize id addresses
#---#---#---#---#---#---#---#
{
  
  #loop through all chunks
  library(parallel)
  library(foreach)
  
  
  list_of_chunk_names <- unique(current_lexicon[still_to_localize==TRUE & worthwhile_to_localize==TRUE,list(chunck)])
  print("-----------------------")
  print(nrow(list_of_chunk_names))
  list_of_chunks <- split(unique(current_lexicon[still_to_localize==TRUE & worthwhile_to_localize==TRUE,
                                          c("chunck",
                                            paste(prefix,"_person_id",sep=""),
                                            paste(prefix,"_address",sep=""),
                                            paste(prefix,"_name",sep=""),
                                            paste(prefix,"_nuts",sep=""),
                                            paste(prefix,"_ctry",sep="")),with=FALSE]),
                          by=c("chunck"))
  print(object.size(current_lexicon), units = "Gb")
  print(object.size(list_of_chunks), units = "Gb")
  print(lapply(list_of_chunks[1:10],object.size), units = "Kb")
  
  print(length(list_of_chunks[]))

  
  lapply(X=list_of_chunks[],
         FUN=localize_chunk,
         prefix=prefix,
         split_further=TRUE)

  setkeyv(current_lexicon,paste(prefix,"_person_id",sep=""))
  localize_chunk(current_data = current_lexicon[list_of_inv_persons_to_test],
                 prefix=prefix)
  
  
# 
#   
#   
#   future_lapply(X=list_of_chunks[1:10,chunck],
#          FUN=localize_chunk,
#          prefix=prefix,
#          current_lexicon=current_lexicon,
#          old_results=old_results)
#   foreach (current_chunck = list_of_chunks[,chunck], .packages='data.table') %dopar% {
#   #if (substr(current_chunck,1,2)=="DD"| substr(current_chunck,1,2) =="DE") {
#    
#       localize_chunk(current_data=list_of_chunks[[1]],
#                      prefix=prefix)
#       
#     
#     lapply(X=list_of_chunks[4000:3000],
#                   FUN=localize_chunk,
#                   prefix=prefix)
#     
#     plan(sequential)
#     plan(multisession, workers = 10)
#     lapply(X=list_of_chunks,
#                   FUN=localize_chunk,
#                   prefix=prefix)
#             
#     
#   #}
#   }
#   

  localized_data <- rbindlist(
    lapply(X=paste(path_to_raw_downloaded_data,"/data_preparation/localization_",long_name,"s/localization_chunk_",list_of_chunk_names[,chunck],".csv", sep=""),
         FUN = fread),
    use.names=TRUE,fill=TRUE
    )

  fwrite(localized_data[,c(paste(prefix,"_person_id",sep=""),"pair_city_region",
                           "dummy_ex_gdr","NUTS2_equivalent_code","county_code","country_code_city","latitude","longitude"),with=FALSE],
         paste(path_to_raw_downloaded_data,"/data_preparation/localization_",long_name,"s/localization_data_",version_of_localizations,".csv", sep=""),
         encoding="UTF-8")
  
  
result_localized_data <- fread(paste(path_to_raw_downloaded_data,"/data_preparation/localization_",long_name,"s/localization_data_",version_of_localizations,".csv", sep=""))
result_localized_data[,lapply(.SD,typeof)]
  

}
}