
print("09: assemble final data set for analysis")
if (create_final_data ==TRUE) {
#----------------
#fread epo files
#----------------
{
#inv patent_data
#---#---#---#---#
original_inv_patent <-unique(fread(paste(path_to_raw_downloaded_data,"patents_inv.csv",sep=""),
                                    encoding="UTF-8")[,list(appln_id, appln_nr_epodoc, appln_auth, 
                                                            appln_filing_year, docdb_family_id,
                                                            inv_person_id, inv_han_id, 
                                                            inv_eee_hrm_id, inv_ctry)])
setkey(original_inv_patent,inv_person_id)

#apl patent_data
#---#---#---#---#
original_apl_patent <- unique(fread(paste(path_to_raw_downloaded_data,"patents_apl.csv",sep=""),
                             encoding="UTF-8")[,list(appln_id, appln_nr_epodoc, appln_auth, 
                                                     appln_filing_year, docdb_family_id,
                                                     apl_person_id, apl_han_id, 
                                                     apl_eee_hrm_id, apl_ctry)])
setkey(original_apl_patent,apl_person_id)


#inv names etc
#---#---#---#---#
original_inv_lexicon <- fread(paste(path_to_raw_downloaded_data,"lexicon_inv.csv",sep=""),
                              encoding="UTF-8")
setkey(original_inv_lexicon,inv_person_id)

#apl names etc
#---#---#---#---#
original_apl_lexicon <- fread(paste(path_to_raw_downloaded_data,"lexicon_apl.csv",sep=""),
                              encoding="UTF-8")
setkey(original_apl_lexicon,apl_person_id)

#inv patent_data
#---#---#---#---#
patent_citations <- fread(paste(path_to_raw_downloaded_data,"patent_citations.csv",sep=""),
                             encoding="UTF-8")
setkey(patent_citations,appln_id)

}





#----------------
#read preparation
#----------------
#the additional results from preparation have to be merged to lexicon
#and an employer-employee file has to be created. First, we read in 
#all the additional preparation results
{
  
  #localization inventors
  #---#---#---#---#---#---#
  localization_inv <- fread(paste(path_to_output_data,"/data_preparation/localization_inventors/localization_data.csv",sep=""),
        encoding="UTF-8")
  
  #ipc communities inventors
  #---#---#---#---#---#---#
  community_statistics_per_inventor <- fread(file=paste(path_to_output_data,"/",data_name,"/data_preparation/community_statistics_per_inventor",".csv", sep=""),
                                             encoding="UTF-8") 
  
  
  #localization applicants
  #---#---#---#---#---#---#
  localization_apl <- fread(paste(path_to_output_data,"/data_preparation/localization_applicants/localization_data.csv",sep=""),
                            encoding="UTF-8")
  
  #cleaned name ids
  #---#---#---#---#
  inventor_cleaned_names_list <- fread(paste(path_to_output_data,"/data_preparation/inventor_cleaned_names_list.csv",sep=""),
                                       encoding="UTF-8")
  setkey(inventor_cleaned_names_list,inv_eee_hrm_id)
  
  #-------------------------
  #Read in disambiguation result
  #-------------------------
  if (file.exists(paste(path_to_output_data,"/data_preparation/","/PatentsView disambiguation/inventors/PatentsView_identifiers.csv",sep=""))==TRUE){
      PatentsView_identifiers <- fread(paste(path_to_output_data,"/data_preparation/","/PatentsView disambiguation/inventors/PatentsView_identifiers.csv",sep=""),
                                       encoding="UTF-8"
      )
    }
  if (exists("PatentsView_identifiers")==TRUE){
    setnames(PatentsView_identifiers,
             old=c("old_id"),
             new=c("inv_person_id"))
    setkey(PatentsView_identifiers,inv_person_id)
    PatentsView_identifiers[,inv_person_id:=as.numeric(inv_person_id)]
  }
  if (exists("PatentsView_identifiers")==FALSE){
    PatentsView_identifiers <- fread(file=paste(path_to_output_data,"/","/list_cleaned_ids_",data_name_short,".csv",sep=""),
                                     encoding="UTF-8"
    )
  }
  #sbr ids
  #---#---#---#---#
  bridge_sbr_id_patent_data <- unique(fread(file=paste(path_to_output_data,"/link_gdr_registry/","bridge_sbr_id_patent_data.csv",sep=""),
                                            encoding="UTF-8")[,list(sbr_id,apl_eee_hrm_id)])
  bridge_sbr_id_patent_data[,N:=.N,by="apl_eee_hrm_id"]

  
}


      
    
#----------------
#create lexicon (inv)
#----------------
#the additional results from preparation have to be merged to lexicon
#and an employer-employee file has to be created. Second, we merge all
#info to PatentsView to create lexicon
{
  #take ids from disambiguation
  enriched_inv_id_lexicon <- unique(PatentsView_identifiers[,c("inv_person_id",id_variable_to_use),with=FALSE])
  #merge with og info
  enriched_inv_id_lexicon <- merge(enriched_inv_id_lexicon,
                                       original_inv_lexicon,
                                       by="inv_person_id",
                                       all.x=TRUE)
  #merge with localization
  enriched_inv_id_lexicon <- merge(enriched_inv_id_lexicon,
                                       localization_inv,
                                       by="inv_person_id",
                                       all.x=TRUE
  )
  #merge with ipc statistics and tech clusters
  enriched_inv_id_lexicon <- merge(enriched_inv_id_lexicon,
                                   community_statistics_per_inventor,
                                   by=id_variable_to_use,
                                   all.x=TRUE
  )
  
  #merge with cleaned_ids
  enriched_inv_id_lexicon <- merge(enriched_inv_id_lexicon,
                                       inventor_cleaned_names_list[,list(inv_eee_hrm_id, cleaned_id,cleaned_names,nr_good_name_components)],
                                       by="inv_eee_hrm_id",
                                       all.x=TRUE
  )    
  
  
  setnames(enriched_inv_id_lexicon,
           old=id_variable_to_use,
           new="inv_rl_cl_uni_id")
  fwrite(enriched_inv_id_lexicon,
         file=paste(path_to_output_data,"/","/enriched_inv_id_lexicon",".csv",sep=""))
  
  
  
}



#-----------------------
#create patent data (inv)
#-----------------------
{
gdr_inv_patent <- merge(
  PatentsView_identifiers,
  original_inv_patent,
  by=c("inv_person_id")
)
setnames(gdr_inv_patent,
         old=id_variable_to_use,
         new="inv_rl_cl_uni_id")
fwrite(gdr_inv_patent,
       paste(path_to_output_data,"/","/raw_data_inv_patent",".csv",sep=""))

}




#----------------
#create lexicon (apl)
#----------------
#the additional results from preparation have to be merged to lexicon
#and an employer-employee file has to be created. Second, we merge all
#info to PatentsView to create lexicon
{
  #take ids from inv data
  enriched_apl_id_lexicon <- unique(merge(unique(original_apl_patent[,c("apl_person_id","appln_nr_epodoc"),with=FALSE]),
                                   unique(gdr_inv_patent[,list(appln_nr_epodoc)]),
                                   by="appln_nr_epodoc")[,list(apl_person_id)])
  #merge with og info
  enriched_apl_id_lexicon <- merge(enriched_apl_id_lexicon,
                                       original_apl_lexicon,
                                       by="apl_person_id",
                                       all.x=TRUE)
  #merge with localization
  enriched_apl_id_lexicon <- merge(enriched_apl_id_lexicon,
                                       localization_apl,
                                       by="apl_person_id",
                                       all.x=TRUE
  )
  
  #merge with gdr ids
  enriched_apl_id_lexicon <- merge(enriched_apl_id_lexicon,
                                   bridge_sbr_id_patent_data[N==1],
                                   by="apl_eee_hrm_id",
                                   all.x=TRUE
  )  
  
  enriched_apl_id_lexicon[!is.na(sbr_id),apl_rl_cl_uni_id:=max(apl_eee_hrm_id),by="sbr_id"]
  enriched_apl_id_lexicon[is.na(sbr_id),apl_rl_cl_uni_id:=apl_eee_hrm_id]
  enriched_apl_id_lexicon[,N:=NULL]
  fwrite(enriched_apl_id_lexicon,file=paste(path_to_output_data,"/","/enriched_apl_id_lexicon" ,".csv",sep=""))
  
  
  
}



#-----------------------
#create patent data (apl)
#-----------------------
{
  
  gdr_apl_patent <- merge(original_apl_patent,
        unique(gdr_inv_patent[,list(appln_nr_epodoc)]),
        by="appln_nr_epodoc")
  
  gdr_apl_patent <- merge(gdr_apl_patent,
                          enriched_apl_id_lexicon[,list(apl_person_id,apl_rl_cl_uni_id)],
                          by="apl_person_id")
  
  fwrite(gdr_apl_patent,
         paste(path_to_output_data,"/","/raw_data_apl_patent",".csv",sep=""))
  
}

#----------------
#create citations
#----------------
#data on how often every patent family is cited by every other patent family
  
  
  citations_family_citing_family <-  patent_citations[!is.na(docdb_family_id)
                                                      ,list(nr_citing_families_family=uniqueN(citing_docdb_family_id)),by="docdb_family_id"]
  
  citations_family_citing_family[,.N,by="nr_citing_families_family"][order(nr_citing_families_family)]
  
fwrite(citations_family_citing_family[docdb_family_id!=0],
       file = paste(path_to_output_data,
                   "citations_family_citing_family",".csv", sep="") )

  
}

