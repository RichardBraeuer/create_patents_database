


list_of_data_paths <- list.dirs(path = path_to_output_data,
                          recursive = FALSE)
list_of_data_paths <- paste0(list_of_data_paths[grepl(x=list_of_data_paths,pattern="_relevant_data")],"/")



print("raw_data_inv_patent")
inv_patent <- fread(paste(path_to_output_data,"/","/raw_data_inv_patent",".csv",sep=""),
                    encoding="UTF-8")
setkey(inv_patent,appln_nr_epodoc)


for (sub_data_path in list_of_data_paths) {
  sub_data_name <-gsub(x=sub_data_path,pattern=path_to_output_data,replacement="")
  sub_data_name <-gsub(x=sub_data_name,pattern="_relevant_data/",replacement="")
  sub_data_name <-gsub(x=sub_data_name,pattern="/",replacement="")
  
  
  print(sub_data_name)

  #patent files
  list_patents <- fread(file = paste0(sub_data_path,"list_patents_",sub_data_name,".csv"),
                        encoding = "UTF-8")
  
  raw_data_inv_patent <- merge(inv_patent,
                               list_patents,
                               by="appln_nr_epodoc")
  
  fwrite(raw_data_inv_patent,
         paste(path_to_output_data,"/",sub_data_name,"_relevant_data/","/raw_data_inv_patent_",version_description,".csv",sep=""))
  
  
  
}













print("raw_data_apl_patent")
apl_patent <- fread(paste(path_to_output_data,"/","/raw_data_apl_patent",".csv",sep=""),
                    encoding="UTF-8")
setkey(apl_patent,appln_nr_epodoc)


for (sub_data_path in list_of_data_paths) {
  sub_data_name <-gsub(x=sub_data_path,pattern=path_to_output_data,replacement="")
  sub_data_name <-gsub(x=sub_data_name,pattern="_relevant_data/",replacement="")
  sub_data_name <-gsub(x=sub_data_name,pattern="/",replacement="")
  
  
  print(sub_data_name)
  
  #patent files
  list_patents <- fread(file = paste0(sub_data_path,"list_patents_",sub_data_name,".csv"),
                        encoding = "UTF-8")
  
  raw_data_apl_patent <- merge(apl_patent,
                               list_patents,
                               by="appln_nr_epodoc")
  
  fwrite(raw_data_apl_patent,
         paste(path_to_output_data,"/",sub_data_name,"_relevant_data/","/raw_data_apl_patent_",version_description,".csv",sep=""))
  
  
  
}















print("enriched_apl_id_lexicon")
enriched_apl_id_lexicon <- fread(paste(path_to_output_data,"/","/enriched_apl_id_lexicon",".csv",sep=""),
                    encoding="UTF-8")
setkey(enriched_apl_id_lexicon,apl_person_id)


for (sub_data_path in list_of_data_paths) {
  sub_data_name <-gsub(x=sub_data_path,pattern=path_to_output_data,replacement="")
  sub_data_name <-gsub(x=sub_data_name,pattern="_relevant_data/",replacement="")
  sub_data_name <-gsub(x=sub_data_name,pattern="/",replacement="")
  
  
  print(sub_data_name)
  
  enriched_apl_id_lexicon_sub <- merge(
    enriched_apl_id_lexicon,
      unique(fread(file=paste(path_to_output_data,"/",sub_data_name,"_relevant_data/","/list_applicants_",sub_data_name,".csv",sep=""))[,
                                                                                                                     list(apl_person_id=person_id)]),
      by="apl_person_id"
)
  
  
  fwrite(enriched_apl_id_lexicon_sub,
         paste(path_to_output_data,"/",sub_data_name,"_relevant_data/","/enriched_apl_id_lexicon_",version_description,".csv",sep=""))
  
  
  
}














print("enriched_inv_id_lexicon")
enriched_inv_id_lexicon <- fread(paste(path_to_output_data,"/","/enriched_inv_id_lexicon",".csv",sep=""),
                                 encoding="UTF-8")
setkey(enriched_inv_id_lexicon,inv_person_id)


for (sub_data_path in list_of_data_paths) {
  sub_data_name <-gsub(x=sub_data_path,pattern=path_to_output_data,replacement="")
  sub_data_name <-gsub(x=sub_data_name,pattern="_relevant_data/",replacement="")
  sub_data_name <-gsub(x=sub_data_name,pattern="/",replacement="")
  
  
  print(sub_data_name)
  
  enriched_inv_id_lexicon_sub <- merge(
    enriched_inv_id_lexicon,
    unique(fread(file=paste(path_to_output_data,"/",sub_data_name,"_relevant_data/","/list_applicants_",sub_data_name,".csv",sep=""))[,
                                                                                                                                        list(inv_person_id=person_id)]),
    by="inv_person_id"
  )
  
  
  fwrite(enriched_inv_id_lexicon_sub,
         paste(path_to_output_data,"/",sub_data_name,"_relevant_data/","/enriched_inv_id_lexicon_",version_description,".csv",sep=""))
  
  
  
}




print("treated_employer_employee_file")

for (sub_data_path in list_of_data_paths) {
  sub_data_name <-gsub(x=sub_data_path,pattern=path_to_output_data,replacement="")
  sub_data_name <-gsub(x=sub_data_name,pattern="_relevant_data/",replacement="")
  sub_data_name <-gsub(x=sub_data_name,pattern="/",replacement="")
  
  
  print(sub_data_name)

  
  pairings_inventor_firm <- unique( merge( 
    fread(paste(path_to_output_data,"/",sub_data_name,"_relevant_data/","/raw_data_inv_patent_",version_description,".csv",sep=""),
          encoding="UTF-8"),
    fread(paste(path_to_output_data,"/",sub_data_name,"_relevant_data/","/raw_data_apl_patent_",version_description,".csv",sep=""),
          encoding="UTF-8"),
    by="appln_nr_epodoc",
    allow.cartesian=TRUE,
    all=TRUE
  ) 
  )[!is.na(inv_rl_cl_uni_id)|!is.na(apl_rl_cl_uni_id)]
  
  pairings_inventor_firm[appln_nr_epodoc=="DD19850280195"]
  pairings_inventor_firm[inv_rl_cl_uni_id==8612775,]
  
  pairings_inventor_firm <- unique(merge(pairings_inventor_firm,
                                         unique(fread(paste(path_to_output_data,"/",sub_data_name,"_relevant_data/","/raw_data_inv_patent_",version_description,".csv",sep=""),
                                                      encoding="UTF-8")[,list(docdb_family_id,appln_nr_epodoc,appln_filing_year)][,list(appln_nr_epodoc,appln_filing_year=min(appln_filing_year)),by="docdb_family_id"]),
                                         by="appln_nr_epodoc")[,list(inv_rl_cl_uni_id, apl_rl_cl_uni_id,docdb_family_id,appln_filing_year,inv_ctry,apl_ctry)])
 
  #---------------------------------------
  #Assign inventors to firms based on most
  #frequent pairing
  #---------------------------------------  
  pairings_inventor_firm[,mentions_of_firm:=.N,by=c("inv_rl_cl_uni_id","apl_rl_cl_uni_id")]
  pairings_inventor_firm[,most_commonly_associated:=max(mentions_of_firm)==mentions_of_firm,by=c("inv_rl_cl_uni_id","docdb_family_id")]  
  nrow(pairings_inventor_firm[most_commonly_associated==1])
  pairings_inventor_firm<-pairings_inventor_firm[most_commonly_associated==1]
  pairings_inventor_firm[,indicator_ambigous_which_firm:=.N,by=c("inv_rl_cl_uni_id","docdb_family_id")]
  round(nrow(pairings_inventor_firm[indicator_ambigous_which_firm>1])/nrow(pairings_inventor_firm[]),digits=3)
  nrow(pairings_inventor_firm[indicator_ambigous_which_firm==1])
  pairings_inventor_firm[,share_invested_in_match:=1/indicator_ambigous_which_firm,by=c("inv_rl_cl_uni_id","docdb_family_id")]
  
  
  pairings_inventor_firm[,patent_value_match:=(1/inventors_per_invention)*(  share_invested_in_match),by=c("inv_rl_cl_uni_id","docdb_family_id")]
  
  max(pairings_inventor_firm[,patent_value_match])
  min(pairings_inventor_firm[,patent_value_match])
  
  setkey(pairings_inventor_firm,docdb_family_id,inv_rl_cl_uni_id,apl_rl_cl_uni_id)
  length(unique(pairings_inventor_firm[,apl_rl_cl_uni_id]))
  length(unique(pairings_inventor_firm[,inv_rl_cl_uni_id]))
  
  #save treated file for actual estimation  
  fwrite(pairings_inventor_firm,
         file =paste(path_to_output_data,"/",sub_data_name,"_relevant_data/","treated_employer_employee_file_",version_description,".csv", sep="") 
  )
  
  
}




print("citations_family_citing_family")


citations_family_citing_family <- fread(file = paste(path_to_output_data,
                    "citations_family_citing_family",".csv", sep="") )


for (sub_data_path in list_of_data_paths) {
  sub_data_name <-gsub(x=sub_data_path,pattern=path_to_output_data,replacement="")
  sub_data_name <-gsub(x=sub_data_name,pattern="_relevant_data/",replacement="")
  sub_data_name <-gsub(x=sub_data_name,pattern="/",replacement="")
  
  
  print(sub_data_name)
  
  sub_citations_family_citing_family <- merge(
    citations_family_citing_family,
    merge(fread(file = paste0(sub_data_path,"list_patents_",sub_data_name,".csv"),
                    encoding = "UTF-8"),
          unique(inv_patent[,list(docdb_family_id,appln_nr_epodoc)]),
          by="appln_nr_epodoc"),
    by="docdb_family_id")
  
  fwrite(sub_citations_family_citing_family,
         paste(path_to_output_data,"/",sub_data_name,"_relevant_data/","/citations_family_citing_family_",version_description,".csv",sep=""),
        encoding="UTF-8")
}