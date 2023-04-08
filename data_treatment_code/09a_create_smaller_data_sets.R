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
         paste(path_to_output_data,"/",sub_data_name,"_relevant_data/","/raw_data_inv_patent",".csv",sep=""))
  
  
  
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
         paste(path_to_output_data,"/",sub_data_name,"_relevant_data/","/raw_data_apl_patent",".csv",sep=""))
  
  
  
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
         paste(path_to_output_data,"/",sub_data_name,"_relevant_data/","/enriched_apl_id_lexicon",".csv",sep=""))
  
  
  
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
         paste(path_to_output_data,"/",sub_data_name,"_relevant_data/","/enriched_inv_id_lexicon",".csv",sep=""))
  
  
  
}


