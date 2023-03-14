debugging_run <- FALSE

library(data.table)
library(stringr)



#data on locations
#---#---#---#---#  
city_data <- fread(file=paste(path_tools_data,"/localities_data/city_data_",version_of_localizations,".csv",sep=""),
                   encoding = "UTF-8")


#---list of inventors relevant for GDR
#---#---#---#---#---#---#---#---#---#
if (file.exists(paste(path_to_output_data,"/gdr_relevant_data/list_cleaned_ids_gdr.csv",sep="")) ==FALSE |
    file.exists(paste(path_to_output_data,"/gdr_relevant_data/list_patents_gdr.csv",sep=""))     ==FALSE |
    file.exists(paste(path_to_output_data,"/gdr_relevant_data/list_applicants_gdr.csv",sep=""))     ==FALSE 
    ){
  

basis_gdr_data_selection <- merge(fread(paste(path_to_raw_downloaded_data,"patents_inv.csv",sep=""),
                                    encoding="UTF-8"),
                              unique(fread(paste(path_to_raw_downloaded_data,"/data_preparation/inventor_cleaned_names_list.csv",sep=""),
                                           encoding="UTF-8")[,list(inv_eee_hrm_id, cleaned_id)]),
                              by="inv_eee_hrm_id",
                              all.x=TRUE)[
                                (inv_ctry=="DD"|inv_ctry=="DE") | 
                                (appln_auth =="DD"|appln_auth =="DE")| 
                                (appln_auth =="EP" & inv_ctry==""),
                                list(inv_person_id ,inventor_id=as.character(cleaned_id),appln_nr_epodoc)
                              ]

list_cleaned_ids_gdr <- unique(basis_gdr_data_selection[,list(inv_person_id ,inventor_id)])


print(paste("nr of cleaned IDs for gdr data:",nrow(list_cleaned_ids_gdr)))

fwrite(list_cleaned_ids_gdr,
       file=paste(path_to_output_data,"/gdr_relevant_data/list_cleaned_ids_gdr.csv",sep=""),
       quote=TRUE)


list_patents_gdr <- unique(basis_gdr_data_selection[,list(appln_nr_epodoc)])


print(paste("nr of patents in gdr data:",nrow(list_patents_gdr)))

fwrite(list_patents_gdr,
       file=paste(path_to_output_data,"/gdr_relevant_data/list_patents_gdr.csv",sep=""),
       quote=TRUE)


list_apl_gdr<- unique(
  merge(fread(paste(path_to_raw_downloaded_data,"patents_apl.csv",sep=""),
      encoding="UTF-8"),
      list_patents_gdr,
      by="appln_nr_epodoc")[,list(apl_person_id)]
)
setkey(list_apl_gdr,apl_person_id)


rich2 <- fread(file = paste(path_to_raw_downloaded_data, "/amadeus_merge_remerge/", "rich2.csv", sep=""),
               encoding="UTF-8")
rich2[,person_id:=as.numeric(person_id)]
list_apl_gdr <- merge(unique(rich2[,list(patstat_id ,person_id)],by="person_id"),
      list_apl_gdr,
      by.x="person_id",
      by.y="apl_person_id",
      all.x=TRUE)


fwrite(list_apl_gdr,
       file=paste(path_to_output_data,"/gdr_relevant_data/list_applicants_gdr.csv",sep=""),
       quote=TRUE)



}




#---list of inventors relevant for USSR
#---#---#---#---#---#---#---#---#---#
dir.create(paste0(path_to_output_data,"/ussr_relevant_data/"))
if (file.exists(paste(path_to_output_data,"/ussr_relevant_data/list_cleaned_ids_ussr.csv",sep="")) ==FALSE |
    file.exists(paste(path_to_output_data,"/ussr_relevant_data/list_patents_ussr.csv",sep=""))     ==FALSE 
){
  
  
  basis_ussr_data_selection <- merge(fread(paste(path_to_raw_downloaded_data,"patents_inv.csv",sep=""),
                                          encoding="UTF-8"),
                                    unique(fread(paste(path_to_raw_downloaded_data,"/data_preparation/inventor_cleaned_names_list.csv",sep=""),
                                                 encoding="UTF-8")[,list(inv_eee_hrm_id, cleaned_id)]),
                                    by="inv_eee_hrm_id",
                                    all.x=TRUE)[
                                      (inv_ctry=="SU"|inv_ctry=="RU") | 
                                        (appln_auth =="SU"|appln_auth =="RU"),
                                      list(inv_person_id ,inventor_id=as.character(cleaned_id),appln_nr_epodoc)
                                    ]
  list_cleaned_ids_ussr <- unique(basis_ussr_data_selection[,list(inv_person_id ,inventor_id)])
  
  
  print(paste("nr of cleaned IDs for ussr data:",nrow(list_cleaned_ids_ussr)))
  
  fwrite(list_cleaned_ids_ussr,
         file=paste(path_to_output_data,"/ussr_relevant_data/list_cleaned_ids_ussr.csv",sep=""),
         quote=TRUE)
  
  
  list_patents_ussr <- unique(basis_ussr_data_selection[,list(appln_nr_epodoc)])
  
  
  print(paste("nr of patents in ussr data:",nrow(list_patents_ussr)))
  
  fwrite(list_patents_ussr,
         file=paste(path_to_output_data,"/ussr_relevant_data/list_patents_ussr.csv",sep=""),
         quote=TRUE)
  
  
  list_apl_ussr<- unique(
    merge(fread(paste(path_to_raw_downloaded_data,"patents_apl.csv",sep=""),
                encoding="UTF-8"),
          list_patents_ussr,
          by="appln_nr_epodoc")[,list(apl_person_id)]
  )
  setkey(list_apl_ussr,apl_person_id)
  
  
  
  rich2 <- fread(file = paste(path_to_raw_downloaded_data, "/amadeus_merge_remerge/", "rich2.csv", sep=""),
                 encoding="UTF-8")
  rich2[,person_id:=as.numeric(person_id)]
  list_apl_ussr <- merge(unique(rich2[,list(patstat_id ,person_id)],by="person_id"),
                         list_apl_ussr,
                        by.x="person_id",
                        by.y="apl_person_id",
                        all.x=TRUE)
  
  
  
  fwrite(list_apl_ussr,
         file=paste(path_to_output_data,"/ussr_relevant_data/list_applicants_ussr.csv",sep=""),
         quote=TRUE)
  
}










#---files as in USPTO
#---#---#---#---#---#
{
  

  
  #create raw_location file
  #---#---#---#---#---#---#
  if (file.exists(paste(path_to_output_data,"/rawlocation.tsv",sep="")) ==FALSE |
      file.exists(paste(path_to_output_data,"/bridge_raw_location_person.csv",sep="")) ==FALSE ){
  
  
  #create raw_location_file
  raw_location <- unique(rbindlist(list(
    merge(
      #original inv lexicon
      fread(paste(path_to_raw_downloaded_data,"lexicon_inv.csv",sep=""),
          encoding="UTF-8"),
      #merged with localization result
      fread(paste(path_to_raw_downloaded_data,"/data_preparation/localization_inventors/localization_data_",version_of_localizations,".csv",sep=""),
                encoding="UTF-8"),
      by="inv_person_id",
      all.x=TRUE)[,list(person_id=inv_person_id,address=inv_address,pair_city_region , 
                        state_local=NUTS2_equivalent_code,
                        state_patent=inv_nuts,
                        country_local=country_code_city,
                        country_patent=inv_ctry)]
    ,
    merge(
      #original inv lexicon
      fread(paste(path_to_raw_downloaded_data,"lexicon_apl.csv",sep=""),
            encoding="UTF-8"),
      #merged with localization result
      fread(paste(path_to_raw_downloaded_data,"/data_preparation/localization_applicants/localization_data_",version_of_localizations,".csv",sep=""),
            encoding="UTF-8"),
      by="apl_person_id",
      all.x=TRUE)[,list(person_id=apl_person_id,address=apl_address,pair_city_region , 
                        state_local=NUTS2_equivalent_code,
                        state_patent=apl_nuts,
                        country_local=country_code_city,
                        country_patent=apl_ctry)]
    
    
  ),fill=TRUE))
  
  raw_location[,state:=state_patent]
  raw_location[is.na(state),state:=state_patent]
  raw_location[,country:=country_patent]
  raw_location[is.na(country),country:=country_patent]
  raw_location[,state_local:=NULL]
  raw_location[,state_patent:=NULL]
  raw_location[,country_local:=NULL]
  raw_location[,country_patent:=NULL]
  #unify "" and NA, which the code would treat as different values
  raw_location[address =="",address :=NA]
  raw_location[state  =="",state  :=NA]
  raw_location[country  =="",country  :=NA]
  #create location id of only location, no personal info
  setkeyv(raw_location,
          c("address","pair_city_region","state","country"))
  raw_location[,first_time_location:=seq_len(.N)==1,by=c("address","pair_city_region","state","country")]
  raw_location[,rawlocation_id:=cumsum(as.numeric(first_time_location)),]
  #store bridge to person_id
  bridge_raw_location_person <- raw_location[,list(rawlocation_id,person_id=as.character(person_id))]
  #shrink data to locations only
  raw_location[,person_id:=NULL]
  raw_location[,first_time_location:=NULL]
  raw_location <- unique(raw_location)

  raw_location <- merge(raw_location,
                        city_data,
                        by=c("pair_city_region"),
                        all.x=TRUE)
  
  #create the variables in the format expected by USPTO
  raw_location[,city :=name_municipality ]
  raw_location[!is.na(NUTS2_equivalent_code),state :=NUTS2_equivalent_code  ]
  raw_location[!is.na(country_code_city),country :=country_code_city  ]
  raw_location[!is.na(latitude),latlong:=paste(as.numeric(round(latitude,digits=4)),as.numeric(round(longitude,digits=4)),sep="|")]
  

  setkey(raw_location,rawlocation_id)
  fwrite(raw_location[,list(id=rawlocation_id,rawlocation_id,city,state,country,latlong)],
         file = paste(path_to_output_data,"rawlocation.tsv",sep=""),
         quote=TRUE,
         sep="\t")
  
  fwrite(bridge_raw_location_person,
         file = paste(path_to_output_data,"bridge_raw_location_person.csv",sep=""),
         quote=TRUE,
         sep="\t")
  
  rm(bridge_raw_location_person)
  rm(raw_location)
  gc()
  
  }
  
  

  #create patent file
  #---#---#---#---#---#
  if (file.exists(paste(path_to_output_data,"/patent.tsv",sep="")) ==FALSE ){
  
    patent<-data.table(number=numeric())
    
    if (location == "chicago_server"){
      
      patent <- fread(paste(path_to_raw_downloaded_data,"patstat_abstracts_en.csv",sep=""),
                            encoding="UTF-8")[,list(number=appln_id,abstract=appln_abstract)]
      patent <- merge(patent,
                      fread(paste(path_to_raw_downloaded_data,"patstat_titles_en.csv",sep=""),
                            encoding="UTF-8")[,list(number=appln_id,title=appln_title)],
                      by="number",
                      all.x=TRUE) 
      
    }
    
    
    patent <- merge(patent,
                    
                    unique(rbindlist(list(
                      fread(paste(path_to_raw_downloaded_data,"patents_apl.csv",sep=""),
                            encoding="UTF-8")[
                              ,list(appln_nr_epodoc,appln_id,appln_auth,appln_filing_year)],
                      fread(paste(path_to_raw_downloaded_data,"patents_inv.csv",sep=""),
                            encoding="UTF-8")[
                              ,list(appln_nr_epodoc,appln_id,appln_auth,appln_filing_year)]
                    ))[,list(
                      id = as.character(appln_nr_epodoc),
                      type="utility",
                      number=appln_id,
                      country=appln_auth,
                      date=paste(as.character(appln_filing_year),"-01-01",sep=""),
                      kind="B2",
                      num_claims=20,
                      filename="xxx.jpg",
                      withdrawn=0
                    )]),
                    by="number",
                    all.y=TRUE) 
    
    
    
    setkey(patent,id)
    fwrite(patent,file = paste(path_to_output_data,"patent.tsv",sep=""),
           quote=TRUE,
           sep="\t")
    
    
  rm(patent)
  gc()
    
  
  }
  
  
  
  
  
  
  
  #create rawinventor file
  #---#---#---#---#---#
  if (file.exists(paste(path_to_output_data,"rawinventor.tsv",sep="")) ==FALSE ){
    
    
    #create rawinventor and add variables uuid,patent_id,inventor_id,
    # name_first, canopy, nationality, sequence rule_47
    raw_inventor <- 
      merge(fread(paste(path_to_raw_downloaded_data,"patents_inv.csv",sep=""),
                encoding="UTF-8")[,sequence:=seq_len(.N),by="appln_nr_epodoc"],
          unique(fread(paste(path_to_raw_downloaded_data,"/data_preparation/inventor_cleaned_names_list.csv",sep=""),
                       encoding="UTF-8")[,list(inv_eee_hrm_id,cleaned_id,cleaned_names_alphabet,cleaned_names,firstname,lastname)]),
          by="inv_eee_hrm_id",
          all.x=TRUE)[,list(uuid = as.character(inv_person_id),
                            patent_id= appln_nr_epodoc ,
                            inventor_id=as.character(cleaned_id),
                            name_first =firstname,
                            canopy = lastname,
                            nationality = inv_ctry,
                            sequence = sequence,
                            rule_47=FALSE
                            )]
    
    raw_inventor <- merge(raw_inventor,
                          fread(paste(path_to_raw_downloaded_data,"/lexicon_inv.csv",sep=""),
                                encoding="UTF-8")[,list(uuid=as.character(inv_person_id),inv_name,inv_address)],
                          by=c("uuid"),
                          all.x=TRUE)
    
    
    raw_inventor[grepl(inv_name,pattern="deceased",fixed = TRUE),deceased:=TRUE]
    raw_inventor[grepl(inv_name,pattern="gestorben",fixed = TRUE),deceased:=TRUE]
    raw_inventor[grepl(inv_name,pattern="verstorben",fixed = TRUE),deceased:=TRUE]
    raw_inventor[grepl(inv_address,pattern="deceased",fixed = TRUE),deceased:=TRUE]
    raw_inventor[grepl(inv_address,pattern="gestorben",fixed = TRUE),deceased:=TRUE]
    raw_inventor[grepl(inv_address,pattern="verstorben",fixed = TRUE),deceased:=TRUE]
    raw_inventor[is.na(deceased),deceased:=FALSE]
    raw_inventor[,inv_name:=NULL]
    raw_inventor[,inv_address:=NULL]
    
    raw_inventor <- merge(raw_inventor,
                                  fread(paste(path_to_output_data,"/bridge_raw_location_person.csv",sep=""),
                                        encoding="UTF-8")[,list(person_id=as.character(person_id),rawlocation_id )],
                                  by.x=c("uuid"),
                                  by.y=c("person_id"),
                                  all.x=TRUE)
    
    
    raw_inventor[,canopy_size :=.N,by="canopy"]
    raw_inventor[canopy_size>40000,.N,by="canopy"]
    
    canopies_to_big <- raw_inventor[canopy_size>40000,.N,by="canopy"]
    for (count in 1:15) {
      raw_inventor[canopy_size>40000,canopy :=paste0(canopy,substr(name_first     ,count,count))]
      raw_inventor[,canopy_size :=.N,by="canopy"]
    }
    canopies_to_big <- raw_inventor[canopy_size>40000,.N,by="canopy"]
    print(canopies_to_big)
    
    
  
    setkey(raw_inventor,uuid)
    fwrite(raw_inventor[,list(
      uuid,
      patent_id,
      inventor_id,
      rawlocation_id,
      name_first,
      name_last= canopy,
      nationality,
      sequence,
      rule_47,
      deceased)],
      file = paste(path_to_output_data,"rawinventor.tsv",sep=""),
           quote=TRUE,
           sep="\t")
    rm(raw_inventor)
    gc()
    
    
  }
  
  
  
  #create rawasignee file
  #---#---#---#---#---#
  if (file.exists(paste(path_to_output_data,"rawassignee.tsv",sep="")) ==FALSE ){
    
    original_apl_lexicon <- fread(paste(path_to_raw_downloaded_data,"lexicon_apl.csv",sep=""),
                                  encoding="UTF-8"
    )
    
    
    library(stringr)
    original_apl_lexicon[,deceased:=grepl(apl_name,pattern="deceased",fixed = TRUE)]
    name_components_apl <- unique(rbindlist(list(
      original_apl_lexicon[grepl(apl_name, pattern=", ",fixed=TRUE)&(apl_eee_sector=="INDIVIDUAL"|apl_eee_sector==""),
                           list(apl_person_id,deceased,str_split_fixed(apl_name, pattern = ", ", n = 2))][,
                                                                                                          list(apl_person_id,deceased,firstname=V2,lastname=V1)]
      ,
      original_apl_lexicon[(grepl(apl_name, pattern=", ",fixed=TRUE)==FALSE)&(apl_eee_sector=="INDIVIDUAL"|apl_eee_sector==""),
                           list(apl_person_id,deceased,str_split_fixed(apl_name, pattern = " ", n = 2))][,
                                                                                                         list(apl_person_id,deceased,firstname=V1,lastname=V2)]
      ,
      original_apl_lexicon[(apl_eee_sector!="INDIVIDUAL"& apl_eee_sector!=""),
                           list(apl_person_id,deceased,organization=apl_name)]
      
      
    ),fill=TRUE))
    
    rm(original_apl_lexicon)
    raw_assignee <- merge(fread(paste(path_to_raw_downloaded_data,"patents_apl.csv",sep=""),
                              encoding="UTF-8")[,sequence:=seq_len(.N),by="appln_nr_epodoc"],
                        name_components_apl,
                        by="apl_person_id")[,list(
                          uuid = as.character(apl_person_id),
                          patent_id= appln_nr_epodoc ,
                          assignee_id =as.character(apl_eee_hrm_id),
                          name_first =firstname,
                          name_last= lastname,
                          organization=organization,
                          sequence = sequence
                          )]
    raw_assignee[!is.na(organization),type:=2]
    raw_assignee[is.na(organization),type:=4]
    
    
    
    
    raw_assignee <- merge(raw_assignee,
                          fread(paste(path_to_output_data,"/bridge_raw_location_person.csv",sep=""),
                                encoding="UTF-8")[,list(uuid=as.character(person_id),rawlocation_id )],
                          by=c("uuid"),
                          all.x=TRUE)
    
    
    
    
    
    rich2 <- fread(file = paste(path_to_raw_downloaded_data, "/amadeus_merge_remerge/", "rich2.csv", sep=""),
                   encoding="UTF-8")
    rich2[,person_id:=as.numeric(person_id)]
    raw_assignee <- merge(raw_assignee,
                          unique(rich2[,list(patstat_id ,person_id=as.character(person_id))],by="person_id"),
                          by.x="uuid",
                          by.y="person_id",
                          all.x=TRUE)
    
    
    raw_assignee[!is.na(patstat_id),assignee_id:=assignee_id[[1]],by="patstat_id"]
    
    
    setkey(raw_assignee,uuid)
    fwrite(raw_assignee[,list(
      uuid,
      patent_id,
      assignee_id,
      rawlocation_id,
      type=type,
      name_first,
      name_last,
      organization,
      sequence)],
      file = paste(path_to_output_data,"rawassignee.tsv",sep=""),
           quote=TRUE,
           sep="\t")
    
    
    rm(raw_assignee)
    
  }
  
  

}





#---files as in USPTO (sub data sets)
#---#---#---#---#---#---#---#---#---#
for (sub_data_name in c("gdr","ussr")) {
  
  
  if (file.exists(paste(path_to_output_data,"/",sub_data_name,"_relevant_data/list_patents_",sub_data_name,".csv",sep="")) ==FALSE |
      file.exists(paste(path_to_output_data,"/",sub_data_name,"_relevant_data/patent.tsv",sep="")) ==FALSE |
      file.exists(paste(path_to_output_data,"/",sub_data_name,"_relevant_data/rawinventor.tsv",sep="")) ==FALSE |
      file.exists(paste(path_to_output_data,"/",sub_data_name,"_relevant_data/rawassignee.tsv",sep="")) ==FALSE |
      file.exists(paste(path_to_output_data,"/",sub_data_name,"_relevant_data/rawlocation.tsv",sep="")) ==FALSE 
  ){
  

  #patent files
  list_patents <- fread(file = paste0(path_to_output_data,"/",sub_data_name,"_relevant_data/list_patents_",sub_data_name,".csv"),
                        encoding = "UTF-8")
  
  
  
  
  patent <- fread(paste(path_to_output_data,"/patent.tsv",sep=""),
        encoding="UTF-8")
  setkey(patent,id)
  fwrite(patent[list_patents[,list(id=appln_nr_epodoc)]],
         file = paste(path_to_output_data,"/",sub_data_name,"_relevant_data/patent.tsv",sep=""),
         quote=TRUE,
         sep="\t")
  rm(patent)
  gc()
  
  
  
  
  raw_inventor <- fread(file = paste(path_to_output_data,"rawinventor.tsv",sep=""),
                        encoding="UTF-8")
  setkey(raw_inventor,patent_id)
  fwrite(raw_inventor[list_patents[,list(id=appln_nr_epodoc)]],
         file = paste(path_to_output_data,"/",sub_data_name,"_relevant_data/rawinventor.tsv",sep=""),
         quote=TRUE,
         sep="\t")

  
  raw_assignee <- fread(file = paste(path_to_output_data,"rawassignee.tsv",sep=""),
                        encoding="UTF-8")
  setkey(raw_assignee,patent_id)
  fwrite(raw_assignee[list_patents[,list(id=appln_nr_epodoc)]],
         file = paste(path_to_output_data,"/",sub_data_name,"_relevant_data/rawassignee.tsv",sep=""),
         quote=TRUE,
         sep="\t")
  
  
  list_of_locations <- 
  unique(
    rbindlist(list(
      raw_inventor[,list(rawlocation_id)],
      raw_assignee[,list(rawlocation_id)]
    ))
  )
  rm(raw_inventor)
  rm(raw_assignee)
  gc()
  
  raw_location <- fread(file = paste(path_to_output_data,"rawlocation.tsv",sep=""),
                        encoding = "UTF-8")
  setkey(raw_location,rawlocation_id)
  fwrite(raw_location[list_of_locations],
         file = paste(path_to_output_data,"/",sub_data_name,"_relevant_data/rawlocation.tsv",sep=""),
         quote=TRUE,
         sep="\t")

  rm(raw_location)
  gc()
  }
}


if(1==2){
  
  
  
  
  original_inv_lexicon[,list(inv_person_id,inv_eee_hrm_id,deceased)]
  
  
  
  #fread epo files
  original_inv_patent <-     fread(paste(path_to_raw_downloaded_data,"patents_inv.csv",sep=""),
                                   encoding="UTF-8")
  setkey(original_inv_patent,appln_nr_epodoc)
  #fread epo files
  original_apl_patent <- fread(paste(path_to_raw_downloaded_data,"patents_apl.csv",sep=""),
                               encoding="UTF-8")
  setkey(original_apl_patent,appln_nr_epodoc)
  inventor_cleaned_names_list <- fread(paste(path_to_raw_downloaded_data,"/data_preparation/inventor_cleaned_names_list.csv",sep=""),
                                       encoding="UTF-8")
  setkey(inventor_cleaned_names_list,inv_eee_hrm_id)
  
  original_inv_lexicon <- merge(fread(paste(path_to_raw_downloaded_data,"lexicon_inv.csv",sep=""),
                                      encoding="UTF-8"),
                                fread(paste(path_to_raw_downloaded_data,"/data_preparation/localization_inventors/localization_data_",version_of_localizations,".csv",sep=""),
                                      encoding="UTF-8"),
                                by="inv_person_id",
                                all.x=TRUE)
  original_inv_lexicon <- merge(original_inv_lexicon,
                                unique(inventor_cleaned_names_list[,list(inv_eee_hrm_id, cleaned_id,cleaned_names_alphabet,cleaned_names,firstname,lastname)]),
                                by="inv_eee_hrm_id",
                                all.x=TRUE)
  original_inv_lexicon[,latlong:=paste(as.numeric(round(latitude,digits=4)),as.numeric(round(longitude,digits=4)),sep="|")]
  setkey(original_inv_lexicon,inv_person_id)
  original_apl_lexicon <- fread(paste(path_to_raw_downloaded_data,"lexicon_apl.csv",sep=""),
                                encoding="UTF-8"
  )
  original_apl_lexicon[,latlong:=""]
  setkey(original_apl_lexicon,apl_person_id)
  patstat_abstracts_en <- fread(paste(path_to_raw_downloaded_data,"patstat_abstracts_en.csv",sep=""),
                                encoding="UTF-8")
  setkey(patstat_abstracts_en,appln_id)
  patstat_titles_en <- fread(paste(path_to_raw_downloaded_data,"patstat_titles_en.csv",sep=""),
                             encoding="UTF-8")
  setkey(patstat_titles_en,appln_id)
  
  
  #create sequence
  original_inv_patent[,sequence:=seq_len(.N)-1,by="appln_nr_epodoc"]
  original_apl_patent[,sequence:=seq_len(.N)-1,by="appln_nr_epodoc"]
  
  if (debugging_run==TRUE){
    original_inv_patent <-rbindlist(list(original_inv_patent[inv_ctry=="DD"][1:1000],
                                         original_inv_patent[inv_ctry=="US"][1:1000]))
    original_apl_patent <- original_apl_patent[unique(original_inv_patent[,list(appln_nr_epodoc)])]
    list_of_inventors <- unique(original_inv_patent[,list(inv_person_id  )])
    original_inv_lexicon <- original_inv_lexicon[list_of_inventors]
    list_of_applicants<- unique(original_apl_patent[,list(apl_person_id  )])
    original_apl_lexicon <- original_apl_lexicon[list_of_applicants]
  }
  
  
  
  
  
  
  
  
  #create rawinventor
  #---#---#---#---#---#
  library(stringr)
  original_inv_lexicon[,deceased:=grepl(inv_name,pattern="deceased",fixed = TRUE)]
  name_components_inv <- merge(
    original_inv_lexicon[,list(inv_person_id,inv_eee_hrm_id,deceased)],
    inventor_cleaned_names_list,
    by="inv_eee_hrm_id")[
      ,list(inv_person_id,cleaned_id,deceased,firstname,lastname)]
  
  
  inv_patent <- merge(original_inv_patent,
                      name_components_inv,
                      by="inv_person_id")
  
  
  
  inv_location <- unique(merge(raw_location,
                               original_inv_lexicon,
                               by.y=c("inv_address","inv_nuts","inv_ctry","latlong"),
                               by.x=c("address","state","country","latlong")
  )[,list(inv_person_id,address,state,country,latlong,rawlocation_id)])
  
  
  
  inv_patent <- merge(inv_patent,
                      inv_location,
                      by="inv_person_id")
  
  
  inv_patent[,canopy :=paste0(lastname,"_",substr(firstname,1,1))]
  inv_patent[,canopy_size :=.N,by="canopy"]
  inv_patent[canopy_size>40000,.N,by="canopy_size"]
  
  canopies_to_big <- inv_patent[canopy_size>40000,.N]
  for (count in 2:10) {
    inv_patent[canopy_size>40000,canopy :=paste0(canopy,substr(firstname,count,count))]
    inv_patent[,canopy_size :=.N,by="canopy"]
  }
  canopies_to_big <- inv_patent[canopy_size>40000,.N]
  print(canopies_to_big)
  unique(inv_patent[canopy_size>40000,list(firstname,lastname,canopy,canopy_size)])
  raw_inventor <- inv_patent[,list(
    uuid = as.character(inv_person_id),
    patent_id= appln_nr_epodoc ,
    inventor_id=as.character(cleaned_id),
    rawlocation_id = rawlocation_id,
    name_first =firstname,
    name_last= canopy,
    nationality = inv_ctry,
    sequence = sequence,
    rule_47=FALSE,
    deceased=deceased
  )]
  
  
  
  
  #create rawaplentor
  #---#---#---#---#---#
  library(stringr)
  original_apl_lexicon[,deceased:=grepl(apl_name,pattern="deceased",fixed = TRUE)]
  name_components_apl <- unique(rbindlist(list(
    original_apl_lexicon[grepl(apl_name, pattern=", ",fixed=TRUE)&(apl_eee_sector=="INDIVIDUAL"|apl_eee_sector==""),
                         list(apl_person_id,deceased,str_split_fixed(apl_name, pattern = ", ", n = 2))][,
                                                                                                        list(apl_person_id,deceased,firstname=V2,lastname=V1)]
    ,
    original_apl_lexicon[(grepl(apl_name, pattern=", ",fixed=TRUE)==FALSE)&(apl_eee_sector=="INDIVIDUAL"|apl_eee_sector==""),
                         list(apl_person_id,deceased,str_split_fixed(apl_name, pattern = " ", n = 2))][,
                                                                                                       list(apl_person_id,deceased,firstname=V1,lastname=V2)]
    ,
    original_apl_lexicon[(apl_eee_sector!="INDIVIDUAL"& apl_eee_sector!=""),
                         list(apl_person_id,deceased,organization=apl_name)]
    
    
  ),fill=TRUE))
  
  
  apl_patent <- merge(original_apl_patent,
                      name_components_apl,
                      by="apl_person_id")
  
  
  
  apl_location <- unique(merge(raw_location,
                               original_apl_lexicon,
                               by.y=c("apl_address","apl_nuts","apl_ctry","latlong"),
                               by.x=c("address","state","country","latlong")
  )[,list(apl_person_id,address,state,country,latlong,rawlocation_id)])
  
  
  apl_patent <- merge(apl_patent,
                      apl_location,
                      by="apl_person_id")
  apl_patent[!is.na(organization),type:=2]
  apl_patent[is.na(organization),type:=4]
  
  raw_assignee <- apl_patent[,list(
    uuid = as.character(apl_person_id),
    patent_id= appln_nr_epodoc ,
    assignee_id =as.character(apl_person_id),
    rawlocation_id = rawlocation_id,
    type=type,
    name_first =firstname,
    name_last= lastname,
    organization=organization,
    sequence = sequence
  )]
  
  
  
  
  
  patent <- merge(patent,
                  patstat_titles_en[,list(number=appln_id,title=appln_title)],
                  by="number",
                  all.x=TRUE)
  
  patent <- merge(patent,
                  patstat_abstracts_en[,list(number=appln_id,abstract=appln_abstract)],
                  by="number",
                  all.x=TRUE) 
  
  
  
  
  
  original_inv_lexicon[,unclear_nationality:=inv_ctry==""]
  original_inv_lexicon[,.N,by="inv_ctry"][order(inv_ctry)]
  original_inv_lexicon[,unclear_nationality:=min(unclear_nationality),by="cleaned_id"]
  
  
  
  #create rawinventor
  setkey(raw_inventor,inventor_id)
  gdr_raw_inventor <- raw_inventor[list_cleaned_ids_gdr]
  setkey(gdr_raw_inventor,uuid)
  fwrite(gdr_raw_inventor,
         file = paste(path_to_output_data,"/gdr_relevant/rawinventor.tsv",sep=""),
         quote=TRUE,
         sep="\t")
  print(nrow(gdr_raw_inventor))
  print(nrow(raw_inventor))
  #create patent
  gdr_patent <- patent[unique(gdr_raw_inventor[,list(id=patent_id)])]
  fwrite(gdr_patent,
         file = paste(path_to_output_data,"/gdr_relevant/patent.tsv",sep=""),
         quote=TRUE,
         sep="\t")
  
  #create raw_assignee
  setkey(raw_assignee,patent_id)
  rawassignee_gdr <- raw_assignee[unique(gdr_patent[,list(patent_id=id)])]
  fwrite(rawassignee_gdr,
         file = paste(path_to_output_data,"/gdr_relevant/rawassignee.tsv",sep=""),
         quote=TRUE,
         sep="\t")
  
  
  #create raw_location
  setkey(raw_location,id)
  raw_location_gdr <- raw_location[unique(
    rbindlist(list(
      rawassignee_gdr[,list(id=rawlocation_id)],
      gdr_raw_inventor[,list(id=rawlocation_id)]
    ))
  )]
  fwrite(raw_location_gdr,
         file = paste(path_to_output_data,"/gdr_relevant/raw_location.tsv",sep=""),
         quote=TRUE,
         sep="\t")
  
  
  print(nrow(raw_inventor[name_last=="hiroyuki"]))
  
  
  print(nrow(gdr_raw_inventor[name_last=="hiroyuki"]))
  
  gdr_raw_inventor[,name_first:=paste(name_first,name_last)]
  gdr_raw_inventor[,name_last:=inventor_id]
  fwrite(gdr_raw_inventor,
         file=paste(path_to_output_data,"/gdr_relevant2/rawinventor.tsv",sep=""),
         quote=TRUE,
         sep="\t")
  
  
  #take cleaned_ids as 
  setkey(raw_inventor,uuid)
  raw_inventor[,name_first:=paste(name_first,name_last)]
  raw_inventor[,name_last:=inventor_id]
  fwrite(raw_inventor,file = paste(path_to_output_data,"rawinventor.tsv",sep=""),
         quote=TRUE,
         sep="\t")
  
  
  
  
  
  
  
  
  
  
  name_components_inv <- merge(
    original_inv_lexicon[,list(inv_person_id,inv_eee_hrm_id,deceased)],
    inventor_cleaned_names_list,
    by="inv_eee_hrm_id")[
      ,list(inv_person_id,cleaned_id,deceased,firstname,lastname)]
  
  
  inv_patent <- merge(original_inv_patent,
                      name_components_inv,
                      by="inv_person_id")
  
  
  
  inv_location <- unique(merge(raw_location,
                               original_inv_lexicon,
                               by.y=c("inv_address","inv_nuts","inv_ctry","latlong"),
                               by.x=c("address","state","country","latlong")
  )[,list(inv_person_id,address,state,country,latlong,rawlocation_id)])
  
  
  
  inv_patent <- merge(inv_patent,
                      inv_location,
                      by="inv_person_id")
  
  
  inv_patent[,canopy :=paste0(lastname,"_",substr(firstname,1,1))]
  inv_patent[,canopy_size :=.N,by="canopy"]
  inv_patent[canopy_size>40000,.N,by="canopy_size"]
  
}



