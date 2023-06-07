#--------------------------
#create inventor communities
#--------------------------
print("06: create communities variable for IDs")
if (inventor_statistics=="YES"){
  
  #---#---#---#---#---#---#---#---#
  #Read in disambiguation result
  #---#---#---#---#---#---#---#---#
  {
    if (file.exists(paste(path_to_output_data,"/data_preparation/",data_name,"/PatentsView disambiguation/inventors/PatentsView_identifiers.csv",sep=""))==TRUE){
      PatentsView_identifiers <- fread(paste(path_to_output_data,"/data_preparation/",data_name,"/PatentsView disambiguation/inventors/PatentsView_identifiers.csv",sep=""),
                                       encoding="UTF-8"
      )
    }else{
      if (file.exists(paste(path_to_output_data,"/data_preparation/","/PatentsView disambiguation/inventors/PatentsView_identifiers.csv",sep=""))==TRUE){
        PatentsView_identifiers <- fread(paste(path_to_output_data,"/data_preparation/","/PatentsView disambiguation/inventors/PatentsView_identifiers.csv",sep=""),
                                         encoding="UTF-8"
        )
      }
    }
    if (exists("PatentsView_identifiers")==TRUE){
      setnames(PatentsView_identifiers,
               old=c("old_id"),
               new=c("inv_person_id"))
      setkey(PatentsView_identifiers,inv_person_id)
      PatentsView_identifiers[,inv_person_id:=as.numeric(inv_person_id)]
    }
    if (exists("PatentsView_identifiers")==FALSE){
      PatentsView_identifiers <- fread(file=paste(path_to_output_data,"/",data_name,"/list_cleaned_ids_",data_name_short,".csv",sep=""),
                                       encoding="UTF-8"
      )
    }
  }
  #---#---#---#---#---#---#---#---#---#---#---#---#---#---#
  #Connect disambiguation result to patents and patent families
  #---#---#---#---#---#---#---#---#---#---#---#---#---#---#
  {
    inventors_of_patents <- unique(fread(file = paste(path_to_raw_downloaded_data, "patents_inv",".csv", sep=""),
                                         encoding="UTF-8")[,list(inv_person_id,year = min(appln_filing_year)), by="docdb_family_id"])
    
    if (data_name!=""){
      
      inventors_of_patents <- merge(
        inventors_of_patents,
        unique(fread(file=paste(path_to_output_data,"//",data_name,"/list_cleaned_ids_",data_name_short,".csv",sep=""))[,
                                                                                                                        list(inv_person_id)]),
        by="inv_person_id"
      )
    }
    inventors_of_patents <- merge(
      PatentsView_identifiers,
      inventors_of_patents,
      by=c("inv_person_id"))
    inventors_of_patents[,team_size_patent:=.N, by="docdb_family_id"]
    
    inventors_of_patents[,nr_of_patents_inventor:=.N, by=id_variable_to_use]
    
    length(unique(inventors_of_patents[,docdb_family_id]))
    length(unique(inventors_of_patents[team_size_patent==1,docdb_family_id]))
    
    
    
    
  }
  #---#---#---#---#---#---#---#
  #Load patents' IPC classes
  #---#---#---#---#---#---#---#
  {
    #patent level
    ipc_classes_for_patent_families  <- unique(merge(
      fread(file = paste(path_to_raw_downloaded_data,"ipc.csv", sep="")),
      unique(inventors_of_patents[,list(docdb_family_id)]),
      by="docdb_family_id")
    )
    
    
    ipc_classes_for_patent_families[,ipc_class_symbol :=gsub(" ","",ipc_class_symbol)]
    ipc_classes_for_patent_families[is.na(ipc_class_symbol),ipc_class_symbol:=""]
    if (node_variable=="ipc_class_symbol_4"){
      ipc_classes_for_patent_families[,ipc_class_symbol :=gsub("\\d*/.*","",ipc_class_symbol)]
      ipc_classes_for_patent_families[,ipc_class_symbol :=gsub(" ","",ipc_class_symbol)]
    }
    #patent family level (any patent class somewhere in 
    #the patent family counts for all inventors) par-
    #ticipating in any patent of the family
    ipc_classes_for_patent_families <-unique(ipc_classes_for_patent_families[,list(docdb_family_id,ipc_class_symbol)])
    
  }
  #---#---#---#---#---#---#---#---#---#---#---#---#---#---#
  #Connect inventors to ipc classes via patent families
  #---#---#---#---#---#---#---#---#---#---#---#---#---#---#
  {
    ipc_classes_per_patent_inventor <- merge(inventors_of_patents,
                                             ipc_classes_for_patent_families,
                                             by=c("docdb_family_id"),
                                             all.x=TRUE,
                                             allow.cartesian=TRUE)
    
    setkeyv(ipc_classes_per_patent_inventor, 
            c(id_variable_to_use,"year"))
  }

#-----------------------------------------------------
#Connect inventors to ipc classes via patent families
#----------------------------------------------------


communities_ipc_classes <- fread(file = paste(path_to_output_data,"/",data_name,"/data_preparation/", "communities_ipc_classes.csv", sep=""))
  inventors_of_patents

ipc_classes_per_patent_inventor <- merge(ipc_classes_per_patent_inventor,
                                         communities_ipc_classes,
                                         by.x=c("ipc_class_symbol"),
                                         by.y=c("ipc_class_symbol_4"),
                                         all.x=TRUE,
                                         allow.cartesian=TRUE)



ipc_classes_per_patent_inventor[#!is.na(community)
  ,nr_inventions  :=uniqueN(docdb_family_id),by=c(id_variable_to_use)]


#-------------------------------------------
#decide on inventor communities per inventor
#-------------------------------------------
community_decision_inv <- ipc_classes_per_patent_inventor[!is.na(community),list(minus_N=-.N),by=c(id_variable_to_use,"community")]
setkeyv(community_decision_inv,
        c(id_variable_to_use,"minus_N"))
community_decision_inv[,n:=seq_len(.N),by=c(id_variable_to_use)]
community_decision_inv[,share_most_common_community:=sum(-minus_N*(n<=1))/sum(-minus_N),by=c(id_variable_to_use)]
community_decision_inv[,share_top_two_communities:=sum(-minus_N*(n<=2))/sum(-minus_N),by=c(id_variable_to_use)]
community_decision_inv <- community_decision_inv[n==1,c(id_variable_to_use,"community","share_most_common_community","share_top_two_communities"),with=FALSE]
setkeyv(community_decision_inv,
       c(id_variable_to_use))

#  
# 
# ipc_classes4_decision_inv <- ipc_classes_per_patent_inventor[!is.na(community),list(minus_N=-.N),by=c(id_variable_to_use,"ipc_class_symbol_4")]
# setkeyv(ipc_classes4_decision_inv,
#         c(id_variable_to_use,"minus_N"))
# ipc_classes4_decision_inv[,n:=seq_len(.N),by=c(id_variable_to_use)]
# ipc_classes4_decision_inv[,nr_ipc_classes_4 :=uniqueN(ipc_class_symbol_4),by=c(id_variable_to_use)] 
# ipc_classes4_decision_inv[,prevalence_most_common_ipc_class_4 :=-minus_N]
# ipc_classes4_decision_inv <- ipc_classes4_decision_inv[n==1,c(id_variable_to_use,"ipc_class_symbol_4","nr_ipc_classes_4","prevalence_most_common_ipc_class_4"),with=FALSE]
# setkeyv(ipc_classes4_decision_inv,
#         c(id_variable_to_use))
# setnames(ipc_classes4_decision_inv,
#         "ipc_class_symbol_4",
#         "most_common_ipc_class_4")
#  
# ipc_classes_decision_inv <- ipc_classes_per_patent_inventor[!is.na(community),list(minus_N=-.N),by=c(id_variable_to_use,"ipc_class_symbol_7")]
# setkeyv(ipc_classes_decision_inv,
#         c(id_variable_to_use,"minus_N"))
# ipc_classes_decision_inv[,n:=seq_len(.N),by=c(id_variable_to_use)]
# ipc_classes_decision_inv <- ipc_classes_decision_inv[n==1,c(id_variable_to_use,"ipc_class_symbol_7"),with=FALSE]
# setkeyv(ipc_classes_decision_inv,
#         c(id_variable_to_use))
# setnames(ipc_classes_decision_inv,
#          "ipc_class_symbol_7",
#          "most_common_ipc_class_7")
# 

#-------------------------------------------
#decide on inventor communities by coauthors
#-------------------------------------------


#collect all authors of interest
setkeyv(ipc_classes_per_patent_inventor,
        id_variable_to_use)
authors <- unique(ipc_classes_per_patent_inventor[!community_decision_inv[,c(id_variable_to_use),with=FALSE],c(id_variable_to_use,"docdb_family_id"),with=FALSE])
setnames(authors,
         id_variable_to_use,
         "author")


#merge with inventions data to get coauthors
coauthors <- merge(
  authors,
  unique(ipc_classes_per_patent_inventor[,c(id_variable_to_use,"docdb_family_id"),with=FALSE]),
  by="docdb_family_id",
  allow.cartesian = TRUE)[get(id_variable_to_use)!=author]

#merge with coauthors ipc classes
coauthors <- merge(coauthors,
                   community_decision_inv,
                   by=c(id_variable_to_use),
                   allow.cartesian = TRUE)
setnames(coauthors,
         id_variable_to_use,
         "coauthor")
setnames(coauthors,
         "author",
         id_variable_to_use)


community_decision_coauthors_inv <- coauthors[!is.na(community),list(minus_N=-.N),by=c(id_variable_to_use,"community")]
setkeyv(community_decision_coauthors_inv,
        c(id_variable_to_use,"minus_N"))
community_decision_coauthors_inv[,n:=seq_len(.N),by=c(id_variable_to_use)]
community_decision_coauthors_inv[,share_most_common_community:=sum(-minus_N*(n<=1))/sum(-minus_N),by=c(id_variable_to_use)]
hist(community_decision_coauthors_inv[,round(share_most_common_community,digits=2)])
community_decision_coauthors_inv <- community_decision_coauthors_inv[n==1,c(id_variable_to_use,"community"),with=FALSE]
setkeyv(community_decision_coauthors_inv,
        c(id_variable_to_use))
setnames(community_decision_coauthors_inv,
         "community",
         "community_coauthors")

# 
# 
# #-------------------------------------------
# #decide on inventor communities per applicant
# #-------------------------------------------
# 
# 
# ipc_classes_of_applicants <- merge(ipc_classes_for_patent_families,
#                               fread(file = paste(path_to_raw_downloaded_data, "patents_apl",".csv", sep=""))[,list(apl_eee_hrm_id,apl_person_id, year = min(appln_filing_year)), by="docdb_family_id"],
#                               by=c("docdb_family_id"),
#                               allow.cartesian = TRUE,
#                               all.y=TRUE)
# 
# 
# if (data_name!=""){
#   
#   ipc_classes_of_applicants <- merge(
#     ipc_classes_of_applicants,
#     unique(fread(file=paste(path_to_output_data,"//",data_name,"/list_applicants_",data_name_short,".csv",sep=""))[,
#                                                                                                                     list(apl_person_id=person_id)]),
#     by="apl_person_id"
#   )
# }
# 
# ipc_classes_of_applicants[,ipc_class_symbol_4 :=gsub("\\d*/.*","",ipc_class_symbol)]
# ipc_classes_of_applicants[,ipc_class_symbol_4 :=gsub(" ","",ipc_class_symbol_4)]
# ipc_classes_of_applicants <- unique(ipc_classes_of_applicants[,list(ipc_class_symbol_4)])
# ipc_classes_of_applicants <- merge(ipc_classes_of_applicants,
#                                          communities_ipc_classes,
#                                          by=c("ipc_class_symbol_4"))
# 
# ipc_classes_of_applicants[apl_eee_hrm_id ==1]
# 
# community_decision_apl <- ipc_classes_of_applicants[!is.na(community),list(minus_N=-.N),by=c("apl_eee_hrm_id","community")]
# setkeyv(community_decision_apl,
#         c("apl_eee_hrm_id","minus_N"))
# community_decision_apl[,n:=seq_len(.N),by=c("apl_eee_hrm_id")]
# community_decision_apl <- community_decision_apl[n==1,c("apl_eee_hrm_id","community"),with=FALSE]
# setkeyv(community_decision_apl,
#         c("apl_eee_hrm_id"))
# setnames(community_decision_apl,
#          "community",
#          "community_apl")
# 
# ipc_classes_of_applicants <- merge(ipc_classes_of_applicants,
#       community_decision_apl,
#       by="apl_eee_hrm_id",
#       all.x=TRUE)
# 
# 
# 
# decision_apl_communities <- merge(
#       unique(ipc_classes_of_applicants[,list(docdb_family_id,apl_eee_hrm_id,community_apl)]),
#       ipc_classes_per_patent_inventor,
#       by="docdb_family_id",
#       allow.cartesian=TRUE)
# 
# 
# imputation_communities_apl <- decision_apl_communities[!is.na(community_apl),list(minus_N=-.N),by=c(id_variable_to_use,"community_apl")]
# setkeyv(imputation_communities_apl,
#         c(id_variable_to_use,"minus_N"))
# imputation_communities_apl[,n:=seq_len(.N),by=c(id_variable_to_use)]
# imputation_communities_apl <- imputation_communities_apl[n==1,c(id_variable_to_use,"community_apl"),with=FALSE]
# setkeyv(imputation_communities_apl,
#         c(id_variable_to_use))
# 
# 
# 
# 



#-------------------------------
#gather all inventor level data
#-------------------------------


inventor_technologies <- unique(ipc_classes_per_patent_inventor[,c(id_variable_to_use,"nr_inventions"),with=FALSE])
setkeyv(inventor_technologies,
        id_variable_to_use)
inventor_technologies <- merge(inventor_technologies,
                               community_decision_inv,
                              by=c(id_variable_to_use),
                              all = TRUE)

# inventor_technologies <- merge(inventor_technologies,
#                                ipc_classes4_decision_inv,
#                                by=c(id_variable_to_use),
#                                all = TRUE)

# inventor_technologies <- merge(inventor_technologies,
#                               ipc_classes_decision_inv,
#                               by=c(id_variable_to_use),
#                               all = TRUE)
#                               
inventor_technologies <- merge(inventor_technologies,
                               community_decision_coauthors_inv,
                               by=c(id_variable_to_use),
                               all = TRUE)

# inventor_technologies <- merge(inventor_technologies,
#                               imputation_communities_apl,
#                               by=c(id_variable_to_use),
#                               all = TRUE)

inventor_technologies[,.N,by="community"]


inventor_technologies[,imputed_community:=NULL]
inventor_technologies[!is.na(community),imputed_community:="no"]



inventor_technologies[is.na(community) & !is.na(community_coauthors),imputed_community:="coauthors"]
inventor_technologies[imputed_community=="coauthors",community :=community_coauthors ]
# inventor_technologies[is.na(community) & !is.na(community_apl),imputed_community:="apl"]
# inventor_technologies[imputed_community=="apl",community :=community_apl ]
inventor_technologies[is.na(community) ,imputed_community:="no_ipc_classes"]
inventor_technologies[imputed_community=="no_ipc_classes",community :=0 ]


print(inventor_technologies[,.N,by="imputed_community"][order(N)])
print(inventor_technologies[,.N,by="community"][order(N)])
imputation_decision <- unique(inventor_technologies[imputed_community!="no",.N,by=c("community","imputed_community")])[,list(community,imputed_community,N,share=round(N/sum(N),digits=4))]


fwrite(inventor_technologies,
       file=paste(path_to_output_data,"/",data_name,"/data_preparation/community_statistics_per_inventor",".csv", sep="")) 


community_overview <- inventor_technologies[,.N,by="community"][order(N)]
community_overview [,share:=N/sum(N)]
community_overview 
}