
#load data from patents and select gdr patents
{
  
  #load the relevant original data
  apl_lexicon <- fread(file=paste(path_to_raw_downloaded_data,"/","/lexicon_apl",".csv",sep=""),
                       encoding = "UTF-8")
  test <- apl_lexicon[31835230==apl_eee_hrm_id]
  setkey(apl_lexicon,apl_eee_hrm_id)
  
  apl_lexicon<- merge(apl_lexicon,
                      fread(file=paste(path_to_raw_downloaded_data,"/data_preparation/","/localization_applicants/localization_data_",version_of_localizations,".csv",sep=""),
                            encoding = "UTF-8"),
                      by="apl_person_id",
                      all.x=TRUE)
  setkey(apl_lexicon,apl_eee_hrm_id)
  
  original_apl_patent <- unique(fread(paste(path_to_raw_downloaded_data,"/","/patents_apl",".csv",sep=""))[
    ,list(appln_id, appln_nr_epodoc, appln_auth, appln_filing_year, docdb_family_id,
          apl_person_id, apl_han_id, apl_eee_hrm_id, apl_ctry )])
  setkey(original_apl_patent,apl_eee_hrm_id)
  
  
  
  location_patent_apl <- 
    original_apl_patent[unique(original_apl_patent[appln_auth=="DE"| apl_ctry=="DE",list(apl_eee_hrm_id)])
                        ,list(DE=sum(as.numeric(appln_auth=="DE")),
                              DD=sum(as.numeric(appln_auth=="DD")),
                              other=sum(as.numeric(appln_auth!="DE"&appln_auth!="DD"))),by="apl_eee_hrm_id"]
  
 
  location_patent_apl[,share_patent_files_frg:=DE/(DD+DE+other)]
  location_patent_apl[,rank_gdr_patents:=frankv(x=location_patent_apl,cols ="DE",ties.method = "random",order=c(-1))]
  
  library(ggplot2)
  ggplot(data=location_patent_apl[share_patent_files_frg>0],
         aes(x=share_patent_files_frg)) + geom_histogram()
  
  
  
  top_100_gdr_patenters <- apl_lexicon[location_patent_apl[rank_gdr_patents<100,list(apl_eee_hrm_id)]]
  top_100_gdr_patenters <- merge(top_100_gdr_patenters,
                                 location_patent_apl,
                                 by="apl_eee_hrm_id")
  top_100_gdr_patenters <- top_100_gdr_patenters[top_100_gdr_patenters[, .I[1], by = apl_eee_hrm_id]$V1]
  top_100_gdr_patenters[,list(apl_eee_hrm_id,apl_name,apl_eee_sector,DE,rank_gdr_patents)][order(rank_gdr_patents)]
  # decide which ids from the whole data
  # could be GDR plants in the first place
  
  #select ids in patent data
  ids_of_relevance <-location_patent_apl[DE!=0,list(apl_eee_hrm_id)]
  ids_of_special_relevance <-location_patent_apl[share_patent_files_frg>=0.5,list(apl_eee_hrm_id)]
  ids_of_likely_foreign_apls <-location_patent_apl[share_patent_files_frg<0.5 & DE!=0,list(apl_eee_hrm_id)]
  
  
  #select the names corresponding to these ids
  names_to_match_potentially <- unique(apl_lexicon[ids_of_relevance])
  print(paste("potential apl names with any gdr patent:",nrow(names_to_match_potentially)))
  print(paste("    of these, likely foreign:",nrow(unique(apl_lexicon[ids_of_likely_foreign_apls]))))
  names_to_match_potentially <- unique(apl_lexicon[ids_of_special_relevance])
  print(paste("apl names with patent majority in GDR:",nrow(names_to_match_potentially)))
  names_to_match_potentially_state <- apl_lexicon[ids_of_special_relevance][apl_eee_sector=="HOSPITAL"|apl_eee_sector=="GOV NON-PROFIT"|apl_eee_sector=="GOV NON-PROFIT UNIVERSITY"|apl_eee_sector=="UNIVERSITY"]
  print(paste("    of these, state branches:",nrow(names_to_match_potentially_state)))
  names_to_match_potentially_ind <- apl_lexicon[ids_of_special_relevance][apl_eee_sector=="INDIVIDUAL"]
  print(paste("    of these, individuals:",nrow(names_to_match_potentially_ind)))
  names_to_match_potentially_comp <- apl_lexicon[ids_of_special_relevance][apl_eee_sector=="COMPANY"]
  print(paste("    of these, companies:",nrow(names_to_match_potentially_comp)))
  print(paste("    of these, unknown:",nrow(apl_lexicon[ids_of_special_relevance][apl_eee_sector=="UNKNOWN"])))
  
  
  
  names_to_match_strict_def <- apl_lexicon[ids_of_special_relevance][apl_eee_sector=="COMPANY"|apl_eee_sector=="UNKNOWN"]
  print(paste("    of these, selected (companies+unknown):",nrow(names_to_match_strict_def)))
  
  
  #select the data covered by these ids
  original_apl_patent[,merge_type:="1discarded"]
  original_apl_patent[ids_of_relevance,merge_type:="2potentially_relevant"]
  original_apl_patent[ids_of_likely_foreign_apls,merge_type:="1discarded bc foreign"]
  original_apl_patent[ids_of_special_relevance,merge_type:="3tried_to_merge"]
  original_apl_patent[names_to_match_potentially_state,merge_type:="1discarded bc state"]
  original_apl_patent[names_to_match_potentially_ind,merge_type:="1discarded bc individual"]
  
  
  apl_lexicon[ids_of_relevance][
    !names_to_match_potentially_state][
      !names_to_match_potentially_ind][
        !names_to_match_strict_def][!ids_of_likely_foreign_apls]
  
  
  original_apl_patent[appln_auth=="DE" & appln_filing_year>=1980 & appln_filing_year<=1990,.N,by="merge_type"]
  ggplot(data=original_apl_patent[appln_auth=="DE" & appln_filing_year>=1980 & appln_filing_year<=1990,
                                  list(nr_inventions=uniqueN(appln_nr_epodoc )),
                                  by=c("appln_filing_year","merge_type")][order(appln_filing_year)],
         aes(x=appln_filing_year,y=nr_inventions,fill=merge_type)) + geom_col(position = "stack",alpha=0.6) + theme_classic() +
    scale_fill_manual(values=c("darkblue","steelblue","cyan","red","yellow","green","darkgreen"))
  
  
  
  
  setkey(names_to_match_strict_def,apl_person_id)
  # 
  # time <- proc.time()
  
  city_data <- fread(file=paste(path_tools_data,"/localities_data/city_data_",version_of_localizations,".csv",sep=""),
                     encoding = "UTF-8")
  
  names_to_match_strict_def <- merge(names_to_match_strict_def,
                                     city_data[,list(pair_city_region,city_name,NUTS2_equivalent, NUTS2_equivalent_code,county)],
                                     by="pair_city_region",
                                     all.x=TRUE)
  
  
  
  names_to_match_strict_def[,pure_name:=gsub(pattern="veb",replacement="",x=apl_name)]
  names_to_match_strict_def[,pure_name:=gsub(pattern="kombinat",replacement="",x=pure_name)]
  names_to_match_strict_def[,pure_name:=gsub(pattern="gmbh",replacement="",x=pure_name)]
  names_to_match_strict_def[,pure_name:=gsub(pattern="ag",replacement="",x=pure_name)]
  names_to_match_strict_def[,pure_name:=gsub(pattern="gesellschaft",replacement="ges",x=pure_name)]
  clean_string_for_city_matching(names_to_match_strict_def,"pure_name")
  #names_to_match_strict_def[,pure_name:=gsub(pattern=" ",replacement="",x=pure_name)]
  
  names_to_match_strict_def["veb berlin chemie"==apl_name,list(apl_name,pure_name)]
  
  
  names_to_match_strict_def <- names_to_match_strict_def[,list(pure_name,apl_eee_hrm_id,
                                                               country_code_city,
                                                               county_code=as.numeric(county_code),
                                                               county)]
  
  names_to_match_strict_def <- merge(names_to_match_strict_def,
                                     location_patent_apl,
                                     by="apl_eee_hrm_id",
                                     all.x=TRUE)
  
  names_to_match_strict_def <- unique(names_to_match_strict_def)
  unique(apl_lexicon[unique(names_to_match_strict_def[rank_gdr_patents<=10,list(apl_eee_hrm_id)]),list(apl_eee_hrm_id,county_code )])
  names_to_match_strict_def[rank_gdr_patents<=10]
  location_patent_apl[rank_gdr_patents<=10]
}
