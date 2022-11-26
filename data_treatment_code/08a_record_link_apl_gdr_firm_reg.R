#install.packages("RecordLinkage")
#install.packages("stringdist")
print("08a: Link applicants to registry")
suppressWarnings({
  dir.create(paste0(path_to_output_data,"/link_gdr_registry/"))
})
library("data.table")
library("ggplot2")

if (link_to_registry == TRUE) {

{
  #load the relevant original data
  apl_lexicon <- fread(file=paste(path_to_raw_downloaded_data,"/data_preparation/",data_name,"/lexicon_apl",".csv",sep=""),
                       encoding = "UTF-8")
  test <- apl_lexicon[31835230==apl_eee_hrm_id]
  setkey(apl_lexicon,apl_eee_hrm_id)
  
  apl_lexicon<- merge(apl_lexicon,
                      fread(file=paste(path_to_raw_downloaded_data,"/data_preparation/","/localization_applicants/localization_data.csv",sep=""),
                            encoding = "UTF-8"),
                      by="apl_person_id",
                      all.x=TRUE)
  setkey(apl_lexicon,apl_eee_hrm_id)
  
  original_apl_patent <- fread(paste(path_to_raw_downloaded_data,"/data_preparation/",data_name,"/raw_data_apl_patent",".csv",sep=""))
  setkey(original_apl_patent,apl_eee_hrm_id)
  
  
  #chracterize the apl in terms of where they do their patenting
  #specifically, the share of patents in East Germany
  original_apl_patent[appln_auth=="DD",patenting_loc:="DD"]
  original_apl_patent[appln_auth=="DE",patenting_loc:="DE"]
  original_apl_patent[appln_auth!="DE"&appln_auth!="DD",patenting_loc:="other"]
  location_patent_apl <- dcast(
    original_apl_patent[unique(original_apl_patent[appln_auth=="DD"| apl_ctry=="DD",list(apl_eee_hrm_id)]),
                        list(nr_inventions=uniqueN(docdb_family_id)),
                        by=c("apl_eee_hrm_id","patenting_loc")]
    , apl_eee_hrm_id  ~ patenting_loc , value.var = "nr_inventions")
  location_patent_apl[is.na(other),other:=0]
  location_patent_apl[is.na(DD),DD:=0]
  location_patent_apl[is.na(DE),DE:=0]
  location_patent_apl[,share_patent_files_gdr:=DD/(DD+DE+other)]
  location_patent_apl[,rank_gdr_patents:=frankv(x=location_patent_apl,cols ="DD",ties.method = "random",order=c(-1))]
  
  ggplot(data=location_patent_apl[share_patent_files_gdr>0],
         aes(x=share_patent_files_gdr)) + geom_histogram()
  
  
  
  top_100_gdr_patenters <- apl_lexicon[location_patent_apl[rank_gdr_patents<100,list(apl_eee_hrm_id)]]
  top_100_gdr_patenters <- merge(top_100_gdr_patenters,
                                 location_patent_apl,
                                 by="apl_eee_hrm_id")
  top_100_gdr_patenters <- top_100_gdr_patenters[top_100_gdr_patenters[, .I[1], by = apl_eee_hrm_id]$V1]
  top_100_gdr_patenters[,list(apl_eee_hrm_id,apl_name,apl_eee_sector,DD,rank_gdr_patents)][order(rank_gdr_patents)]
  # decide which ids from the whole data
  # could be GDR plants in the first place
  
  #select ids in patent data
  ids_of_relevance <-location_patent_apl[DD!=0,list(apl_eee_hrm_id)]
  ids_of_special_relevance <-location_patent_apl[share_patent_files_gdr>=0.5,list(apl_eee_hrm_id)]
  ids_of_likely_foreign_apls <-location_patent_apl[share_patent_files_gdr<0.5 & DD!=0,list(apl_eee_hrm_id)]
  
  
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
  
  
  
  original_apl_patent[patenting_loc=="DD" & appln_filing_year>=1980 & appln_filing_year<=1990,.N,by="merge_type"]
  ggplot(data=original_apl_patent[patenting_loc=="DD" & appln_filing_year>=1980 & appln_filing_year<=1990,
                                  list(nr_inventions=uniqueN(appln_nr_epodoc )),
                                  by=c("appln_filing_year","merge_type")][order(appln_filing_year)],
         aes(x=appln_filing_year,y=nr_inventions,fill=merge_type)) + geom_col(position = "stack",alpha=0.6) + theme_classic() +
    scale_fill_manual(values=c("darkblue","steelblue","cyan","red","yellow","green","darkgreen"))
  
  
  
  
  setkey(names_to_match_strict_def,apl_person_id)
  # 
  # time <- proc.time()
  # 
  # localize_strings(data_table = names_to_match_strict_def[1:100],
  #                  vector_of_names_of_variables_to_localize = c("apl_address","apl_name"),
  #                  vector_variables_country_codes = c("apl_ctry"),
  #                  id_variables = c("apl_person_id"),
  #                  extract_clean_names_from =c("apl_name"),
  #                  threshold_nr_words_to_consider = 0,
  #                  extract_country_codes="no",
  #                  extract_firstname_info="no",
  #                  extract_city_localization="YES",
  #                  threshold_acceptance=0.5
  # )
  # time_100_matches <- proc.time() - time
  # 
  # 
  # print(paste("Localization of gdr applicants started", format(Sys.time(), "%b %d %X %Y"), "Expected minutes of runtime:",round((time_100_matches[3]*nrow(names_to_match_strict_def)/100)/60,digits=2)))
  # 
  # 
  # 
  # localized_apl_data <- localize_strings(data_table = names_to_match_strict_def,
  #                                        vector_of_names_of_variables_to_localize = c("apl_address","apl_name"),
  #                                        vector_variables_country_codes = c("apl_ctry"),
  #                                        id_variables = c("apl_person_id"),
  #                                        extract_clean_names_from =c("apl_name"),
  #                                        threshold_nr_words_to_consider = 0,
  #                                        extract_country_codes="no",
  #                                        extract_firstname_info="no",
  #                                        extract_city_localization="YES",
  #                                        threshold_acceptance=0.5
  # )
  # 
  # localized_apl_data[,list(apl_person_id,city_name,dummy_ex_gdr,meta_info_city_match,county,cleaned_name)]
  # 
  # names_to_match_strict_def <- merge(names_to_match_strict_def,
  #                                    localized_apl_data,
  #                                    by="apl_person_id",
  #                                    all.x=TRUE)
  # 
  # 
  city_data <- fread(file=paste(path_tools_data,"/localities_data/city_data.csv",sep=""),
                     encoding = "UTF-8")
  
  names_to_match_strict_def <- merge(names_to_match_strict_def,
                                     city_data[,list(pair_city_region,city_name,NUTS2_equivalent, NUTS2_equivalent_code,county)],
                                     by="pair_city_region",
                                     all.x=TRUE)
}

names_to_match_strict_def[,pure_name:=gsub(pattern="veb",replacement="",x=apl_name)]
names_to_match_strict_def[,pure_name:=gsub(pattern="kombinat",replacement="",x=pure_name)]
names_to_match_strict_def[,pure_name:=gsub(pattern="gmbh",replacement="",x=pure_name)]
names_to_match_strict_def[,pure_name:=gsub(pattern="ag",replacement="",x=pure_name)]
names_to_match_strict_def[,pure_name:=gsub(pattern="gesellschaft",replacement="ges",x=pure_name)]
clean_string_for_city_matching(names_to_match_strict_def,"pure_name")
names_to_match_strict_def[,pure_name:=gsub(pattern=" ",replacement="",x=pure_name)]

names_to_match_strict_def["veb berlin chemie"==apl_name,list(apl_name,pure_name)]
# load the data sbr_id data with which to merge
# alternative: VEB Betriebsliste, but does not 
# contain sbr_id, so where would that leave us?
{
  library(openxlsx)
  gdr_registry_names <-as.data.table(read.xlsx(paste(path_tha_project_data,"/orig/gdr_firm_data/DE 2 MD 083/04_CODELISTEN/BetriebsNr und -kurztext.xlsx",sep="")))
  
  setnames(gdr_registry_names,
           old=colnames(gdr_registry_names),
           new=c("year","sbr_id","short_name"))
  gdr_registry_names <- gdr_registry_names[gdr_registry_names[, .I[1], by = short_name]$V1]
  
  
  #import gdr registry data
  #---#---#---#---#---#---#
  
  gdr_registry_data <- fread(paste(path_tha_project_data,"/final/gdr_firm_data/complete_DE_2_MD_083_data.csv",sep=""),
                             encoding = "UTF-8")
  setkey(gdr_registry_data,year,sbr_id)
  gdr_registry_data[,typeof(sbr_id)]
  gdr_registry_data[,N:=.N,by=c("sbr_id","year")]
  gdr_registry_data[sbr_id==4005006,]
  print(nrow(gdr_registry_data[N>1]))
  gdr_registry_data[,n:=seq_len(.N),by=c("sbr_id","year")]
  gdr_registry_data <- gdr_registry_data[n==N]
  gdr_registry_data[,c("n","N"):=NULL]
  gdr_registry_data[,gdr_sector:=substr(economic_group,1,2)]
  #create county codes
  gdr_registry_data[,district_code:=as.character(district_code)]
  gdr_registry_data[nchar(district_code)==1,district_code:=paste(0,district_code,sep="")]
  
  gdr_registry_data[,only_county_code:=as.character(only_county_code)]
  gdr_registry_data[nchar(only_county_code)==1,only_county_code:=paste(0,only_county_code,sep="")]
  
  gdr_registry_data[,municipality_code:=as.character(municipality_code)]
  gdr_registry_data[nchar(municipality_code)==1,municipality_code:=paste(0,municipality_code,sep="")]
  
  gdr_registry_data[,gdr_county_code:=paste(district_code,only_county_code,municipality_code,sep="")]
  gdr_registry_data[nchar(gdr_county_code)!=6]
  
  
  
  gdr_registry_names <- merge(gdr_registry_names,
                              gdr_registry_data[,list(sbr_id,plant_name_anon ,gdr_county_code=as.numeric(gdr_county_code)),],
                              by.x=c("short_name"),
                              by.y=c("plant_name_anon"),
                              all=TRUE)
  gdr_registry_names[,sbr_id:=sbr_id.x]
  gdr_registry_names[is.na(sbr_id),sbr_id:=sbr_id.y]
  gdr_registry_names[,num_sbr_id:=as.numeric(sbr_id)]
  
  
  #merge the county met-info to the names
  gdr_registry_names <- merge(gdr_registry_names,
                              fread(file=paste(path_tha_project_data,"/temp/gdr_firm_data/county_codes_gdr_with_frg_city.csv",sep="")),
                              by="gdr_county_code",
                              all.x = TRUE)
  gdr_registry_names[!is.na(latitude )]
  
  
  gdr_registry_names <- unique(gdr_registry_names[,list(gdr_county_code,short_name, sbr_id,gdr_county_name, city_name, county, latitude, longitude)])
  
  
}




gdr_registry_names[,pure_name_reg:=tolower(short_name)]
gdr_registry_names[,pure_name_reg:=gsub(pattern="veb",replacement="",x=pure_name_reg)]
gdr_registry_names[,pure_name_reg:=gsub(pattern="kombinat",replacement="",x=pure_name_reg)]
gdr_registry_names[,pure_name_reg:=gsub(pattern="gmbh",replacement="",x=pure_name_reg)]
gdr_registry_names[,pure_name_reg:=gsub(pattern="gesellschaft",replacement="ges",x=pure_name_reg)]
gdr_registry_names[,pure_name_reg:=gsub(pattern="ag",replacement="",x=pure_name_reg)]
clean_string_for_city_matching(gdr_registry_names,"pure_name_reg")
gdr_registry_names[,pure_name_reg:=gsub(pattern=" ",replacement="",x=pure_name_reg)]





data_set_1 = unique(names_to_match_strict_def[,list(pure_name,
                                                    county)])
colnames(data_set_1)
data_set_2 = unique(gdr_registry_names[,list(pure_name_reg,county)])
colnames(data_set_2)
varname_string_data_1 = "pure_name"
varname_string_data_2 = "pure_name_reg"
varname_string_block_var_both ="county"
block_value=data.table(county="Chemnitz Stadt")
block_var=varname_string_block_var_both

print(typeof(data_set_1[,county]))
print(typeof(data_set_2[,county]))


match_result <-   fuzzy_name_match(data_set_1=names_to_match_strict_def,
                                   data_set_2=gdr_registry_names,
                                   varname_string_data_1="pure_name",
                                   varname_id_data_1="apl_eee_hrm_id",
                                   varname_string_data_2="pure_name_reg",
                                   varname_id_data_2="sbr_id",
                                   varname_string_block_var_both)


match_result[,rank_fit := seq_len(.N),by="apl_eee_hrm_id"]
match_result[,nr_fits := .N,by=c("criterium","apl_eee_hrm_id")]



original_apl_patent[unique(match_result[merge_type=="4low"&rank_fit==1 & nr_fits==1,list(apl_eee_hrm_id)]),merge_type:="4low"]
original_apl_patent[unique(match_result[merge_type=="5middle"&rank_fit==1 & nr_fits==1,list(apl_eee_hrm_id)]),merge_type:="5middle"]
original_apl_patent[unique(match_result[merge_type=="6high"&rank_fit==1 & nr_fits==1,list(apl_eee_hrm_id)]),merge_type:="6high"]




ggplot(data=original_apl_patent[patenting_loc=="DD" & appln_filing_year>=1980 & appln_filing_year<=1990,
                                list(nr_inventions=uniqueN(appln_nr_epodoc )),
                                by=c("appln_filing_year","merge_type")][order(appln_filing_year)],
       aes(x=appln_filing_year,y=nr_inventions,fill=merge_type)) + geom_col(position = "stack",alpha=0.6) + theme_classic() +
  scale_fill_manual(values=c("darkblue","steelblue","cyan","red","yellow","green","darkgreen"))

ggsave(paste(path_to_output_data,"/link_gdr_registry/","coverage_bridge_sbr_id_patent_data.png",sep=""))


for (confidence_level in sort(unique(match_result[,merge_type]))) {
  print(confidence_level)
  print(match_result[merge_type==eval(confidence_level),list(merge_type,
                                                             pure_name,
                                                             pure_name_reg,
                                                             wrong_percent=round(wrong_percent,digits = 2),
                                                             str_dist_j =round(str_dist_j ,digits = 2) 
                                                             )])
}



setkey(match_result,
       apl_eee_hrm_id,
       criterium)



setkey(match_result,apl_eee_hrm_id)
results_top_100 <- merge(top_100_gdr_patenters,
                         match_result[rank_fit==1 & nr_fits==1,
                                      list(merge_type,
                                           county, 
                                           pure_name,
                                           pure_name_reg,
                                           wrong_percent=round(wrong_percent,digits = 2),
                                           str_dist,
                                           str_dist_j,
                                           apl_eee_hrm_id )],
                         by="apl_eee_hrm_id")

print(results_top_100[,.N,by="merge_type"])

results_top_100[,list(apl_name,DD,pure_name_reg,merge_type,wrong_percent,str_dist,str_dist_j)]


print(match_result[merge_type=="3tried_to_merge" & str_dist_j <0.74 & str_dist_j>0.70,
                   list(county, 
                        pure_name,
                        pure_name_reg,
                        wrong_percent=round(wrong_percent,digits = 2),
                        str_dist,
                        str_dist_j)])


fwrite(match_result[merge_type!="3tried_to_merge"&rank_fit==1 & nr_fits==1],
       file=paste(path_to_output_data,"/link_gdr_registry/","bridge_sbr_id_patent_data.csv",sep=""))



patents_per_year <- dcast(
  original_apl_patent[unique(match_result[merge_type!="3tried_to_merge",list(apl_eee_hrm_id)]),
                      list(nationality_GDR=max(apl_ctry=="DD"),
                           nr_inventions=uniqueN(docdb_family_id)),
                      by=c("apl_eee_hrm_id","appln_filing_year","patenting_loc")]
  , apl_eee_hrm_id +appln_filing_year +nationality_GDR  ~ patenting_loc , value.var = "nr_inventions")
patents_per_year[is.na(other),other:=0]
patents_per_year[is.na(DD),DD:=0]
patents_per_year[is.na(DE),DE:=0]

setnames(patents_per_year,
         c("DD","DE","other"),
         c("nr_patents_gdr","nr_patents_frg","nr_patents_other"))

fwrite(
  merge(unique(match_result[merge_type!="3tried_to_merge"&rank_fit==1 & nr_fits==1,list(apl_eee_hrm_id,sbr_id)]),
        patents_per_year,
        by="apl_eee_hrm_id",
        all.x=TRUE)[,list( nr_patents_gdr =sum(nr_patents_gdr), 
                           nr_patents_frg = sum(nr_patents_frg),
                           nr_patents_other = sum(nr_patents_other)),
                    by=c( "sbr_id", "appln_filing_year" )],
  file=paste(path_to_output_data,"/link_gdr_registry/","/patents_per_year.csv",sep=""))


}
