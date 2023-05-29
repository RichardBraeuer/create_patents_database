#install.packages("RecordLinkage")
#install.packages("stringdist")

print("08a: Link applicants to registry")
suppressWarnings({
  dir.create(paste0(path_to_output_data,"/link_gdr_registry/"))
})
library("data.table")
library("ggplot2")
#version_of_localizations <- "old"
if (link_to_registry == TRUE) {

  
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
  
  
  #chracterize the apl in terms of where they do their patenting
  #specifically, the share of patents in East Germany
  #original_apl_patent[appln_auth=="DD",patenting_loc:="DD"]
  #original_apl_patent[appln_auth=="DE",patenting_loc:="DE"]
  #original_apl_patent[appln_auth!="DE"&appln_auth!="DD",patenting_loc:="other"]
  

  location_patent_apl <- 
    original_apl_patent[unique(original_apl_patent[appln_auth=="DD"| apl_ctry=="DD",list(apl_eee_hrm_id)])
                        ,list(DD=sum(as.numeric(appln_auth=="DD")),
                              DE=sum(as.numeric(appln_auth=="DE")),
                              other=sum(as.numeric(appln_auth!="DE"&appln_auth!="DD"))),by="apl_eee_hrm_id"]
    
    # 
    # dcast(
    # original_apl_patent[unique(original_apl_patent[appln_auth=="DD"| apl_ctry=="DD",list(apl_eee_hrm_id)])][1:100,
    #                     list(nr_inventions=uniqueN(docdb_family_id)),
    #                     by=c("apl_eee_hrm_id","patenting_loc")]
    # , apl_eee_hrm_id  ~ patenting_loc , value.var = "nr_inventions")
  # location_patent_apl[is.na(other),other:=0]
  # location_patent_apl[is.na(DD),DD:=0]
  # location_patent_apl[is.na(DE),DE:=0]
  location_patent_apl[,share_patent_files_gdr:=DD/(DD+DE+other)]
  location_patent_apl[,rank_gdr_patents:=frankv(x=location_patent_apl,cols ="DD",ties.method = "random",order=c(-1))]
  
  library(ggplot2)
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
  
  
  original_apl_patent[appln_auth=="DD" & appln_filing_year>=1980 & appln_filing_year<=1990,.N,by="merge_type"]
  ggplot(data=original_apl_patent[appln_auth=="DD" & appln_filing_year>=1980 & appln_filing_year<=1990,
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

  
# load the data sbr_id data with which to merge
# alternative: VEB Betriebsliste, but does not 
# contain sbr_id, so where would that leave us?
{
  
  
  
  #sbr_id data
  #---#---#---#---#
  {
  #import gdr labor data
  #---#---#---#---#---#---#
  complete_gdr_labor_data <- fread(paste(path_tha_project_data,"/temp/gdr_firm_data/complete_DE_2_MD_028_data.csv",sep=""))
  setkey(complete_gdr_labor_data,year,sbr_id)
  
  
  #extract gdr employment data
  #---#---#---#---#---#---# 
  #total employment numbers per sbr_id 1980-1989
  employment_1980_1989 <-complete_gdr_labor_data[
    ((punch_card_number == "005"|punch_card_number_num==5) & year <1990),
    list(punch_card_number,punch_card_number_num,sbr_id,year,emp_total_1,county_code_1)]
  #total employment numbers per sbr_id 1990
  employment_1990 <-complete_gdr_labor_data[
    ((punch_card_number_num == 1|punch_card_number_num==2) & year ==1990),
    list(emp_total_1=sum(as.numeric(emp_total_1))),by=c("sbr_id","year")]
  
  
  gdr_employment_data <- rbindlist(list(employment_1980_1989,employment_1990),
                                   fill=TRUE,use.names=TRUE)
  gdr_employment_data[,emp_total_1:=gsub(emp_total_1,pattern="\\*",replacement="0")]
  gdr_employment_data[sbr_id==4006]

  
  
  
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

  gdr_registry_data[is.na(as.numeric(gdr_county_code))]
  gdr_registry_data[is.na(as.numeric(gdr_county_code)),gdr_county_code:=gsub(gdr_county_code,pattern="NA",replacement="00")]
  
  gdr_registry_data[is.na(as.numeric(gdr_county_code))]
  gdr_registry_data[is.na(gdr_county_code)]
  gdr_registry_data[nchar(gdr_county_code)!=6]
  gdr_registry_names <- merge(gdr_registry_names,
                              gdr_registry_data[,list(sbr_id,plant_name_anon ,gdr_county_code=as.numeric(gdr_county_code),directing_body ),],
                              by.x=c("short_name"),
                              by.y=c("plant_name_anon"),
                              all=TRUE)
  gdr_registry_names[,sbr_id:=sbr_id.x]
  gdr_registry_names[is.na(sbr_id),sbr_id:=sbr_id.y]
  gdr_registry_names[,num_sbr_id:=as.numeric(sbr_id)]
  gdr_registry_names[,list(names_data=!is.na(sbr_id.x)==TRUE)][,.N,by="names_data"]
  gdr_registry_names[,list(registry_data=!is.na(sbr_id.y)==TRUE)][,.N,by="registry_data"]
  gdr_registry_names[,list(has_gdr_county=!is.na(gdr_county_code)==TRUE)][,.N,by="has_gdr_county"]
  gdr_registry_data[,list(sbr_id,plant_name_anon ,gdr_county_code=as.numeric(gdr_county_code)),][,
            list(has_gdr_county=!is.na(gdr_county_code)==TRUE)][,.N,by="has_gdr_county"]
  
  
  #merge the county met-info to the names
  gdr_registry_names <- merge(gdr_registry_names,
                              fread(file=paste(path_tha_project_data,"temp/gdr_firm_data/county_codes_gdr_with_frg_city_",version_of_localizations,".csv",sep="")),
                              by="gdr_county_code",
                              all.x = TRUE)
  gdr_registry_names[,list(has_county=!is.na(county_code )==TRUE)][,.N,by="has_county"]
  
  #merge the directing body info to the names
  gdr_registry_names <- merge(gdr_registry_names,
                              as.data.table(read.xlsx(
                                paste(path_tha_project_data,"/orig/gdr_firm_data/DE 2 MD 083/04_CODELISTEN/names_directing_bodies.xlsx",sep=""))),
                              by="directing_body",
                              all.x = TRUE)
  gdr_registry_names[,list(has_dir_body=!is.na(name_directing_body)==TRUE)][,.N,by="has_dir_body"]
  
  
  gdr_registry_names <- unique(gdr_registry_names[,list(gdr_county_code,short_name, sbr_id,gdr_county_name, city_name, county, county_code ,latitude, longitude,name_directing_body)])
  
  
  gdr_registry_names[,pure_name_reg:=tolower(short_name)]
  gdr_registry_names[,pure_name_reg:=gsub(pattern="veb",replacement="",x=pure_name_reg)]
  gdr_registry_names[,pure_name_reg:=gsub(pattern="kombinat",replacement="",x=pure_name_reg)]
  gdr_registry_names[,pure_name_reg:=gsub(pattern="gmbh",replacement="",x=pure_name_reg)]
  gdr_registry_names[,pure_name_reg:=gsub(pattern="gesellschaft",replacement="ges",x=pure_name_reg)]
  gdr_registry_names[,pure_name_reg:=gsub(pattern="ag",replacement="",x=pure_name_reg)]
  clean_string_for_city_matching(gdr_registry_names,"pure_name_reg")

  setkey(gdr_registry_names,sbr_id)
  
  
  gdr_registry_names[,list(nr_sbr_mentions=.N),by="sbr_id"][,.N,by="nr_sbr_mentions"][order(nr_sbr_mentions)]
  gdr_registry_names[sbr_id=="00004014"]
  gdr_registry_names[,shortened_pure_name_reg:=pure_name_reg]
  for (current_city_name in unique(gdr_registry_names[city_name!="",city_name])) {
    gdr_registry_names[city_name==current_city_name,
                       shortened_pure_name_reg:=gsub(shortened_pure_name_reg,
                                           pattern=strsplit(paste(current_city_name, collapse = " "), ' ')[[1]][[1]],
                                           replacement="")]

  }
  gdr_registry_names[,shortened_pure_name_reg:=gsub(shortened_pure_name_reg,pattern="sachsenring",replacement="")]
  clean_string_for_city_matching(gdr_registry_names,"shortened_pure_name_reg")
  gdr_registry_names[,sbr_id_num:=as.numeric(sbr_id)]
  gdr_registry_names <- merge(gdr_registry_names,
                              gdr_employment_data[,list(max_l=max(as.numeric(emp_total_1),na.rm=TRUE)),by="sbr_id"],
                              by.x="sbr_id_num",
                              by.y="sbr_id",
                              all.x=TRUE)
  
  
  
  
  setkey(gdr_registry_names,sbr_id)
  }
  
  
  
  #veb list -> another source of names
  #---#---#---#---#---#---#---#---#---#
  {
  all_veb_long_names <-as.data.table(read.xlsx(
    paste(path_tha_project_data,"/orig/VEB_list/VEB_Betriebsliste.xlsx",sep="")))
  
  setnames(all_veb_long_names,
           c("Name.des.Betriebs","Übergeordnetes.Organ.Kombinat"),
           c("long_name","name_directing_body"))
  all_veb_long_names <- all_veb_long_names[,list(long_name=gsub(long_name,pattern="\\n",replacement=" "),
                                                 name_directing_body=gsub(name_directing_body,pattern="\\n",replacement=" "))]
  all_veb_long_names[,ctry:="DD"]
  all_veb_long_names[,row:=seq_len(.N)]
  localized_veb_long_names <- localize_strings(all_veb_long_names,
                                           vector_of_names_of_variables_to_localize = c("long_name","name_directing_body"),
                                           vector_variables_country_codes = c("ctry"),
                                           version_of_localizations = version_of_localizations,
                                           id_variables = c("row"),
                                           threshold_nr_words_to_consider = 0,
                                           extract_country_codes="no",
                                           extract_firstname_info="no",
                                           extract_city_localization="YES",
                                           extract_clean_names_from="long_name",
                                           threshold_acceptance=0.5,
                                           debug_mode = FALSE,
                                           verbose=FALSE,
                                           only_fitting_blocks =FALSE,
                                           select_most_populous=TRUE,
                                           only_gdr_firms=TRUE
  )
  localized_veb_long_names[,list(has_county=!is.na(county_code)==TRUE)][,.N,by="has_county"]
  
  
  all_veb_long_names <- merge(all_veb_long_names[,list(long_name,name_directing_body,row )],
                              localized_veb_long_names[,list(row ,cleaned_name =gsub(cleaned_name,pattern="veb",replacement="") ,pair_city_region,NUTS2_equivalent_code,county_code,city_code_1998)],
                              by="row",
                              all.x=TRUE)

  gdr_registry_names[sbr_id=="07709009"]
  
  match_result_names <- fuzzy_name_match(data_set_1=gdr_registry_names[#county_code   ==15002 &
                                                      max_l>100 & shortened_pure_name_reg !="",list(sbr_id,shortened_pure_name_reg,county_code,max_l)],
                                     data_set_2=all_veb_long_names[,list(row,cleaned_name,county_code=as.numeric(county_code))],
                                     varname_string_data_1="shortened_pure_name_reg",
                                     varname_id_data_1="sbr_id",
                                     varname_string_data_2="cleaned_name",
                                     varname_id_data_2="row",
                                     varname_string_block_var_both=c("county_code"),
                                     match_no_block=FALSE,
                                     verbose=TRUE)
  
  match_result_names <- rbindlist(list(match_result_names,
                 fuzzy_name_match(data_set_1=gdr_registry_names[!match_result_names[,list(sbr_id)]][#county_code   ==15002 &
                   max_l>100,list(sbr_id,pure_name_reg,county_code,max_l)],
                   data_set_2=all_veb_long_names[,list(row,cleaned_name,county_code=as.numeric(county_code))],
                   varname_string_data_1="pure_name_reg",
                   varname_id_data_1="sbr_id",
                   varname_string_data_2="cleaned_name",
                   varname_id_data_2="row",
                   varname_string_block_var_both=c("county_code"),
                   match_no_block=FALSE,
                   verbose=TRUE)
                 ),use.names=TRUE,fill=TRUE)


  setkey(match_result_names,sbr_id,criterium)
  match_result_names[,select:=seq_len(.N),by="sbr_id"]
  }
  
  
  
  #merge the two 
  #---#---#---#---#
  {
  gdr_registry_names <- merge(gdr_registry_names,
                              match_result_names[select==1,list(sbr_id,cleaned_name)],
                              by="sbr_id",
                              all.x=TRUE)
  gdr_registry_names[cleaned_name=="minol"]
  gdr_registry_names[cleaned_name=="minol"]
    
  }
  

}




# match sbr_ids from the registry with patent data
{
print(names_to_match_strict_def[,.N,by="county"][order(N)])
print(gdr_registry_names[,.N,by="county"][order(N)])
gdr_registry_names[,country:="DD"]
names_to_match_strict_def[!is.na(county_code),uniqueN(apl_eee_hrm_id)]
names_to_match_strict_def[,uniqueN(apl_eee_hrm_id)]
names_to_match_strict_def[,country:="DD"]

match_result <-   fuzzy_name_match(data_set_1=names_to_match_strict_def[!is.na(county_code )][,list(pure_name, apl_eee_hrm_id ,country_code_city, county_code)],
                                   data_set_2=gdr_registry_names[!is.na(county_code )][,list(sbr_id,cleaned_name,country_code_city="de",county_code)],
                                   varname_string_data_1="pure_name",
                                   varname_id_data_1="apl_eee_hrm_id",
                                   varname_string_data_2="cleaned_name",
                                   varname_id_data_2="sbr_id",
                                   varname_string_block_var_both=c("county_code","country_code_city"),
                                   match_no_block=FALSE,
                                   verbose=TRUE)[,list(country_code_city,county_code,apl_eee_hrm_id,sbr_id,patent_name=pure_name,
                                                       registry_name=cleaned_name,criterium, merge_type, change_crit,
                                                       merge_variable="long name veb list")]


match_result <- rbindlist(list(match_result,
                                     fuzzy_name_match(data_set_1=names_to_match_strict_def[!match_result[merge_type=="5middle"|merge_type=="6high",list(apl_eee_hrm_id)]][
                                                                          !is.na(county_code)][,list(pure_name, apl_eee_hrm_id ,country_code_city, county_code)],
                                                      data_set_2=gdr_registry_names[!is.na(county_code )][,list(sbr_id,shortened_pure_name_reg,country_code_city="de",county_code)],
                                                      varname_string_data_1="pure_name",
                                                      varname_id_data_1="apl_eee_hrm_id",
                                                      varname_string_data_2="shortened_pure_name_reg",
                                                      varname_id_data_2="sbr_id",
                                                      varname_string_block_var_both=c("county_code","country_code_city"),
                                                      match_no_block=FALSE,
                                                      verbose=TRUE)[,list(country_code_city,county_code,apl_eee_hrm_id,sbr_id,patent_name=pure_name,
                                                                          registry_name=shortened_pure_name_reg,criterium, merge_type, change_crit,
                                                                          merge_variable="shortened cleaned registry")]
),use.names=TRUE,fill=TRUE)


match_result <- rbindlist(list(match_result,
                                     fuzzy_name_match(data_set_1=names_to_match_strict_def[!match_result[merge_type=="5middle"|merge_type=="6high",list(apl_eee_hrm_id)]][
                                       !is.na(county_code)][,list(pure_name, apl_eee_hrm_id ,country_code_city, county_code)],
                                                      data_set_2=gdr_registry_names[!is.na(county_code )][,list(sbr_id,name_directing_body,country_code_city="de",county_code)],
                                                      varname_string_data_1="pure_name",
                                                      varname_id_data_1="apl_eee_hrm_id",
                                                      varname_string_data_2="name_directing_body",
                                                      varname_id_data_2="sbr_id",
                                                      varname_string_block_var_both=c("county_code","country_code_city"),
                                                      match_no_block=FALSE,
                                                      verbose=TRUE)[,list(country_code_city,county_code,apl_eee_hrm_id,sbr_id,patent_name=pure_name,
                                                                          registry_name=name_directing_body,criterium, merge_type, change_crit,
                                                                          merge_variable="directing body")]
),use.names=TRUE,fill=TRUE)

        

match_result <- rbindlist(list(match_result,
                                     fuzzy_name_match(data_set_1=names_to_match_strict_def[!match_result[merge_type=="5middle"|merge_type=="6high",list(apl_eee_hrm_id)]][
                                       !is.na(county_code)][,list(pure_name, apl_eee_hrm_id ,country_code_city, county_code)],
                                                      data_set_2=gdr_registry_names[!is.na(county_code )][,list(sbr_id,pure_name_reg,country_code_city="de",county_code)],
                                                      varname_string_data_1="pure_name",
                                                      varname_id_data_1="apl_eee_hrm_id",
                                                      varname_string_data_2="pure_name_reg",
                                                      varname_id_data_2="sbr_id",
                                                      varname_string_block_var_both=c("county_code","country_code_city"),
                                                      match_no_block=FALSE,
                                                      verbose=TRUE)[,list(country_code_city,county_code,apl_eee_hrm_id,sbr_id,patent_name=pure_name,
                                                                          registry_name=pure_name_reg,criterium, merge_type, change_crit,
                                                                          merge_variable="cleaned registry")]
),use.names=TRUE,fill=TRUE)
 




#without county 
match_result <-   rbindlist(list(match_result,
                                 fuzzy_name_match(data_set_1=names_to_match_strict_def[!match_result[merge_type=="5middle"|merge_type=="6high",list(apl_eee_hrm_id)]][
                                   ][,list(pure_name, apl_eee_hrm_id ,country)],
                                   data_set_2=gdr_registry_names[][,list(sbr_id,cleaned_name,country)],
                                   varname_string_data_1="pure_name",
                                   varname_id_data_1="apl_eee_hrm_id",
                                   varname_string_data_2="cleaned_name",
                                   varname_id_data_2="sbr_id",
                                   varname_string_block_var_both=c("country"),
                                   match_no_block=FALSE,
                                   verbose=TRUE)[,list(country,apl_eee_hrm_id,sbr_id,patent_name=pure_name,
                                                       registry_name=cleaned_name,criterium, merge_type, change_crit,
                                                       merge_variable="long name veb list")]
),use.names=TRUE,fill=TRUE)

match_result <- rbindlist(list(match_result,
                               fuzzy_name_match(data_set_1=names_to_match_strict_def[!match_result[merge_type=="5middle"|merge_type=="6high",list(apl_eee_hrm_id)]][
                                 ][,list(pure_name, apl_eee_hrm_id ,country)],
                                 data_set_2=gdr_registry_names[][,list(sbr_id,shortened_pure_name_reg,country)],
                                 varname_string_data_1="pure_name",
                                 varname_id_data_1="apl_eee_hrm_id",
                                 varname_string_data_2="shortened_pure_name_reg",
                                 varname_id_data_2="sbr_id",
                                 varname_string_block_var_both=c("country"),
                                 match_no_block=FALSE,
                                 verbose=TRUE)[,list(country,apl_eee_hrm_id,sbr_id,patent_name=pure_name,
                                                     registry_name=shortened_pure_name_reg,criterium, merge_type, change_crit,
                                                     merge_variable="shortened cleaned registry")]
),use.names=TRUE,fill=TRUE)


match_result <- rbindlist(list(match_result,
                               fuzzy_name_match(data_set_1=names_to_match_strict_def[!match_result[merge_type=="5middle"|merge_type=="6high",list(apl_eee_hrm_id)]][
                                 ][,list(pure_name, apl_eee_hrm_id ,country)],
                                 data_set_2=gdr_registry_names[][,list(sbr_id,name_directing_body,country)],
                                 varname_string_data_1="pure_name",
                                 varname_id_data_1="apl_eee_hrm_id",
                                 varname_string_data_2="name_directing_body",
                                 varname_id_data_2="sbr_id",
                                 varname_string_block_var_both=c("country"),
                                 match_no_block=FALSE,
                                 verbose=TRUE)[,list(country,apl_eee_hrm_id,sbr_id,patent_name=pure_name,
                                                     registry_name=name_directing_body,criterium, merge_type, change_crit,
                                                     merge_variable="directing body")]
),use.names=TRUE,fill=TRUE)



match_result <- rbindlist(list(match_result,
                               fuzzy_name_match(data_set_1=names_to_match_strict_def[!match_result[merge_type=="5middle"|merge_type=="6high",list(apl_eee_hrm_id)]][
                                 ][,list(pure_name, apl_eee_hrm_id ,country)],
                                 data_set_2=gdr_registry_names[][,list(sbr_id,pure_name_reg,country)],
                                 varname_string_data_1="pure_name",
                                 varname_id_data_1="apl_eee_hrm_id",
                                 varname_string_data_2="pure_name_reg",
                                 varname_id_data_2="sbr_id",
                                 varname_string_block_var_both=c("country"),
                                 match_no_block=FALSE,
                                 verbose=TRUE)[,list(country,apl_eee_hrm_id,sbr_id,patent_name=pure_name,
                                                     registry_name=pure_name_reg,criterium, merge_type, change_crit,
                                                     merge_variable="cleaned registry")]
),use.names=TRUE,fill=TRUE)



match_result[,max_l:=NULL]
match_result[,sbr_id_num:=as.numeric(sbr_id)]
match_result <- merge(match_result,
                      gdr_employment_data[,list(max_l=-max(as.numeric(emp_total_1),na.rm=TRUE)),by="sbr_id"],
                      by.x="sbr_id_num",
                      by.y="sbr_id",
                      all.x=TRUE)
match_result[is.na(max_l),max_l:=0]
match_result[,no_county_match:=is.na(county_code)]

setkey(match_result,apl_eee_hrm_id,sbr_id)
match_result[,match_proposal_id :=seq_len(.N),by=c("apl_eee_hrm_id","sbr_id")]
match_result[,match_proposal_id:=cumsum(as.numeric(match_proposal_id==1))]
setkey(match_result,apl_eee_hrm_id,criterium,max_l)

match_result[,rank_fit := seq_len(.N),by="apl_eee_hrm_id"]
match_result[,rank_fit :=min(rank_fit),by="match_proposal_id"]
match_result[,nr_fits := uniqueN(match_proposal_id),by=c("apl_eee_hrm_id","criterium","max_l")]

match_result[,.N,by="merge_variable"]
match_result[,.N,by="merge_type"]

match_result[,best_strings_in_proposal:=seq_len(.N)==1,by=c("apl_eee_hrm_id","sbr_id")]
match_result_final <- match_result[
  #accept best match in county if it to only 1 sbr or to a whole combine
  ((!is.na(county_code) & rank_fit==1 & (nr_fits==1|merge_variable=="directing body")) &best_strings_in_proposal==TRUE) |
  #accept best high quality match with no county if unique or to a whole combine
  ((is.na(county_code) & rank_fit==1 & merge_type =="6high" & (nr_fits==1|merge_variable=="directing body")) &best_strings_in_proposal==TRUE)
  ]


original_apl_patent[merge_type=="4low"|merge_type=="5middle"|merge_type=="6high",merge_type :="3tried_to_merge"]
original_apl_patent[unique(match_result_final[merge_type=="4low",list(apl_eee_hrm_id)]),merge_type:="4low"]
original_apl_patent[unique(match_result_final[merge_type=="5middle",list(apl_eee_hrm_id)]),merge_type:="5middle"]
original_apl_patent[unique(match_result_final[merge_type=="6high",list(apl_eee_hrm_id)]),merge_type:="6high"]

    
inspect_fails <- merge(unique(original_apl_patent[merge_type =="3tried_to_merge",list(apl_eee_hrm_id)]),
      top_100_gdr_patenters,
      by="apl_eee_hrm_id")[order(rank_gdr_patents)][,list(apl_name,apl_eee_hrm_id ,county_code ,rank_gdr_patents,DD    )]


nr_to_inspect <- 1
view_failed_match_result <- match_result[inspect_fails[nr_to_inspect,apl_eee_hrm_id]==apl_eee_hrm_id]
view_failed_match_input <- names_to_match_strict_def[inspect_fails[nr_to_inspect,apl_eee_hrm_id]==apl_eee_hrm_id]
view_failed_potential_matches <- gdr_registry_names[inspect_fails[nr_to_inspect,county_code]==county_code]
view_failed_potential_matches[,.N,by="name_directing_body"]
  }
  
  
  # load the data sbr_id data with which to merge
  # alternative: VEB Betriebsliste, but does not 
  # contain sbr_id, so where would that leave us?
  {
ggplot(data=original_apl_patent[appln_auth=="DD" & appln_filing_year>=1980 & appln_filing_year<=1990,
                                list(nr_inventions=uniqueN(appln_nr_epodoc )),
                                by=c("appln_filing_year","merge_type")][order(appln_filing_year)],
       aes(x=appln_filing_year,y=nr_inventions,fill=merge_type)) + geom_col(position = "stack",alpha=0.6) + theme_classic() +
  scale_fill_manual(values=c("darkblue","steelblue","cyan","red","yellow","green","darkgreen"))

ggsave(paste(path_to_output_data,"/link_gdr_registry/","coverage_bridge_sbr_id_patent_data.png",sep=""))

 
for (confidence_level in sort(unique(match_result_final[,merge_type]))) {
  print(confidence_level)
  print(match_result_final[merge_type==eval(confidence_level),list(merge_type,
                                                                   patent_name,
                                                                   registry_name ,
                                                             criterium=round(criterium,digits = 2),
                                                             county_code
                                                             )])
}



setkey(match_result_final,
       apl_eee_hrm_id,
       criterium)



setkey(match_result_final,apl_eee_hrm_id)
results_top_100 <- merge(top_100_gdr_patenters,
                         match_result_final[rank_fit==1 & nr_fits==1,
                                      list(merge_type,
                                           county_code, 
                                           patent_name,
                                           registry_name,
                                           criterium=round(criterium,digits = 2),
                                           apl_eee_hrm_id )],
                         by="apl_eee_hrm_id")

print(results_top_100[,.N,by="merge_type"])

results_top_100[,list(apl_name,DD,registry_name,merge_type,criterium)]


print(match_result_final[merge_type=="3tried_to_merge",
                   list(county_code, 
                        patent_name,
                        registry_name,
                        criterium=round(criterium,digits = 2))])


fwrite(match_result_final,
       file=paste(path_to_output_data,"/link_gdr_registry/","bridge_sbr_id_patent_data.csv",sep=""))



patents_per_year <- dcast(
  original_apl_patent[unique(match_result_final[merge_type!="3tried_to_merge",list(apl_eee_hrm_id)]),
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
  merge(unique(match_result_final[merge_type!="3tried_to_merge"&rank_fit==1 & nr_fits==1,list(apl_eee_hrm_id,sbr_id)]),
        patents_per_year,
        by="apl_eee_hrm_id",
        all.x=TRUE)[,list( nr_patents_gdr =sum(nr_patents_gdr), 
                           nr_patents_frg = sum(nr_patents_frg),
                           nr_patents_other = sum(nr_patents_other)),
                    by=c( "sbr_id", "appln_filing_year" )],
  file=paste(path_to_output_data,"/link_gdr_registry/","/patents_per_year.csv",sep=""))


  }
}
