###############################################
#++++++++++++describe patstat data++++++++++++#
###############################################
print("07:describe data (team sizes, concentration measures etc.)")
suppressWarnings({
  dir.create(paste0(path_to_output_data,"/",data_name,"/"))
  dir.create(paste0(path_to_output_data,"/",data_name,"/data_description/"))
  dir.create(paste0(path_to_output_data,"/",data_name,"/data_description/descriptives/"))
})


if (describe_patent_data =="YES"){
  patent_data_inv  <- fread(file = paste(path_to_raw_downloaded_data, "patents_inv",".csv", sep=""))
  patent_data_apl  <- fread(file = paste(path_to_raw_downloaded_data, "patents_apl",".csv", sep=""))
  
 # patent_data <- merge(patent_data,
  #                     unique(fread(file=paste(path,"data/apl_name_lexicon_all.csv",sep=""))[,list(apl_eee_hrm_id,apl_eee_sector)]),
 #                      by=c("apl_eee_hrm_id"), all.x=TRUE)[apl_eee_sector=="COMPANY"]
  #
  
  
  library(ggplot2)
  team_size_invention_family <- unique(patent_data_inv[,list(inv_eee_hrm_id=inv_eee_hrm_id,year=min(appln_filing_year)),by="docdb_family_id"])[,N:=.N,by="docdb_family_id"]
  
  
  ggplot(data=unique(team_size_invention_family[N<20,list(N,docdb_family_id)])[
    ,list(share_occurences=.N),by="N"][
      ,list(share_occurences=share_occurences/sum(share_occurences),team_size_invention=N)][
        order(team_size_invention)] , 
    aes(x=team_size_invention,y=share_occurences))    +
    geom_col()
  ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/descriptives/", "distribution_of_team_sizes", ".emf" , sep = ""))
  ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/descriptives/", "distribution_of_team_sizes.png" , sep = ""))
  
  
  
  #nr of inventions per firm
  size_firm <- unique(patent_data_apl[,list(appln_auth=appln_auth, apl_eee_hrm_id=apl_eee_hrm_id, year=min(appln_filing_year)),by="docdb_family_id"])[
    ,split:=1/.N,by=c("docdb_family_id","appln_auth")][
      , decade := ceiling(year/10)*10][
        ,list(firm_nr_of_inventions=sum(split),inventions_with_firm_involved=.N),by=c("apl_eee_hrm_id", "year","appln_auth")][
          ,avg_nr_of_inventions_firm:=mean(firm_nr_of_inventions), by=c("apl_eee_hrm_id","appln_auth")]
  
  
  
  #size classes
  size_firm[firm_nr_of_inventions<=1,firm_size_class := "1: 1 or less"][
    firm_nr_of_inventions<=50 & is.na(firm_size_class)==1,firm_size_class := "2: 50  or less"][
      firm_nr_of_inventions<=200 & is.na(firm_size_class)==1,firm_size_class := "3: 200  or less"][
        firm_nr_of_inventions<=1000 & is.na(firm_size_class)==1,firm_size_class := "4: 1000  or less"][
          firm_nr_of_inventions>1000 & is.na(firm_size_class)==1,firm_size_class := "5: above 1000"]
  
  
  
  #size_firm[appln_auth=="JP" & year==2010,list(sum(firm_nr_of_inventions)),by=c("appln_auth","year","firm_size_class")]
  #nrow(unique(patent_data_inv[,list(appln_auth, year=min(appln_filing_year)),by="docdb_family_id"][appln_auth=="JP" & year ==2010,list(docdb_family_id)]))
  
  
  #percentiles
  size_firm[,rank_firm_in_year := frank(firm_nr_of_inventions, ties.method = "random"), by=c("appln_auth", "year")][
    ,percentile_firm := ceiling(rank_firm_in_year/max(rank_firm_in_year)*100), by=c("appln_auth" , "year")]
  
  size_firm[,.N,by="appln_auth"]
  
  
  
  setkey(size_firm, appln_auth)
  
  vector_of_countries <- c("DD","DE","EP","ES","FR","GB","IT","JP","KR","TW","US")
  
  
  #plot inventions per percentile
  ggplot(data= size_firm[vector_of_countries][year>=1975,list(inventions_per_percentile=sum(firm_nr_of_inventions)),by = c("percentile_firm","year","appln_auth")][
    order(year, appln_auth, percentile_firm)],  
    aes( x=year,y=inventions_per_percentile,group=percentile_firm,fill= percentile_firm)   ) +
    geom_area(position="fill") +scale_fill_gradientn(  colours =c("#AEC6CF","#77DD77" , "#6e362a", "#3B0800","#8B0000") ) + facet_wrap(~appln_auth)
  ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/descriptives/", "share_inventions_per_company_percentiles", ".emf" , sep = ""))
  ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/descriptives/","share_inventions_per_company_percentiles", ".png" , sep = ""))
  
  
  #plot inventions per size_class
  ggplot(data= size_firm[vector_of_countries][year>=1975,list(inventions_per_size_class=sum(firm_nr_of_inventions)),by = c("firm_size_class","year","appln_auth")],  
         aes( x=year,y=inventions_per_size_class,group=firm_size_class ,fill= firm_size_class)   ) +
    geom_area(position="fill") + 
    facet_wrap(~appln_auth) + 
    scale_fill_manual("yearly inventions by the firm", 
                      values =c("#AEC6CF" , "#77DD77" ,"#6e362a","#3B0800", "#8B0000") )
  ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/descriptives/", "share_inventions_per_company_size_class", ".eps" , sep = ""))
  ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/descriptives/", "share_inventions_per_company_size_class", ".png" , sep = ""))
  
  
  
  #plot inventions per size_class
  ggplot(data= size_firm[vector_of_countries][year>=1975,list(inventions_per_size_class=sum(firm_nr_of_inventions)),by = c("firm_size_class","year","appln_auth")][
    order(appln_auth,year,firm_size_class)][
      ,cum_inventions_per_size_class := cumsum(inventions_per_size_class),by=c("appln_auth", "year") ],  
    aes( x=year,y=cum_inventions_per_size_class,group=firm_size_class ,colour= firm_size_class)   ) +
    geom_area() + facet_wrap(~appln_auth) +
    scale_fill_manual("yearly inventions by the firm", 
                      values =c("#AEC6CF" , "#77DD77" ,"#6e362a","#3B0800", "#8B0000") )
  ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/descriptives/", "inventions_per_company_size_class", ".eps" , sep = ""))
  ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/descriptives/", "inventions_per_company_size_class", ".png" , sep = ""))
  
  
  
  
  
  
  
  team_size_within_firms        <- merge (unique(patent_data_apl[,list(docdb_family_id,apl_eee_hrm_id)] )  ,   unique(team_size_invention_family[,list(docdb_family_id, inventors_per_invention=N,year)]) ,by="docdb_family_id",all=TRUE  )
  nr_inventors_working_with_firm <-  unique(merge(unique(patent_data_apl[,list(docdb_family_id,apl_eee_hrm_id,year=appln_filing_year)] ), 
                                                  unique(patent_data_inv[,list(inv_eee_hrm_id,docdb_family_id)]),
                                                  by=c("docdb_family_id") ,
                                                  allow.cartesian=TRUE ,
                                                  all = TRUE )[,list(inv_eee_hrm_id,apl_eee_hrm_id,year)])[,list(nr_inventors_working_with_firm =.N),by=c("apl_eee_hrm_id","year")]
  
  
  team_size_within_firms <- merge(team_size_within_firms, nr_inventors_working_with_firm, by=c("apl_eee_hrm_id","year"))
  setkey(team_size_within_firms, year, apl_eee_hrm_id)
  
  
  
  ggplot(data= team_size_within_firms[ nr_inventors_working_with_firm < 50], 
         aes(group= nr_inventors_working_with_firm,y = inventors_per_invention,x=nr_inventors_working_with_firm)) +
    geom_boxplot() +
    geom_smooth(method = "auto")+ 
    scale_y_continuous(expand = c(0, 0), limits = c(0, 15.5))
  ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/descriptives/", "inventor_team_size_and_firm_size", ".png" , sep = ""))
  
  
  
  
  
  
  #-------------------------
  #Load patents' IPC classes
  #-------------------------
  ipc_classes_for_patents  <- fread(file = paste(path_to_raw_downloaded_data,"ipc.csv", sep=""))
  ipc_classes_for_patents[,ipc_class_symbol_4 :=gsub("\\d*/.*","",ipc_class_symbol)]
  
  
  team_size_and_ipc_class_diversity <-  merge(team_size_within_firms,
                                              unique(ipc_classes_for_patents[,list(docdb_family_id,ipc_class_symbol)])[,list(nr_of_ipc_classes=.N),by="docdb_family_id"],
                                              by="docdb_family_id"
  )
  
  ggplot(data= team_size_and_ipc_class_diversity[inventors_per_invention<11], 
         aes(group= inventors_per_invention,y = nr_of_ipc_classes,x=inventors_per_invention)) +
    geom_boxplot() +
    geom_smooth(method = "auto")+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 15.5)) +
    ggtitle("Nr. of IPC classes per team size")
  ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/descriptives/", "inventor_team_size_and_ipc_class_diversity", ".png" , sep = ""))
  
  
  
   
  
  
  community_result <- fread(file =paste(path_to_output_data,"/",data_name,"/", "communities_ipc_classes.csv", sep=""))
  setkey(community_result,ipc_class_symbol_4)
  community_result[community_result[,.N,by="ipc_class_symbol_4"][N>1,list(ipc_class_symbol_4)]]
  
  team_size_and_ipc_class_diversity <-  merge(team_size_within_firms,
                                              unique(merge(
                                                fread( file = paste(path_to_output_data,"/",data_name,"/", "communities_ipc_classes.csv", sep="")),
                                                unique(ipc_classes_for_patents[,list(docdb_family_id,ipc_class_symbol_4)]),
                                                by="ipc_class_symbol_4"
                                              )
                                              )[,list(nr_of_communities=.N),by="docdb_family_id"]
                                              ,
                                              by="docdb_family_id"
  )
  
  
  
  ggplot(data= team_size_and_ipc_class_diversity[inventors_per_invention<11], 
         aes(group= inventors_per_invention,y = nr_of_communities,x=inventors_per_invention)) +
    geom_boxplot() +
    geom_smooth(method = "auto")+
    scale_y_continuous(expand = c(0, 0), limits = c(0, 15.5)) +
    ggtitle("Nr. of technology clusters per team size")
  ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/descriptives/", "inventor_team_size_and_ipc_community_diversity", ".png" , sep = ""))
  
  
  
  
  rm(patent_data_inv,patent_data_apl, size_firm, team_size_invention_family, team_size_within_firms, nr_inventors_working_with_firm,team_size_and_ipc_class_diversity)
  gc()
}


