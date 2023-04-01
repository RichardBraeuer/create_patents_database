###############################################
#+++++++++find_ipc_class_communities++++++++++#
###############################################

suppressWarnings({
dir.create(paste0(path_to_output_data,"/",data_name,"/"))
dir.create(paste0(path_to_output_data,"/",data_name,"/data_description/"))
dir.create(paste0(path_to_output_data,"/",data_name,"/data_description/ipc_communities/"))
})

if (find_ipc_class_communites =="YES"){
  #-------------------------
  #Load patents' IPC classes
  #-------------------------
  #patent level
  ipc_classes_for_patents  <- fread(file = paste(path_to_raw_downloaded_data,"ipc.csv", sep=""))
  if (data_name!=""){

    relevant_appln_ids <- merge(
      unique(fread(paste(path_to_raw_downloaded_data,"patents_inv.csv",sep=""),
            encoding="UTF-8")[,list(appln_nr_epodoc,appln_id )]),
      fread(file=paste(path_to_output_data,"//",data_name,"/list_patents_",data_name_short,".csv",sep="")),
      by="appln_nr_epodoc"
    )

    ipc_classes_for_patents  <- merge(
      ipc_classes_for_patents,
      relevant_appln_ids,
      by="appln_id"
    )
    }
  #patent family level (any patent class somewhere in 
  #the patent family counts for all inventors) par-
  #ticipating in any patent of the family
  ipc_classes_for_patent_families <-unique(ipc_classes_for_patents[,list(docdb_family_id,appln_id, ipc_class_symbol)])
  if (data_name!=""){
    ipc_classes_for_patent_families  <- merge(
      ipc_classes_for_patent_families,
      relevant_appln_ids,
      by="appln_id"
    )
  }
  ipc_classes_for_patent_families <-unique(ipc_classes_for_patent_families[,list(docdb_family_id, ipc_class_symbol)])
  #-------------------------
  #Read in disambiguation result
  #-------------------------
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
           old=c("og_id"),
           new=c("inv_person_id"))
  setkey(PatentsView_identifiers,inv_person_id)
  PatentsView_identifiers[,inv_person_id:=as.numeric(inv_person_id)]
  }
  if (exists("PatentsView_identifiers")==FALSE){
    PatentsView_identifiers <- fread(file=paste(path_to_output_data,"/",data_name,"/list_cleaned_ids_",data_name_short,".csv",sep=""),
                                     encoding="UTF-8"
    )
  }
  #-----------------------------------------------------------
  #Connect disambiguation result to patents and patent families
  #------------------------------------------------------------
  
  inventors_of_patents <- fread(file = paste(path_to_raw_downloaded_data, "patents_inv",".csv", sep=""),
                                encoding="UTF-8")[,list(inv_person_id,year = min(appln_filing_year)), by="docdb_family_id"]
    
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
  
  inventors_of_patents[,nr_of_patents_inventor:=.N, by=id_variable_to_use]
  inventors_of_patents[,team_size_patent:=.N, by="docdb_family_id"]
  
  length(unique(inventors_of_patents[,docdb_family_id]))
  length(unique(inventors_of_patents[team_size_patent==1,docdb_family_id]))
  
  
  #-----------------------------------------------------
  #Connect inventors to ipc classes via patent families
  #----------------------------------------------------
  
  
  
  
  ipc_classes_per_patent_inventor <- merge(inventors_of_patents,
                                ipc_classes_for_patent_families,
                                by=c("docdb_family_id"),
                                all.x=TRUE,
                                allow.cartesian=TRUE)
  
  

  ipc_classes_per_patent_inventor[,ipc_class_symbol :=gsub(" ","",ipc_class_symbol)]
  ipc_classes_per_patent_inventor[is.na(ipc_class_symbol),ipc_class_symbol:=""]
  ipc_classes_per_patent_inventor[,ipc_class_symbol_4 :=gsub("\\d*/.*","",ipc_class_symbol)]
  ipc_classes_per_patent_inventor[,ipc_class_symbol_4 :=gsub(" ","",ipc_class_symbol_4)]

  unique(ipc_classes_per_patent_inventor[team_size_patent==1,list(docdb_family_id, ipc_class_symbol_4)])[,.N,by="docdb_family_id"][N==1]
  
  
  #---------------------------------
  #Count IPC occurences per inventor
  #---------------------------------
  ipc_occurences_per_inventor_8 <- copy(ipc_classes_per_patent_inventor)
  ipc_occurences_per_inventor_8[,share_ipc_class:=.N,by=c(id_variable_to_use,"ipc_class_symbol")]
  ipc_occurences_per_inventor_8[,share_ipc_class:=share_ipc_class/nr_of_patents_inventor,]
  ipc_occurences_per_inventor_8[,is_max_share:=(share_ipc_class==max(share_ipc_class)),by=id_variable_to_use]
  ipc_occurences_per_inventor_8[,nr_ipc_classes_with_max_share:=sum(is_max_share),by=id_variable_to_use]
  library(ggplot2)
  ggplot(data=ipc_occurences_per_inventor_8,
         aes(x=share_ipc_class)) +
    geom_histogram(bins = 100) + 
    geom_histogram(data=ipc_occurences_per_inventor_8[is_max_share==TRUE,], fill="blue",bins = 100) + 
    geom_histogram(data=ipc_occurences_per_inventor_8[(is_max_share==TRUE & nr_ipc_classes_with_max_share == 1),], fill="green",bins = 100) +
    ggtitle("Share of ipc classes per inventor (all/highest shares/highest singular share")
  ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/ipc_communities/", "ipc_8_shares_inventors", ".eps" , sep = ""))
  ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/ipc_communities/", "ipc_8_shares_inventors", ".png" , sep = ""))
  
  (length(unique(ipc_occurences_per_inventor_8[nr_ipc_classes_with_max_share==1,c(id_variable_to_use),with=FALSE]))  / length(unique(ipc_occurences_per_inventor_8[,c(id_variable_to_use),with=FALSE])) )
  (length(unique(ipc_occurences_per_inventor_8[nr_ipc_classes_with_max_share==1 & is_max_share==1 & share_ipc_class>0.5,c(id_variable_to_use),with=FALSE]))  / length(unique(ipc_occurences_per_inventor_8[,c(id_variable_to_use),with=FALSE])) )
  #rm(ipc_occurences_per_inventor_8)
  #---------------------------------
  #Count IPC occurences per inventor(4digit)
  #---------------------------------
  
  ipc_occurences_per_inventor_4 <- unique(ipc_classes_per_patent_inventor[,c("docdb_family_id", "ipc_class_symbol_4", id_variable_to_use, "nr_of_patents_inventor", "team_size_patent"),with=FALSE])
  ipc_occurences_per_inventor_4[,share_ipc_class:=.N,by=c(id_variable_to_use,"ipc_class_symbol_4")]
  ipc_occurences_per_inventor_4[,share_ipc_class:=share_ipc_class/nr_of_patents_inventor,]
  ipc_occurences_per_inventor_4[,is_max_share:=(share_ipc_class==max(share_ipc_class)),by=id_variable_to_use]
  ipc_occurences_per_inventor_4[,nr_ipc_classes_with_max_share:=sum(is_max_share),by=id_variable_to_use]
  library(ggplot2)
  ggplot(data=ipc_occurences_per_inventor_4,
         aes(x=share_ipc_class)) +
    geom_histogram(bins = 100) + 
    geom_histogram(data=ipc_occurences_per_inventor_4[is_max_share==TRUE,], fill="blue",bins = 100) + 
    geom_histogram(data=ipc_occurences_per_inventor_4[(is_max_share==TRUE & nr_ipc_classes_with_max_share == 1),], fill="green",bins = 100) +
    ggtitle("Share of ipc classes per inventor (all/highest shares/highest singular share")
  ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/ipc_communities/", "ipc_4_shares_inventors", ".eps" , sep = ""))
  ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/ipc_communities/", "ipc_4_shares_inventors", ".png" , sep = ""))
  
  print(length(unique(ipc_occurences_per_inventor_4[nr_ipc_classes_with_max_share==1,c(id_variable_to_use),with=FALSE]))  / length(unique(ipc_occurences_per_inventor_4[,c(id_variable_to_use),with=FALSE])) )
  print(length(unique(ipc_occurences_per_inventor_4[nr_ipc_classes_with_max_share==1 & is_max_share==1 & share_ipc_class>0.5,c(id_variable_to_use),with=FALSE]))  / length(unique(ipc_occurences_per_inventor_4[,c(id_variable_to_use),with=FALSE])) )
  #rm(ipc_occurences_per_inventor_4)
  #-----------------------------------------
  #Create data with only one inventor patents
  #------------------------------------------
  
  #----
  #load all patents created by single inventors
  #--------------------------------------------
  setkeyv(inventors_of_patents,
          c("docdb_family_id",id_variable_to_use))
  setkey(ipc_classes_per_patent_inventor,docdb_family_id)
  one_inventor_patents <- unique(ipc_classes_per_patent_inventor[list(unique(inventors_of_patents[, c(id_variable_to_use, "docdb_family_id"),with=FALSE])[,.N,by="docdb_family_id"][N==1,docdb_family_id])][,c("docdb_family_id","year", "ipc_class_symbol","ipc_class_symbol_4",id_variable_to_use),with=FALSE][is.na(get(id_variable_to_use))==FALSE])
  setkeyv(one_inventor_patents, 
          c(id_variable_to_use, "year", "docdb_family_id", "ipc_class_symbol"))
  list_of_ipc_classes <- sort(unique(one_inventor_patents[,ipc_class_symbol]))
  list_of_ipc_classes_4 <- sort(unique(one_inventor_patents[,ipc_class_symbol]))        
  #----        
  #create the ipc compbination of every patent in sample
  #-----------------------------------------------------
  patent_ipc_class_combinations <- unique(one_inventor_patents[,list(docdb_family_id,ipc_class_symbol)])[,list(ipc_combination=paste0(ipc_class_symbol,collapse=" ")),by=.(docdb_family_id)]
  patent_ipc_class_combinations_4 <- unique(one_inventor_patents[,list(docdb_family_id,ipc_class_symbol_4)])[,list(ipc_combination=paste0(ipc_class_symbol_4,collapse=" ")),by=.(docdb_family_id)]
  
  
  
  #----        
  #decide on a cutoff for how often combinations 
  #have to be present to make it into the data
  #--------------------------------------------
  importance_of_combinations <- patent_ipc_class_combinations[,list(.N),by="ipc_combination"]
  importance_of_combinations[,nr_of_patents :=N]
  importance_of_combinations[,order :=-N]
  setkey(importance_of_combinations,order)
  importance_of_combinations[, nr_of_combinations_above_N := seq_len(.N)]
  importance_of_combinations[,order :=NULL]
  setkey(importance_of_combinations, ipc_combination)
  setkey(importance_of_combinations,N)
  
  #candidate cutof
  cutoff_N_ipc_combinations <- 0
  #print graph of tradeoff between patents lost and combination reduced
  library(ggplot2)
  ggplot(data=importance_of_combinations[,list(nr_of_patents=sum(nr_of_patents),nr_of_combinations_above_N=min(nr_of_combinations_above_N)),by="N"][,nr_of_combinations_above_N:=nr_of_combinations_above_N/max(nr_of_combinations_above_N)][order(N)][,cumulative_patents:=cumsum(nr_of_patents)/sum(nr_of_patents)][N>(cutoff_N_ipc_combinations-500) & N<(cutoff_N_ipc_combinations+500)],
         mapping = aes(y=cumulative_patents,x=N)
  ) + 
    geom_line(aes(y=nr_of_combinations_above_N)) + 
    geom_line() +
    geom_vline(aes(xintercept=eval(cutoff_N_ipc_combinations)))
  #print number of combinations still remainng
  print(importance_of_combinations[N>eval(cutoff_N_ipc_combinations),nr_of_combinations_above_N][[1]])
  
  #decide final cutoff
  final_cutoff_combination_importance <- cutoff_N_ipc_combinations
  #select combinations accordingly
  combination_taken_into_consideration <- sort(importance_of_combinations[nr_of_patents>eval(final_cutoff_combination_importance)][,ipc_combination])
  length(combination_taken_into_consideration)
  #delete temporary data
  #rm(cutoff_N_ipc_combinations,importance_of_combinations,final_cutoff_combination_importance)
  
  #----
  #generate graph between combinations
  #-----------------------------------
  
  #create data set on which inventors were active in which combinations
  setkey(patent_ipc_class_combinations,ipc_combination)
  data_graph_inventor_movements_patents_original_ipc8 <-unique(
    merge(
      unique(patent_ipc_class_combinations[list(combination_taken_into_consideration)]),
      inventors_of_patents[,c(id_variable_to_use , "docdb_family_id", "year"),with=FALSE],
      by="docdb_family_id"
    )[,c(id_variable_to_use,
            "docdb_family_id",
            "year",
            "ipc_combination"),with=FALSE]
  )
  setkey(patent_ipc_class_combinations_4,ipc_combination)
  data_graph_inventor_movements_patents_original_ipc4 <-unique(
    merge(
      unique(patent_ipc_class_combinations_4[list(combination_taken_into_consideration)]),
      inventors_of_patents[,c(id_variable_to_use , "docdb_family_id", "year"),with=FALSE],
      by="docdb_family_id"
    )[,c(id_variable_to_use,
         "docdb_family_id",
         "year",
         "ipc_combination"),with=FALSE]
  )
  
  
  data_graph_inventor_movements_patents_original_classes_ipc4 <-unique(
    merge(
      unique(one_inventor_patents[,list(docdb_family_id,ipc_combination=ipc_class_symbol_4)]),
      inventors_of_patents[,c(id_variable_to_use , "docdb_family_id", "year"),with=FALSE],
      by="docdb_family_id"
    ))

  data_graph_inventor_movements_patents_original_classes_ipc8 <-unique(
    merge(
      unique(one_inventor_patents[,list(docdb_family_id,ipc_combination=ipc_class_symbol)]),
      inventors_of_patents[,c(id_variable_to_use , "docdb_family_id", "year"),with=FALSE],
      by="docdb_family_id"
    ))        
  
  for (length_ipc in c("ipc8","ipc4","classes_ipc8","classes_ipc4")) {
    #define the name of the various datas, so that the correct ipc length is always referenced
    name_original_data <- paste("data_graph_inventor_movements_patents_original",length_ipc, sep="_")
    name_movement_data <- paste("data_graph_inventor_movements_patents",length_ipc, sep="_" )
    name_node_data     <- paste("node_data_inventors_between_combinations",length_ipc, sep="_" )
    name_edge_data     <- paste("edge_data_between_combinations",length_ipc, sep="_" )
    name_ipc_encoding <-  paste(length_ipc, "combinations_and_clusters_0", sep="_")
    
    #encode the original ipc class combinations
    assign(eval(name_ipc_encoding),
           unique(get(name_original_data)[,list(ipc_combination)])
    )
    setkey(get(name_ipc_encoding),ipc_combination)
    get(name_ipc_encoding)[,tech_cluster_id:=seq_len(.N)]
    
    
    
    #----
    # This section of the code seperates out the edge 
    # and the node data, to be used in standard clustering
    # algorithms
    
    #copy of the original data to work with and merge
    #with the encoded ipc combinations
    assign(eval(name_movement_data),
           unique(merge(copy(get(name_original_data)),
                        get(name_ipc_encoding),
                        by="ipc_combination"
           )[,c("tech_cluster_id", id_variable_to_use,  "docdb_family_id", "year"),with=FALSE]
           )
    )
    
    #sort patents in a chronological order, tiebreak 
    #patents in the same year randomly
    nr_rows_movemnt_data <- nrow(get(name_movement_data))
    get(name_movement_data)[,random:=runif(nr_rows_movemnt_data,0,1)]
    setkeyv(get(name_movement_data),
            c(id_variable_to_use,"year","random"))
    
    get(name_movement_data)[,random:=NULL]
    get(name_movement_data)[,next_tech_cluster_id := shift(tech_cluster_id, type ="lead"),by=id_variable_to_use]
    
    
    #Create node data: Count the number of patents for each combination
    nr_of_patents <-   length(unique(get(name_movement_data)[,docdb_family_id]))
    nr_of_inventors <- length(unique(get(name_movement_data)[,id_variable_to_use,with=FALSE]))
    assign(eval(name_node_data),
           get(name_movement_data)[,list(patents_in_node = .N),by="tech_cluster_id"][,all_patents:=nr_of_patents][,share_all_patents:=patents_in_node/all_patents]
    )
    setkey(get(name_node_data),tech_cluster_id)
    #Create node data: Count the number of active inventors for each combination
    assign(eval(name_node_data),
           merge( get(name_node_data),
                  get(name_movement_data)[,list(inventors_in_node = .N),by="tech_cluster_id"][,all_inventors:=nr_of_inventors][,share_all_inventors:=inventors_in_node/all_inventors],
                  by="tech_cluster_id")
    )
    setkey(get(name_node_data),tech_cluster_id)
    
    
    
    #Count how often inventor move from one 
    # node to the other: edge data
    assign(name_edge_data,
           get(name_movement_data)[,.N,by=c("tech_cluster_id","next_tech_cluster_id")]
    )
    setkey(get(name_edge_data),tech_cluster_id,next_tech_cluster_id )
    
    
    
    
    
    
    get(name_edge_data)[,overall_nr_of_occurences := sum(N),by="tech_cluster_id"]
    get(name_edge_data)[,movement_probability := N/overall_nr_of_occurences,]
    get(name_edge_data)[,sum_probability:=sum(movement_probability*(movement_probability>0.01)),by="tech_cluster_id"]
    get(name_edge_data)[is.na(next_tech_cluster_id)==TRUE,prob_of_end:=sum(movement_probability),by="tech_cluster_id"]
    get(name_edge_data)[is.na(next_tech_cluster_id) == FALSE & next_tech_cluster_id!=tech_cluster_id,
                        prob_given_movement:=sum(movement_probability),
                        by="tech_cluster_id"]
    get(name_edge_data)[,prob_given_movement:=movement_probability/prob_given_movement,by="tech_cluster_id"]
    get(name_edge_data)[,distance_measure := 1-movement_probability]
    
    setkey(get(name_edge_data),tech_cluster_id, distance_measure)
    get(name_edge_data)[tech_cluster_id!=next_tech_cluster_id & is.na(next_tech_cluster_id)==FALSE
                        ,rank_nearness := seq_len(.N),by="tech_cluster_id"]
    
    
    
    data_graph_knn_distance <- rbindlist(list(
      get(name_edge_data)[rank_nearness<=1,list(neighbor = "nearest"  , overall_nr_of_occurences , mean_distance=mean(distance_measure), mean_movement_probability=mean(movement_probability)),by="tech_cluster_id"],
      get(name_edge_data)[rank_nearness<=2,list(neighbor = "nearest 2", overall_nr_of_occurences , mean_distance=mean(distance_measure), mean_movement_probability=mean(movement_probability)),by="tech_cluster_id"],
      get(name_edge_data)[rank_nearness<=3,list(neighbor = "nearest 3", overall_nr_of_occurences , mean_distance=mean(distance_measure), mean_movement_probability=mean(movement_probability)),by="tech_cluster_id"]
    ))[
      overall_nr_of_occurences<100,size_class := "small"][
        overall_nr_of_occurences>=100,size_class := "above 100 patents"]
    library(ggplot2)
    ggplot(data=data_graph_knn_distance[], aes(x=mean_distance, group= neighbor, fill= neighbor))+
      geom_histogram(aes(y=..density..) , bins=100) +
      facet_grid(size_class~ .) +
      ggtitle(paste("Avg. Distance to Nearest Other Clusters:",length_ipc))
    ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/ipc_communities/", "distance_to_nearest_clusters_", length_ipc, ".eps" , sep = ""))
    ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/ipc_communities/", "distance_to_nearest_clusters_", length_ipc, ".png" , sep = ""))
    
    
    library(ggplot2)
    ggplot(data=data_graph_knn_distance, aes(x=mean_distance, color=neighbor, group= neighbor, fill= neighbor))+
      geom_step(stat="ecdf") +
      facet_grid(size_class~ .) +
      ggtitle(paste("Cdf of avg. Distance to Nearest Other Clusters:",length_ipc))
    ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/ipc_communities/", "cdf_distance_to_nearest_clusters_", length_ipc, ".eps" , sep = ""))
    ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/ipc_communities/", "cdf_distance_to_nearest_clusters_", length_ipc, ".png" , sep = ""))
    
    
    
    library(ggplot2)
    ggplot(data=data_graph_knn_distance, aes(x=mean_movement_probability, group= neighbor, fill= neighbor))+
      geom_histogram(aes(y=..density..) , bins=100) +
      facet_grid(size_class~ .) +
      ggtitle(paste("Avg. Movement Probabilities to Nearest Other Clusters:",length_ipc))
    ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/ipc_communities/", "movement_probability_to_nearest_clusters_", length_ipc, ".eps" , sep = ""))
    ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/ipc_communities/", "movement_probability_to_nearest_clusters_", length_ipc, ".png" , sep = ""))
    
  }
  
  
  
  
  
  # pick the specification
  node_data_inventors_between_combinations <- merge(node_data_inventors_between_combinations_classes_ipc4,
                                                    classes_ipc4_combinations_and_clusters_0[,list(ipc_code= ipc_combination, tech_cluster_id)],
                                                    by="tech_cluster_id")
  inventor_movements_edge_data <- edge_data_between_combinations_classes_ipc4
  setkey(inventor_movements_edge_data,tech_cluster_id, next_tech_cluster_id)
  setkey(node_data_inventors_between_combinations,tech_cluster_id)
  
  
  #---- 
  #density clustering
  #--------------------------------------------      
  #install.packages("dbscan")
  library(dbscan)
  
  
  
  
  
  
  
  minimum_strength <- 0.11
  smallest_community <- 0.01
  
  
  agglomerating_small_nodes <- inventor_movements_edge_data[node_data_inventors_between_combinations[share_all_patents<smallest_community,list(tech_cluster_id)]]
  setkey(agglomerating_small_nodes, next_tech_cluster_id)
  agglomerating_small_nodes <- agglomerating_small_nodes[node_data_inventors_between_combinations[share_all_patents>=smallest_community,list(tech_cluster_id)]]
  setkey(agglomerating_small_nodes, tech_cluster_id,rank_nearness) 
  agglomerating_small_nodes[,min_rank_nearness := min(rank_nearness), by="tech_cluster_id"]
  agglomerating_small_nodes <- agglomerating_small_nodes[min_rank_nearness==rank_nearness,list(tech_cluster_id,next_tech_cluster_id) ]
  
  
  nodes_to_fuse <- inventor_movements_edge_data[node_data_inventors_between_combinations[share_all_patents>=smallest_community,list(tech_cluster_id)]]
  setkey(nodes_to_fuse, next_tech_cluster_id)
  nodes_to_fuse <- nodes_to_fuse[node_data_inventors_between_combinations[share_all_patents>=smallest_community,list(tech_cluster_id)]]  
  setkey(nodes_to_fuse, tech_cluster_id)
  nodes_to_fuse <- nodes_to_fuse[
    tech_cluster_id != next_tech_cluster_id][
      is.na(next_tech_cluster_id) == FALSE][
        movement_probability>minimum_strength][
          ,list(tech_cluster_id, next_tech_cluster_id)
          ]
  
  
  nodes_to_fuse
  agglomerating_small_nodes
  node_data_inventors_between_combinations[,community_density := tech_cluster_id]
  for (nr_of_row in 1:nrow(agglomerating_small_nodes)) {
    community_of_second_cluster_id <- node_data_inventors_between_combinations[tech_cluster_id==agglomerating_small_nodes[nr_of_row,next_tech_cluster_id],community_density]
    setkey(node_data_inventors_between_combinations, community_density)
    node_data_inventors_between_combinations[node_data_inventors_between_combinations[tech_cluster_id==agglomerating_small_nodes[nr_of_row,tech_cluster_id],list(community_density)],community_density:=community_of_second_cluster_id]
  }
  
  for (nr_of_row in 1:nrow(nodes_to_fuse)) {
    community_of_second_cluster_id <- node_data_inventors_between_combinations[tech_cluster_id==nodes_to_fuse[nr_of_row,next_tech_cluster_id],community_density]
    setkey(node_data_inventors_between_combinations, community_density)
    node_data_inventors_between_combinations[node_data_inventors_between_combinations[tech_cluster_id==nodes_to_fuse[nr_of_row,tech_cluster_id],list(community_density)],community_density:=community_of_second_cluster_id]
  }
  setkey(node_data_inventors_between_combinations, tech_cluster_id)
  
  community_size_density <- node_data_inventors_between_combinations[,
                                                                     list(size_community = sum(patents_in_node)), 
                                                                     by="community_density"][,
                                                                                             share_community := round(size_community/ node_data_inventors_between_combinations[1,all_patents],digits=5),][
                                                                                               order(-share_community)
                                                                                               ][,community_density_hlp:=seq_len(.N)][
                                                                                                 ,overcounting :=sum(share_community)]


  community_size_density[,too_small:=share_community<0.01]
  community_size_density[too_small==TRUE,community_density_hlp:=  community_size_density[too_small==FALSE,max(community_density_hlp)]]
  

  node_data_inventors_between_combinations <- merge(node_data_inventors_between_combinations,
                                                    community_size_density[,list(community_density,community_density_hlp)],
                                                    by="community_density")

  node_data_inventors_between_combinations[,community_density := NULL]

  setnames(node_data_inventors_between_combinations, "community_density_hlp", "community_density")

  
  community_size_density <- node_data_inventors_between_combinations[,
                                                                     list(size_community = sum(patents_in_node)), 
                                                                     by="community_density"][,
                                                                                             share_community := round(size_community/ node_data_inventors_between_combinations[1,all_patents],digits=5),][
                                                                                               order(-share_community)
                                                                                             ][,community_density_hlp:=seq_len(.N)][
                                                                                               ,overcounting :=sum(share_community)]
  
  
  fwrite(community_size_density, file = paste(path_to_output_data,"/",data_name,"/", "community_size_density.csv", sep=""))
  
  
  
  #----
  #Clustering walktrap
  #--------------------------   
  #install.packages("igraph")   
  library(igraph) # Load the igraph package 
  
  
  ipc_classes_network_complete <- graph_from_data_frame(inventor_movements_edge_data[is.na(next_tech_cluster_id)==FALSE ], vertices = node_data_inventors_between_combinations, directed=TRUE)
  
  
  E(ipc_classes_network_complete)$weight <-     inventor_movements_edge_data[is.na(next_tech_cluster_id)==FALSE,movement_probability]
  V(ipc_classes_network_complete)$size <- (node_data_inventors_between_combinations[,patents_in_node]/max(node_data_inventors_between_combinations[,patents_in_node])  )*40
  V(ipc_classes_network_complete)$label <- node_data_inventors_between_combinations[,ipc_code]
  V(ipc_classes_network_complete)$community_density <- node_data_inventors_between_combinations[,community_density]  
  
  #V(ipc_classes_network)$label_4 <-   node_data_inventors_between_combinations$next_tech_cluster_id  
  
  
  
  #cluster_membership_ipc_classes_4 <-         cluster_infomap(ipc_classes_network,  nb.trials = 100,
  #                                                            e.weights = edge_data_set_pooled[repeated_data == 0,N] , 
  #                                                            v.weights = nodes_size_ipc_4_network_pooled[,share_of_all_patents] ,
  #                                                            modularity = FALSE)
  
  #spinglass.community
  cluster_membership <-   cluster_walktrap(ipc_classes_network_complete, weights = E(ipc_classes_network_complete)$weight, 
                                           steps = 5,
                                           merges = TRUE, 
                                           modularity = TRUE, 
                                           membership = TRUE)
  
  cluster_membership
  
  
  #cluster_label_prop(ipc_classes_network)
  
  
  V(ipc_classes_network_complete)$membership  <- as.numeric(cluster_membership$membership)
  
  
  node_data_inventors_between_combinations[,community_walktrap:=NULL]
  node_data_inventors_between_combinations <- merge(node_data_inventors_between_combinations,
                                                    data.table(
                                                      tech_cluster_id          = as.numeric(V(ipc_classes_network_complete)$name),
                                                      community_walktrap       = as.numeric(V(ipc_classes_network_complete)$membership)
                                                    ),
                                                    by="tech_cluster_id",
                                                    all.x=TRUE
  )
  
  community_size_walktrap <- node_data_inventors_between_combinations[,
                                                                      list(size_community = sum(patents_in_node)), 
                                                                      by="community_walktrap"][,
                                                                                               share_community := round(size_community/  node_data_inventors_between_combinations[1,all_patents],digits=5),][
                                                                                                 order(share_community)
                                                                                                 ][,overcounting :=sum(share_community)]
  community_size_walktrap[,too_small:=share_community<0.01]
  community_size_walktrap[too_small==TRUE,community_walktrap:=  community_size_walktrap[too_small==FALSE,max(community_walktrap)]]
  
  fwrite(community_size_walktrap, file = paste(path_to_output_data,"/",data_name,"/", "community_size_walktrap.csv", sep=""))
  
  
  length((cluster_membership))
  is_hierarchical(cluster_membership)
  # 
  # ipc_combinations_and_clusters_1 <- unique(merge(ipc_combinations_and_clusters_0,
  #                                         data.table(
  #                                           tech_cluster_id = as.numeric(V(ipc_classes_network)$name),
  #                                           community       = as.numeric(V(ipc_classes_network)$membership)
  #                                         ),
  #                                         by="tech_cluster_id",
  #                                         all.x=TRUE
  #                                   )[
  #                                     is.na(community)       ,new_tech_cluster_id := tech_cluster_id][
  #                                     is.na(community)==FALSE,new_tech_cluster_id := max(tech_cluster_id), by="community"
  #                                     ][,list(ipc_combination, tech_cluster_id=new_tech_cluster_id)])
  #                             
  # 
  
  
  #--------------------------
  #visualize graph 
  #--------------------------   
  
  
  #select the data set to actually draw
  nodes_to_draw <- node_data_inventors_between_combinations[share_all_patents>0.015,list(tech_cluster_id)]
  
  ggplot(data=inventor_movements_edge_data[nodes_to_draw][movement_probability>0.01][
    ,list(tech_cluster_id, sum_probability)],
    aes(x=sum_probability)) + geom_histogram(bins=200)
  
  
  subgraph_to_draw <- induced_subgraph(ipc_classes_network_complete, as.character(nodes_to_draw$tech_cluster_id))
  subgraph_to_draw <- simplify(subgraph_to_draw, remove.multiple = FALSE, remove.loops = FALSE)
  
  set.seed(6547547)
  layout_in2d_space <- layout_with_fr(subgraph_to_draw)
  #layout_in2d_space <- layout_with_kk(ipc_classes_network_graph)
  
  
  
  #Color scaling function
  c_scale <- colorRamp(c('white','cyan','blue','darkblue','darkblue','black','black','black','black','black','black','black','black','black','black','black','black','black','black','black','black','black'))
  
  
  #"rgb(174, 198, 207)","rgb(119, 221, 119)", "rgb(110,54,42)","rgb(32,12,37)","rgb(0,0,0)","rgb(0,0,0)")
  
  
  colors_communities <- adjustcolor(c('black','orange','cyan','yellow','darkgreen'))
  
  #Applying the color scale to edge weights.
  #rgb method is to convert colors to a character vector.
  E(subgraph_to_draw)$color = apply(c_scale(E(subgraph_to_draw)$weight/max(E(subgraph_to_draw)$weight)  ), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255) )
  E(subgraph_to_draw)$width = E(subgraph_to_draw)$weight/max(E(subgraph_to_draw)$weight)*3
  
  #ggplot(data=as.data.table(E(ipc_classes_network)$weight/max(E(ipc_classes_network)$weight)),aes(x=V1)) + geom_histogram()
  
  #V(subgraph_to_draw)$color = as.numeric(V(subgraph_to_draw)$membership)
  
  
  plot(
    subgraph_to_draw, 
    vertex.color=colors_communities[as.numeric(V(subgraph_to_draw)$membership)],
    layout=layout_in2d_space,
    vertex.label = V(subgraph_to_draw)$label
  )  
  
  node_data_inventors_between_combinations[ipc_code=="G01N"|ipc_code == "A61B"]
  inventor_movements_edge_data[(tech_cluster_id==60 | tech_cluster_id == 525) & (next_tech_cluster_id==60 | next_tech_cluster_id == 525)]
  node_data_inventors_between_combinations[ipc_code=="H04N"|ipc_code == "G06F"]
  inventor_movements_edge_data[(tech_cluster_id==555  | tech_cluster_id == 635) & (next_tech_cluster_id==555  | next_tech_cluster_id == 635)]
  
  plot(
    subgraph_to_draw, 
    vertex.color=as.numeric(V(subgraph_to_draw)$community_density),
    layout=layout_in2d_space,
    vertex.label = V(subgraph_to_draw)$label
  )  
  
  
  
  #--------------------------
  #save information communities 
  #--------------------------

  communities_ipc_classes <- data.table(
    ipc_class_symbol_4 = vertex_attr(ipc_classes_network_complete,"label"),
    community = V(ipc_classes_network_complete)$community_density
  )
  
  communities_ipc_classes[,.N,by="ipc_class_symbol_4"][,.N,by="N"]
  
  fwrite(communities_ipc_classes, file = paste(path_to_output_data,"/",data_name,"/", "communities_ipc_classes.csv", sep=""))
  
  
  # rm(data_graph_inventor_movements_patents_classes_ipc4,data_graph_inventor_movements_patents_classes_ipc8,
  #    data_graph_inventor_movements_patents_ipc4, data_graph_inventor_movements_patents_ipc8,
  #    data_graph_inventor_movements_patents_original_classes_ipc4, data_graph_inventor_movements_patents_original_classes_ipc8,
  #    data_graph_inventor_movements_patents_original_ipc4, data_graph_inventor_movements_patents_original_ipc8)
  # rm(data_graph_knn_distance)
  # rm(edge_data_between_combinations_classes_ipc4, edge_data_between_combinations_classes_ipc8,
  #    edge_data_between_combinations_ipc4, edge_data_between_combinations_ipc8)
  # rm(agglomerating_small_nodes,nodes_to_draw, nodes_to_fuse,layout_in2d_space)
  # rm(community_size_density, community_size_walktrap,communities_ipc_classes)
  # rm(node_data_inventors_between_combinations, node_data_inventors_between_combinations_classes_ipc4,
  #    node_data_inventors_between_combinations_classes_ipc8, node_data_inventors_between_combinations_ipc4,
  #    node_data_inventors_between_combinations_ipc8)
  gc()
  
  
  
}

