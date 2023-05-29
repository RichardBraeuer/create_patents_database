###############################################
#+++++++++find_ipc_class_communities++++++++++#
###############################################

suppressWarnings({
dir.create(paste0(path_to_output_data,"/",data_name,"/"))
dir.create(paste0(path_to_output_data,"/",data_name,"/data_description/"))
dir.create(paste0(path_to_output_data,"/",data_name,"/data_description/ipc_communities/"))
})

if (find_ipc_class_communites =="YES"){
  
  
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
  inventors_of_patents <- inventors_of_patents[team_size_patent==1]
  
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
                                #all.x=TRUE,
                                allow.cartesian=TRUE)
  
  ipc_classes_per_patent_inventor[,random:=runif(nrow(ipc_classes_per_patent_inventor),0,1)]
  setkeyv(ipc_classes_per_patent_inventor, 
          c(id_variable_to_use,"year","random"))
  ipc_classes_per_patent_inventor[is.na(ipc_class_symbol),ipc_class_symbol:="0000"]
  }
  #---#---#---#---#---#---#
  #Density community finding
  #---#---#---#---#---#---#
  {
  #Count the number of inventors and patents
  nr_all_patents <-   length(unique(ipc_classes_per_patent_inventor[,docdb_family_id]))
  nr_all_inventors <- nrow(unique(ipc_classes_per_patent_inventor[,c(id_variable_to_use),with=FALSE]))
    
  #If the share of inventors who also patent in the second class is higher than x, they will be merged
  minimum_strength <- 0.33
  #communities who are too small get merged to the nearest community no matter what
  smallest_community <- 0.02
    
    
  #set up density variable
  ipc_classes_per_patent_inventor[,community_density := ipc_class_symbol]
  ipc_classes_per_patent_inventor[,inv_community_share :=.N/nr_of_patents_inventor[[1]] ,by=c(id_variable_to_use,"community_density")]
  strongest_link <- 1
  large_loop_count <- 0
  while  (strongest_link>minimum_strength) {

      print(paste0("reset edges ",large_loop_count," times"))

  
    #----------------
    #Create node data
    #----------------
    {
    #copy of the original data to work with and merge
    #with the encoded ipc combinations
    
  
    node_data <- ipc_classes_per_patent_inventor[,list(inventors_in_node=uniqueN(get(id_variable_to_use)),
                                                       nr_inv_records=.N,
                                                       nr_of_patents=uniqueN(docdb_family_id),
                                                       nr_inv_share=sum(inv_community_share,na.rm=TRUE)),
                                                 by="community_density"]
    setkey(node_data,inventors_in_node)
    node_data[,share_all_inventors:=inventors_in_node/nr_all_inventors]
    node_data[,share_all_patents:=nr_of_patents/nr_all_patents]
    node_data[,valid_community:=share_all_inventors>=smallest_community]

    }
    #----------------
    #Create edge data
    #----------------
    {
      edge_data_all <- merge(
      #select all inventors that have a patent with the current ipc class
      ipc_classes_per_patent_inventor[,list(share=.N/nr_of_patents_inventor[[1]]),by=c("community_density",id_variable_to_use)],
      ipc_classes_per_patent_inventor[,list(share=.N/nr_of_patents_inventor[[1]]),by=c("community_density",id_variable_to_use)],
      by=id_variable_to_use,
      allow.cartesian=TRUE
    )[,list(N=sum(share.x*share.y)),by=c("community_density.x","community_density.y")][community_density.x !=community_density.y]
      
    setnames(edge_data_all,
             old=c("community_density.x","community_density.y"),
             new=c("community_density","next_community_density"))
  
    edge_data_all <- merge(edge_data_all,
                       node_data[,list(community_density, share_all_inventors, inventors_in_node,nr_inv_share)],
                       by="community_density")
    edge_data_all <- merge(edge_data_all,
                       node_data[,list(next_community_density=community_density, 
                                       next_share_all_inventors=share_all_inventors, 
                                       next_inventors_in_node=inventors_in_node,
                                       next_nr_inv_share = nr_inv_share)],
                       by="next_community_density")
    edge_data <-edge_data_all[share_all_inventors <=next_share_all_inventors ]
    setkey(edge_data,community_density,next_community_density)
  
    edge_data[,movement_probability := (N)/((nr_inv_share))]
    edge_data[,distance_measure := 1-movement_probability]
    
    
    setkey(edge_data,community_density, distance_measure,next_share_all_inventors)
    edge_data[community_density!=next_community_density
              ,rank_nearness := seq_len(.N),by="community_density"]
    
    
    }
    
    
    #----------------
    #report status
    #----------------
    {
      print(paste("number of communities left:", node_data[,uniqueN(community_density)]))
      print(paste("strongest link:", strongest_link))
      print(paste("size largest community:", max(node_data[,share_all_inventors])))
    }
    
    
    #----------------
    #Fuse communities
    #----------------
    {
    #sort nodes according to link priority
    #idea is to do the unambigious merges first
    #i.e. very small communities and very strongly connected ones
    #They are sorted to the beginning of the nodes data set 
    node_data <- merge(node_data,
                         edge_data[,list(max_con=-max(movement_probability,na.rm=TRUE)),by="community_density"],
                         by="community_density",
                         all.x = TRUE)
    setkey(node_data,valid_community,max_con)
    
    stop_loop <- FALSE
    current_row <- 1
    node_data[,new_community_density:=community_density]
    while (stop_loop==FALSE & current_row < nrow(node_data)){
      old_community<-node_data[current_row,community_density]
      print(paste("fusing:",old_community))
      connected_ipc_class<-edge_data[old_community==community_density&rank_nearness==1&(movement_probability >minimum_strength & 
                                                                                          share_all_inventors >smallest_community& 
                                                                                          movement_probability <minimum_strength*2),next_community_density]
      if (length(connected_ipc_class)>0){
        stop_loop <- TRUE
      }
      connected_ipc_class<-edge_data[node_data[current_row,community_density]==community_density&rank_nearness==1&(movement_probability >minimum_strength 
                                                                                                                   |share_all_inventors <smallest_community
                                                                                                                   ),next_community_density]
      if (length(connected_ipc_class)>0){
        new_community <- node_data[connected_ipc_class==community_density,new_community_density]
        node_data[new_community_density==old_community,new_community_density:=new_community]
      }

      current_row <- current_row + 1
      rm(connected_ipc_class)
    }
    
    
    large_loop_count <-   large_loop_count + 1
    strongest_link <- max(edge_data[,movement_probability ])
    
    
    ipc_classes_per_patent_inventor <- merge(ipc_classes_per_patent_inventor,
                                             node_data[,list(community_density,new_community_density)],
                                             by="community_density")
    ipc_classes_per_patent_inventor[,community_density:=new_community_density]
    ipc_classes_per_patent_inventor[,new_community_density:=NULL]                     
    
  
    
    }
    

    
  }
  
  
  community_size_density <- unique(ipc_classes_per_patent_inventor[,list(ipc_class_symbol,community_density)])
  
      
  
  community_size_density <- merge(community_size_density,
                                  ipc_classes_per_patent_inventor[,list(nr_of_inventors=uniqueN(inventor_id)),by="community_density"],
                                  by="community_density")
  community_size_density[,share_community := -round(nr_of_inventors/nr_all_inventors,digits=5),]
  setkey(community_size_density,share_community)
  community_size_density[,share_community :=-share_community]
  community_size_density[,community_density_hlp:=as.numeric(seq_len(.N)==1),by="community_density"]
  community_size_density[,overcounting :=sum(share_community*community_density_hlp)]
  community_size_density[,community_density_hlp:=cumsum(community_density_hlp)]
  setkey(community_size_density,share_community)
  
  
  fwrite(community_size_density, file = paste(path_to_output_data,"/",data_name,"/data_preparation/", "community_size_density.csv", sep=""))
  }
  
  #----------------
  #graph results
  #----------------
  {
    
    #----------------
    #Create node data
    #----------------
    {
      #copy of the original data to work with and merge
      #with the encoded ipc combinations
      
      
      node_data_ipc <- ipc_classes_per_patent_inventor[,list(inventors_in_node=uniqueN(get(id_variable_to_use)),
                                                         nr_inv_records=.N,
                                                         nr_of_patents=uniqueN(docdb_family_id),
                                                         nr_inv_share=sum(inv_community_share,na.rm=TRUE)),
                                                   by="ipc_class_symbol"]
      setkey(node_data_ipc,inventors_in_node)
      node_data_ipc[,share_all_inventors:=inventors_in_node/nr_all_inventors]
      node_data_ipc[,share_all_patents:=nr_of_patents/nr_all_patents]
      node_data_ipc[,valid_community:=share_all_inventors>=smallest_community]
      
    }
    #----------------
    #Create edge data
    #----------------
    {
      edge_data_ipc_all <- merge(
        #select all inventors that have a patent with the current ipc class
        ipc_classes_per_patent_inventor[,list(share=.N/nr_of_patents_inventor[[1]]),by=c("ipc_class_symbol",id_variable_to_use)],
        ipc_classes_per_patent_inventor[,list(share=.N/nr_of_patents_inventor[[1]]),by=c("ipc_class_symbol",id_variable_to_use)],
        by=id_variable_to_use,
        allow.cartesian=TRUE
      )[,list(N=sum(share.x*share.y)),by=c("ipc_class_symbol.x","ipc_class_symbol.y")][ipc_class_symbol.x !=ipc_class_symbol.y]
      
      setnames(edge_data_ipc_all,
               old=c("ipc_class_symbol.x","ipc_class_symbol.y"),
               new=c("ipc_class_symbol","next_ipc_class_symbol"))
      
      edge_data_ipc_all <- merge(edge_data_ipc_all,
                             node_data_ipc[,list(ipc_class_symbol, share_all_inventors, inventors_in_node,nr_inv_share)],
                             by="ipc_class_symbol")
      edge_data_ipc_all <- merge(edge_data_ipc_all,
                             node_data_ipc[,list(next_ipc_class_symbol=ipc_class_symbol, 
                                             next_share_all_inventors=share_all_inventors, 
                                             next_inventors_in_node=inventors_in_node,
                                             next_nr_inv_share = nr_inv_share)],
                             by="next_ipc_class_symbol")
      edge_data_ipc <-edge_data_ipc_all[share_all_inventors <=next_share_all_inventors ]
      setkey(edge_data_ipc,ipc_class_symbol,next_ipc_class_symbol)
      
      edge_data_ipc[,movement_probability := (N)/((nr_inv_share))]
      edge_data_ipc[,distance_measure := 1-movement_probability]
      
      
      setkey(edge_data_ipc,ipc_class_symbol, distance_measure,next_share_all_inventors)
      edge_data_ipc[ipc_class_symbol!=next_ipc_class_symbol
                ,rank_nearness := seq_len(.N),by="ipc_class_symbol"]
      
      
    }
    
  data_graph_knn_distance <- rbindlist(list(
    edge_data_ipc[rank_nearness<=1,list(neighbor = "nearest"  , inventors_in_node  , mean_distance=mean(distance_measure), mean_movement_probability=mean(movement_probability)),by="ipc_class_symbol"],
    edge_data_ipc[rank_nearness<=2,list(neighbor = "nearest 2", inventors_in_node  , mean_distance=mean(distance_measure), mean_movement_probability=mean(movement_probability)),by="ipc_class_symbol"],
    edge_data_ipc[rank_nearness<=3,list(neighbor = "nearest 3", inventors_in_node  , mean_distance=mean(distance_measure), mean_movement_probability=mean(movement_probability)),by="ipc_class_symbol"]
  ))[
    inventors_in_node <100,size_class := "small"][
      inventors_in_node >=100,size_class := "above 100 patents"]
  library(ggplot2)
  ggplot(data=data_graph_knn_distance[], aes(x=mean_distance, group= neighbor, fill= neighbor))+
    geom_histogram(aes(y=..density..) , bins=100) +
    facet_grid(size_class~ .) +
    ggtitle(paste("Avg. Distance to Nearest Other Clusters:",node_variable))
  ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/ipc_communities/", "distance_to_nearest_clusters_", node_variable, ".pdf" , sep = ""))
  
  
  ggplot(data=data_graph_knn_distance, aes(x=mean_distance, color=neighbor, group= neighbor, fill= neighbor))+
    geom_step(stat="ecdf") +
    facet_grid(size_class~ .) +
    ggtitle(paste("Cdf of avg. Distance to Nearest Other Clusters:",node_variable))
  ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/ipc_communities/", "cdf_distance_to_nearest_clusters_", node_variable, ".pdf" , sep = ""))
  
  
  ggplot(data=data_graph_knn_distance, aes(x=mean_movement_probability, group= neighbor, fill= neighbor))+
    geom_histogram(aes(y=..density..) , bins=100) +
    facet_grid(size_class~ .) +
    ggtitle(paste("Avg. Movement Probabilities to Nearest Other Clusters:",node_variable))
  ggsave(file=paste(path_to_output_data,"/",data_name,"/data_description/ipc_communities/", "movement_probability_to_nearest_clusters_", node_variable, ".pdf" , sep = ""))
  
  
  
  
  
  
  library(igraph) # Load the igraph package 
  
  
  # load information about nodes
  #---#---#---#---#---#---#---#
  node_data_ipc <- merge(node_data_ipc,
                     fread(file = paste(path_to_output_data,"/",data_name,"/", "community_size_density.csv", sep="")),
                     by="ipc_class_symbol")
  node_data_ipc[,label_ipc_class_symbol:=NULL]
  node_data_ipc[share_all_inventors>0.5,label_ipc_class_symbol:=ipc_class_symbol]
  node_data_ipc[is.na(label_ipc_class_symbol),label_ipc_class_symbol:=""]
  
  
  
  
  
  # actually draw the plot
  #---#---#---#---#---#---#---#
  smalles_size_to_draw <- 0.015
  
  library(randomcoloR)
 
  
  #mark which nodes get color
  node_data_ipc[,nr_of_drawn_communties:=sum(as.numeric(share_all_inventors>smalles_size_to_draw)),by="community_density_hlp"]
  node_data_ipc[nr_of_drawn_communties>1,node_color_num:=community_density_hlp]
  node_data_ipc[,color:=NULL]
  nr_of_colors_needed <-node_data_ipc[nr_of_drawn_communties>1,uniqueN(node_color_num)]
  
  community_colors = data.table(node_color_num=unique(node_data_ipc[nr_of_drawn_communties>1,node_color_num]),
                                color=  distinctColorPalette(nr_of_colors_needed)
                                )
  node_data_ipc <- merge(node_data_ipc,
                           community_colors,
                         by="node_color_num",
                         all.x=TRUE)
  
  node_data_ipc[is.na(color),color:="white"]
  node_data_ipc[,node_color_num:=NULL]
  
  setkey(node_data_ipc,ipc_class_symbol)
  
  edge_data_ipc_to_draw <- edge_data_ipc[rank_nearness<4 #& movement_probability >0.1 
                                           ]
  ipc_classes_network_complete <- graph_from_data_frame(edge_data_ipc_to_draw,
                                                        vertices = node_data_ipc, directed=FALSE)
  
  
  E(ipc_classes_network_complete)$N <-     edge_data_ipc_to_draw[,N]
  E(ipc_classes_network_complete)$weight <-     edge_data_ipc_to_draw[,movement_probability]
  E(ipc_classes_network_complete)$prob <-     edge_data_ipc_to_draw[,movement_probability]
  V(ipc_classes_network_complete)$size <- ((node_data_ipc[,share_all_patents]  )^(0.4))*40
  V(ipc_classes_network_complete)$label <- node_data_ipc[,label_ipc_class_symbol]
  V(ipc_classes_network_complete)$community_density <- node_data_ipc[,community_density_hlp ]  
  
  #select the data set to actually draw
  nodes_to_draw <- node_data_ipc[share_all_inventors>smalles_size_to_draw,list(ipc_class_symbol)]
  node_data_ipc[nodes_to_draw]
  
  subgraph_to_draw <- induced_subgraph(ipc_classes_network_complete, as.character(nodes_to_draw$ipc_class_symbol))
  subgraph_to_draw <- simplify(subgraph_to_draw, remove.multiple = FALSE, remove.loops = FALSE)
  
  #set.seed(6547547)
  layout_in2d_space <- layout_with_fr(subgraph_to_draw)
  # 
  # ,
  #                                             minx=rep(-5,nrow(nodes_to_draw)),
  #                                             maxx=rep(5,nrow(nodes_to_draw)),
  #                                             miny=rep(-60,nrow(nodes_to_draw)),
  #                                             maxy=rep(60,nrow(nodes_to_draw))
  #                                             )

  #layout_in2d_space <- layout.fruchterman.reingold(subgraph_to_draw)

  #layout_with_fr(subgraph_to_draw)
  #layout_in2d_space <- layout_with_kk(subgraph_to_draw)
  
  
  
  #Color scaling function
  c_scale <- colorRamp(c('white','cyan','blue','darkblue','black'))
  
  
  #"rgb(174, 198, 207)","rgb(119, 221, 119)", "rgb(110,54,42)","rgb(32,12,37)","rgb(0,0,0)","rgb(0,0,0)")
  
  
  # colors_communities <- adjustcolor(c('black','orange','cyan','yellow','darkgreen',
  #                                     'steelblue','red','gold','gray','lightblue',
  #                                     'darkblue','lightgreen'))
  # 
  #Applying the color scale to edge weights.
  #rgb method is to convert colors to a character vector.
  E(subgraph_to_draw)$color = apply(c_scale((E(subgraph_to_draw)$prob/max(E(subgraph_to_draw)$prob))^0.6  ), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255) )
  E(subgraph_to_draw)$width = E(subgraph_to_draw)$N/max(E(subgraph_to_draw)$N)*3
  
  #ggplot(data=as.data.table(E(ipc_classes_network)$weight/max(E(ipc_classes_network)$weight)),aes(x=V1)) + geom_histogram()
  
  #V(subgraph_to_draw)$color = as.numeric(V(subgraph_to_draw)$membership)
  
  par(mar=c(0,0,0,0)+.1)
  plot(
    subgraph_to_draw, 
    vertex.color=node_data_ipc[nodes_to_draw,color],
    #vertex.color=colors_communities[as.numeric(V(subgraph_to_draw)$community_density)],
    layout=layout_in2d_space,
    #margin=c(0.0000000000000001)
    #vertex.label = V(subgraph_to_draw)$label
  )  
  
  }
  
  
  final_communities <- fread(file = paste(path_to_output_data,"/",data_name,"/data_preparation/", "community_size_density.csv", sep=""))
  setnames(final_communities,
           old=c("community_density","ipc_class_symbol"),
           new=c("community",node_variable))

  
  fwrite(unique(final_communities[,c("community",node_variable),with=FALSE]),
         file = paste(path_to_output_data,"/",data_name,"/data_preparation/", "communities_ipc_classes.csv", sep=""))
  
  
}