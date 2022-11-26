match_in_block <- function(block_value,block_var,data_set_1,data_set_2) {
  
  print(eval(block_value))
  nrow_names_data_1 <- nrow(data_set_1[block_value])
  nrow_names_data_2 <- nrow(data_set_2[block_value])
  if  (nrow_names_data_1>0 & nrow_names_data_2 >0){
    
    
    
    merged_block <-  merge(data_set_1[block_value],
                           data_set_2[block_value],
                           by=block_var,
                           allow.cartesian = TRUE)
    
    
    
    merged_block[,str_dist:=stringdist(get(varname_string_data_1),
                                       get(varname_string_data_2),
                                       method = 'lcs')]
    merged_block[,str_dist_j:=stringdist(get(varname_string_data_1),
                                         get(varname_string_data_2),
                                         method='jaccard', 
                                         q=3)]
    
    return(merged_block)
    
    
  }else{
    print(paste("nrow_names_data_1:",nrow_names_data_1,"nrow_names_data_2",nrow_names_data_2))
    return(NULL)
  }
  
  
}




fuzzy_name_match <- 
  function(data_set_1,
           data_set_2,
           varname_string_data_1,
           varname_id_data_1,
           varname_string_data_2,
           varname_id_data_2,
           varname_string_block_var_both){
    
    
    #to shorten running time, only take the unique names, ignoring
    #the rest of the data for now
    list_strings_1 = unique(data_set_1[,c(varname_string_data_1,varname_string_block_var_both),with=FALSE])
    list_strings_2 = unique(data_set_2[,c(varname_string_data_2,varname_string_block_var_both),with=FALSE])
    
    list_strings_1[!is.na(county)]
    list_strings_2[!is.na(county)]
    #create a data set of all string comparisons "allowed"
    # if there was no block variable, this would just be
    # a dataset of n_1 * n_2 rows with every possible 
    # combination of strings
    if ((list_strings_1[,typeof(varname_string_block_var_both)] != list_strings_2[,typeof(varname_string_block_var_both)]) | list_strings_1[,typeof(varname_string_block_var_both)] != "character"){
      print("string expected in block variable")
    }
    
    
    library(stringdist)
    setkeyv(list_strings_1,varname_string_block_var_both)
    setkeyv(list_strings_2,varname_string_block_var_both)
    
    
    list_of_blocks <-merge(list_strings_1[,list(N_1=.N),by=varname_string_block_var_both],
                           list_strings_2[,list(N_2=.N),by=varname_string_block_var_both],
                           by=varname_string_block_var_both
    )[get(varname_string_block_var_both)!=""]
    list_of_blocks[,size_block:=N_1*N_2]
    
    nr_smallest_block <- which(list_of_blocks[,size_block==min(size_block)])[1]
    
    
    time <- proc.time()
    
    
    match_in_block(block_value=list_of_blocks[nr_smallest_block,varname_string_block_var_both,with=FALSE],
                   block_var=varname_string_block_var_both,
                   data_set_1=list_strings_1,
                   data_set_2=list_strings_2)
    
    
    time_smallest_block <- proc.time() - time
    
    
    
    print(paste("Fuzzy Name Matching. Started within blocks", format(Sys.time(), "%b %d %X %Y"), "Expected minutes of runtime:",round((time_smallest_block[3]*list_of_blocks[,sum(size_block)]/list_of_blocks[nr_smallest_block,size_block])/60,digits=2)))
    
    
    
    results_within_blocks <- rbindlist(lapply(X=list_of_blocks[,varname_string_block_var_both,with=FALSE][,1],
                                              FUN=match_in_block,
                                              block_var=varname_string_block_var_both,
                                              data_set_1=list_strings_1,
                                              data_set_2=list_strings_2))
    
    
    print(paste("Finished Fuzzy Name Matching within blocks:", format(Sys.time(), "%b %d %X %Y")))
    
    
    
    
    list_strings_1[!list_of_blocks,relevant:=TRUE]
    list_strings_2[,relevant:=TRUE]
    
    setkeyv(list_strings_1,"relevant")
    setkeyv(list_strings_2,"relevant")
    
    
    results_without_block <- match_in_block(block_value=data.table(relevant=TRUE),
                                            block_var="relevant",
                                            data_set_1=list_strings_1,
                                            data_set_2=list_strings_2)
    
    
    
    print(paste("Finished Fuzzy Name Matching outside of blocks:", format(Sys.time(), "%b %d %X %Y")))
    
    
    match_result <-rbindlist(list(
      results_within_blocks[,list(pure_name,pure_name_reg,str_dist, str_dist_j,block=get(varname_string_block_var_both))],
      results_without_block[,list(pure_name,pure_name_reg,str_dist, str_dist_j,block="")])
    )
    
    
    
    
    
    setkey(match_result,pure_name,str_dist_j)
    
    
    match_result[,length_1:=nchar(get(varname_string_data_1))]
    match_result[,length_2:=nchar(get(varname_string_data_2))]
    
    #match_result[length_1<=length_2,shortname:=get(varname_string_data_1)]
    #match_result[length_1>length_2,shortname:=get(varname_string_data_2)]
    #match_result[length_1<=length_2,longname:=get(varname_string_data_2)]
    #match_result[length_1>length_2,longname:=get(varname_string_data_1)]
    match_result[length_1<=length_2,length_shortname:=length_1]
    match_result[length_1>length_2,length_shortname:=length_2]
    match_result[length_1<=length_2,length_longname:=length_2]
    match_result[length_1>length_2,length_longname:=length_1]
    
    
    match_result[,str_same_letters:=(str_dist-(length_longname-length_shortname)-length_shortname)]
    match_result[,wrong_percent:=(str_dist-(length_longname-length_shortname))/length_shortname]
    
    
    #mark the match results according to security
    #---#---#---#---#---#---#---#---#---#---#---#
    #threshold for those in the same block
    match_result[block!="" & ((str_dist_j<0.8 |(wrong_percent <0.3 &str_dist <15)  )& length_shortname >2),
                 merge_type:="4low"]
    match_result[block!="" & ((str_dist_j<0.7 |(wrong_percent <0.2 & str_dist <10) )& length_shortname >5),
                 merge_type:="5middle"]
    match_result[block!="" & ((str_dist_j<0.6 |(wrong_percent <0.1 & str_dist <10) )& length_shortname >7),
                 merge_type:="6high"]
    
    #threshold for those in unidentified block
    match_result[block=="" & (str_dist_j<0.7 | (wrong_percent <0.2&str_dist <10) & length_shortname >5),
                 merge_type:="4low"]
    match_result[block=="" & (str_dist_j<0.6 | (wrong_percent <0.1&str_dist <10) & length_shortname >7),
                 merge_type:="5middle"]
    match_result[block=="" & (str_dist_j<0.5 | (wrong_percent <0.1&str_dist <5) & length_shortname >10),
                 merge_type:="6high"]
    
    match_result[is.na(merge_type),merge_type:="3tried_to_merge"]
    
    
    
    
    
    potential_matches <- match_result[merge_type!="3tried_to_merge"]
    
    
    potential_matches[,criterium:=str_dist_j+wrong_percent]
    
    
    setkeyv(potential_matches,c(varname_string_data_1,"criterium"))
    
    potential_matches[, change_crit:=  criterium-shift(criterium), by=varname_string_data_1]
    potential_matches[is.na(change_crit),change_crit:=0]
    potential_matches[, change_crit:=  cumsum(change_crit), by=varname_string_data_1]
    
    
    
    print(paste("Selected most promising", format(Sys.time(), "%b %d %X %Y")))
    
    
    match_result <- potential_matches[change_crit<0.25]
    
    match_result <- unique(match_result[,list(criterium=mean(criterium),
                                              merge_type=max(merge_type),
                                              str_dist_j=mean(str_dist_j),
                                              str_dist=mean(str_dist),
                                              length_shortname=mean(length_shortname),
                                              wrong_percent=mean(wrong_percent)),
                                        by=c("block",varname_string_data_1,varname_string_data_2)])
    
    
    
    
    
    setnames(match_result,
             old="block",
             new = varname_string_block_var_both)
    
    merged_data <- merge(match_result,
                         unique(data_set_1[,c(varname_id_data_1,varname_string_data_1),with=FALSE]),
                         by=c(varname_string_data_1),
                         allow.cartesian = TRUE,
                         all.x=TRUE)
    merged_data <- merge(merged_data,
                         unique(data_set_2[,c(varname_id_data_2,varname_string_data_2),with=FALSE]),
                         by=c(varname_string_data_2),
                         allow.cartesian = TRUE,
                         all.x=TRUE)
    
    #match_result[pure_name_reg=="carlzeissjena"]
    #data_set_1[pure_name=="*carlzeissjena"]
    #data_set_2[pure_name_reg=="carlzeissjena"]
    #merged_data[pure_name=="*carlzeissjena"]
    
    return(merged_data)
    
    
  }

