###############################################
#+++++++match top inventors wikipedia+++++++++#
###############################################

# The purpose of this file is twofold: 
#
#First, to assess how common every 
# word is and thus how descriptive it is of specific inventors:
# E.g. two patents of "John Smith" are likely different people,
# but two patents by "Quirin Theodore Vanderbilt" are likely to be
# from the same inventor, because not many people have that name

#Second: This is a prerequisite from filtering out titles, prefixes
# and other words that do not distinguish inventors. E.g. "Dr. John
# Smith" and "John Smith" might be the same person, same with "John
# Smith Junior". To find these words, one has to split the names into
# words as well. In the next step, I manually expect very frequent words
# to find these words, use lists from exogenous sources etc. I then
# put the names together without these title-words.
if (create_list_of_names=="YES"){
  
#generate a list of all the words within every inventor name
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
{
#This is the first chapter where we generate a list of words for every 
# inventor name
  
  #read in names lexikon
  names_observed_patstat      <- unique(fread(file=paste(path_orig_patent_data,"data/lexicon_inv.csv",sep=""))[,list(inv_name, inv_eee_hrm_id)])
  names_observed_patstat      <- names_observed_patstat[names_observed_patstat[, .I[1], by = inv_eee_hrm_id]$V1]
  
  areas_inventors_are_active_in  <- unique(fread(file = paste(path_orig_patent_data, "/data/", "patents_inv",".csv", sep=""))[,list(inv_eee_hrm_id,appln_auth)])
  
  
  
  names_observed_patstat_first_last <- unique(rbindlist(list(
    names_observed_patstat[grepl(inv_name, pattern=", ",fixed=TRUE),
                         list(inv_eee_hrm_id,str_split_fixed(inv_name, pattern = ", ", n = 2))][,
                               list(inv_eee_hrm_id,firstname=V2,lastname=V1)]
    ,
    names_observed_patstat[(grepl(inv_name, pattern=", ",fixed=TRUE)==FALSE),
                         list(inv_eee_hrm_id,str_split_fixed(inv_name, pattern = " ", n = 2))][,
                              list(inv_eee_hrm_id,firstname=V1,lastname=V2)]
  )))
  
  DT.m1 = melt(names_observed_patstat_first_last,
               id.vars = c("inv_eee_hrm_id"),
               measure.vars = c("firstname", "lastname"))
  setnames(DT.m1,"variable","name_type")
  setnames(DT.m1,"value","inv_name")
  setkey(DT.m1, inv_eee_hrm_id,name_type,inv_name)
  
  list_of_name_components <- extract_list_of_name_components(
    data_table_of_names= DT.m1,
    id_variable = c("inv_eee_hrm_id","name_type"),
    variable_containing_names = "inv_name"
  )
  
  names_observed_patstat <- merge(names_observed_patstat,
                                  list_of_name_components,
                                  by="inv_eee_hrm_id")
  
  
  
  
  word_frequency_table      <- unique(merge(
    list_of_name_components,
    areas_inventors_are_active_in,
    by="inv_eee_hrm_id")
  )        
  
  setkey(word_frequency_table,inv_eee_hrm_id,name_type,nr_word )
  
  word_frequency_table[,frequency:=uniqueN(inv_eee_hrm_id),by=c("name_component","appln_auth")]
  word_frequency_table[,mentions_of_country := uniqueN(inv_eee_hrm_id), by=c("appln_auth") ]
  word_frequency_table[,probability_name_component := frequency/mentions_of_country, ]
  
  nrow(unique(word_frequency_table[,list(inv_eee_hrm_id)]))
  
  
  setkey(word_frequency_table,appln_auth,inv_eee_hrm_id,probability_name_component)
  view_data <-unique(word_frequency_table[appln_auth=="GR",list(name_component,appln_auth,frequency,mentions_of_country,probability_name_component)])[order(-probability_name_component)]
  view_data[name_component=="john"]
  unique(word_frequency_table[,list(appln_auth)])
  
  fwrite(word_frequency_table, file=paste(path_orig_patent_data,"data/data_preparation/name_frequency_table.csv",sep=""))
  
  word_frequency_table <- fread( file=paste(path_orig_patent_data,"data/data_preparation/name_frequency_table.csv",sep=""))
  
  
  setkey(word_frequency_table,appln_auth,inv_eee_hrm_id,probability_name_component)
  view_data <-unique(word_frequency_table[appln_auth=="DE",list(name_component,appln_auth,frequency,mentions_of_country,probability_name_component)])[order(-probability_name_component)]
  view_data[name_component=="john"]
  unique(word_frequency_table[,list(appln_auth)])
}


#mark words that should not show up in the clean names
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
{

#---------------------------------
#Merge information on the frequency of names
#---------------------------------  


#load inventor name frequency
list_of_name_component_frequencies <- unique(fread( file=paste(path_orig_patent_data,"data/data_preparation/name_frequency_table.csv",sep=""),
                                                    encoding = "UTF-8")[,list(inv_eee_hrm_id,highest_probability_name_component=max(probability_name_component)),by = "name_component" ] ) 
setkey(list_of_name_component_frequencies,inv_eee_hrm_id)


#merge information on frequency to data
names_observed_patstat <- merge(names_observed_patstat,
                                unique(unique(list_of_name_component_frequencies[,list(name_component,highest_probability_name_component)])),
                                by = c("name_component"),
                                all.x = TRUE
)
names_observed_patstat[inv_eee_hrm_id==655|5660==inv_eee_hrm_id|35340571==inv_eee_hrm_id|5657==inv_eee_hrm_id]


#---------------------------------
#Manually correct most common words in names
#---------------------------------  


#Select the most common words
list_of_very_frequent_words <- unique(
  list_of_name_component_frequencies[highest_probability_name_component > 0.001,list(name_component,highest_probability_name_component)  ])[
    order(-highest_probability_name_component)][,is_title:=0]


#write them to disk to manually check for titles and other undesirables
fwrite(list_of_very_frequent_words, file=paste(path_orig_patent_data, "/data/data_preparation/", "most_frequent_words_patstat_automatic",".csv", sep=""))
rm(list_of_very_frequent_words)
#look up the occurences of some components manually
{
  setkey(list_of_name_component_frequencies, name_component)
  setkey(names_observed_patstat, inv_eee_hrm_id)
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("te"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("der"),inv_eee_hrm_id])]
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("ju"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("csc"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("sw"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("lin"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("ja"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("ir"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("min"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("st"),inv_eee_hrm_id])] 
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("dzh"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("per"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("ol"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("ev"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("corp"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("new"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("of"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("ga"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("rndr"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("il"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("in"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("kon/nos"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("ii"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("ca"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("ind"),inv_eee_hrm_id])] 
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("kh"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("ha"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("ei"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("gi"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("kh"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("eh"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("fh"),inv_eee_hrm_id])] 
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("ko"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("an"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("dae"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("sony"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("eindhoven"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("se"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("nippon"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("fujitsu"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("pa"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("pol"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("calif"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("im"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("drsc"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("kodak"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("int"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("pii"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("so"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("honda"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("geb"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("ee"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("vol"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("er"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("mo"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("inst"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("samsung"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("lo"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("sr"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("toshiba"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("no"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("ik"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("ke"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("apt"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("ip"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("tu"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("sub"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("photo"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("dd"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("drs"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("ri"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("ntt"),inv_eee_hrm_id])]
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("veldhoven"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("arturas"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("santiago"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("nj"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("dl"),inv_eee_hrm_id])]  
  
  names_observed_patstat[list(list_of_name_component_frequencies[list("dev"),inv_eee_hrm_id])]  
}
 

#load the manually treated data again
list_of_very_frequent_words_manual <- fread( file=paste(path_orig_patent_data, "/data/data_preparation/", "most_frequent_words_patstat_manual",".csv", sep=""),
                                             encoding = "UTF-8")



#merge information on title status of name_component to data
names_observed_patstat <- merge(names_observed_patstat,
                                list_of_very_frequent_words_manual[,list(name_component,is_title)],
                                by = c("name_component"),
                                all.x=TRUE
)  

#correct some things that jumped out during manual checking
names_observed_patstat[name_component=="-agr",is_title := 1]
names_observed_patstat[name_component=="landw",is_title := 1]
names_observed_patstat[name_component=="-landw",is_title := 1]
names_observed_patstat[name_component=="landwirt",is_title := 1]
names_observed_patstat[name_component=="-landwirt",is_title := 1]
names_observed_patstat[name_component=="habil",is_title := 1]

names_observed_patstat[inv_eee_hrm_id==655|5660==inv_eee_hrm_id|35340571==inv_eee_hrm_id|5657==inv_eee_hrm_id]

#-----------------------------------------------------------------------
#Manually identify and eliminate name of common organizations, firms...
#-----------------------------------------------------------------------


list_of_common_non_name_words <- list(
  c("corp", "cop", "corporation", "co"),
  c("ltd", "limited"),
  c("standard", "standards"), 
  c("ag", "aktiengesellschaft"),
  c("gmbh"),
  c("international", "int"),
  c("pa", "nj", "ny", "ca", "minnesota", "mass", "calif"),
  c("dt", "dd", "kr", "nl", "frankreich", "rj", "us", "usa", "jap", "jp", "fr"), 
  c("city", "building", "apt", "apartment","road", "street"),
  c("nippon"),
  c("rue"),
  c("strase", "straße"),
  c("department", "dept", "division"), 
  c("systems"), 
  c("tech", "technology"), 
  c("laboratory", "laboratories", "lab"),
  c("dev", "development"), 
  c("chemical", "electrical", "electric", "pharma", "pharmaceutical", "pharmaceuticals"),
  c("center"), 
  c("university"),
  c("intellectual"),
  c("property")
)  


#define a function to detect problematic correlations
find_problematic_terms_because_correlations <- function(vector_of_words){
  #find all inv_eee_hrm_ids that contain one of the words
  vec_of_relevant_ids <- unique(list_of_name_components[vector_of_words,inv_eee_hrm_id])
  
  #load all words from these inv_eee_hrm_ids
  relevant_data <- unique(names_observed_patstat[list(vec_of_relevant_ids)])
  tot_N <- length(vec_of_relevant_ids)
  setkey(relevant_data,name_component)
  
  return_data <- relevant_data[,list(percentage=.N/tot_N,N=.N),by="name_component"][
    order(N)][
      percentage>0.005&N>150][
        ,example_correlated_term := vector_of_words[[1]] ]
  
  
  return(list(return_data, relevant_data[list(return_data[,name_component]), list(name_component, inv_eee_hrm_id)]) )
  
}


#sort data and run
setkey(names_observed_patstat,inv_eee_hrm_id)
setkey(list_of_name_components,name_component)
results_check_problematic_combinations <- lapply(list_of_common_non_name_words, find_problematic_terms_because_correlations) 


#collect the results: all name componets that have to be flagged  
collect_list <- list()
for (i in 1:length(results_check_problematic_combinations)) {
  collect_list[[length(collect_list)+1]] <- results_check_problematic_combinations[[i]][[2]]    
}
problematic_name_components <- rbindlist(collect_list)[,name_component_problematic_correlation := 1]
rm(collect_list)


#merge information on title status of name_component to data
names_observed_patstat <- merge(names_observed_patstat,
                                problematic_name_components,
                                by = c("name_component","inv_eee_hrm_id"),
                                all.x=TRUE
)  


names_observed_patstat[inv_eee_hrm_id==655|5660==inv_eee_hrm_id|35340571==inv_eee_hrm_id|5657==inv_eee_hrm_id]


#----------------------------------------
#Create data with inventor problem status
#----------------------------------------

#set missing values to unproblematic => I have only merged problem flags
names_observed_patstat[is.na(is_title)==1,is_title:=0]
names_observed_patstat[is.na(name_component_problematic_correlation)==1,name_component_problematic_correlation:=0]


names_observed_patstat[inv_eee_hrm_id==655|5660==inv_eee_hrm_id|35340571==inv_eee_hrm_id|5657==inv_eee_hrm_id]

#create a cleaned name without titles, corporation names, etc.
setkey(names_observed_patstat,inv_eee_hrm_id,name_type ,nr_word)


cleaned_names <- merge(dcast(names_observed_patstat[is_title==0 & name_component_problematic_correlation==0 ,list(cleaned_names=paste0(name_component,collapse=" ")),
                                                    by=.(inv_eee_hrm_id,name_type )],
                             inv_eee_hrm_id  ~ name_type, value.var = c("cleaned_names")),
                       names_observed_patstat[is_title==0 & name_component_problematic_correlation==0 ,list(cleaned_names_alphabet=paste0(sort(name_component),collapse=" ")),
                                              by=.(inv_eee_hrm_id )],
                       by=c("inv_eee_hrm_id"))
cleaned_names[is.na(firstname),firstname:=""]
cleaned_names[is.na(lastname),lastname:=""]
cleaned_names[,cleaned_names:=paste(firstname,lastname)]
cleaned_names[,cleaned_id:=as.numeric(factor(cleaned_names_alphabet))]

#analyze how many cleaned names there are
setkey(cleaned_names,cleaned_id,inv_eee_hrm_id)
cleaned_names[,number_fused_idents_through_cleaning_names:=.N,by="cleaned_id"]
print("Nr. of names fused together")
print(cleaned_data_names[,.N, by=c("number_fused_idents_through_cleaning_names")][10>=number_fused_idents_through_cleaning_names][order(number_fused_idents_through_cleaning_names)])
cleaned_data_names[number_fused_idents_through_cleaning_names>1][order(cleaned_id)]



cleaned_names[inv_eee_hrm_id==655|5660==inv_eee_hrm_id|35340571==inv_eee_hrm_id|5657==inv_eee_hrm_id]

#visual inspection of names that united especially many inv_eee_hrm_id 
cleaned_names <- merge(cleaned_names,
                       unique(names_observed_patstat[,list(inv_eee_hrm_id,inv_name)]),
                       all.x=TRUE,
                       by="inv_eee_hrm_id")
visually_inspect_fusion_results <- cleaned_names[number_fused_idents_through_cleaning_names>10,]
setkey(visually_inspect_fusion_results,cleaned_id,inv_eee_hrm_id)

length(unique(cleaned_names$inv_eee_hrm_id))
length(unique(cleaned_names$cleaned_id))

cleaned_names[inv_eee_hrm_id==655|5660==inv_eee_hrm_id|35340571==inv_eee_hrm_id|5657==inv_eee_hrm_id]


cleaned_data_names <- merge(unique(cleaned_names[,list(cleaned_id,cleaned_names_alphabet,cleaned_names,firstname,lastname,number_fused_idents_through_cleaning_names,inv_eee_hrm_id)])
                            ,
                            unique(names_observed_patstat[,list(
                              inv_name,
                              nr_good_name_components=sum(is_title==0 & name_component_problematic_correlation==0)),
                              by="inv_eee_hrm_id"])
                            ,
                            by="inv_eee_hrm_id")

cleaned_data_names[inv_eee_hrm_id==655|5660==inv_eee_hrm_id|35340571==inv_eee_hrm_id|5657==inv_eee_hrm_id]


setkey(cleaned_data_names,cleaned_id, inv_eee_hrm_id)
fwrite(cleaned_data_names, file=paste(path_orig_patent_data, "/data/", "/data_preparation/inventor_cleaned_names_list",".csv", sep=""))
cleaned_data_names <-fread(file=paste(path_orig_patent_data, "/data/", "/data_preparation/inventor_cleaned_names_list",".csv", sep=""))
cleaned_data_names


}
  
}

