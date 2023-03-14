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
  library(stringr)
  #read in names lexikon
  names_observed_patstat      <- unique(fread(file=paste(path_to_raw_downloaded_data,"/lexicon_inv.csv",sep=""),
                                              encoding="UTF-8")[,list(inv_name, inv_eee_hrm_id)])
  names_observed_patstat      <- names_observed_patstat[names_observed_patstat[, .I[1], by = inv_eee_hrm_id]$V1]
  
  # 
  # cleaned_data_names_read_in <-fread(file=paste(path_to_raw_downloaded_data, "//", "/data_preparation/inventor_cleaned_names_list",".csv", sep=""),
  #                                    encoding="UTF-8")
  # cleaned_data_names_read_in[,random :=runif(nrow(cleaned_data_names_read_in),0,1)]
  # setkey(cleaned_data_names_read_in,random)
  # view <-cleaned_data_names_read_in[1:50,list(inv_name,inv_eee_hrm_id ,firstname,lastname)]
  # 
  # 
  # setkey(names_observed_patstat,inv_eee_hrm_id)
  # names_observed_patstat <- names_observed_patstat[unique(view[,list(inv_eee_hrm_id)])]
  #immediately correct observed problems
  names_observed_patstat[grepl(inv_name,pattern=", SU$"),inv_name:=sub(inv_name,pattern=" ",replacement=", ")]
  names_observed_patstat[grepl(inv_name,pattern=", su$"),inv_name:=sub(inv_name,pattern=" ",replacement=", ")]
  names_observed_patstat[grepl(inv_name,pattern=",SU$"),inv_name:=sub(inv_name,pattern=" ",replacement=", ")]
  names_observed_patstat[grepl(inv_name,pattern=",su$"),inv_name:=sub(inv_name,pattern=" ",replacement=", ")]
  
  
  print(names_observed_patstat[inv_eee_hrm_id==18063640])
  names_observed_patstat[,inv_name:=gsub(x=inv_name,pattern="(+),",fixed=TRUE,replacement="")]
  names_observed_patstat[,inv_name:=gsub(x=inv_name,pattern="(+)",fixed=TRUE,replacement="")]
  names_observed_patstat[,inv_name:=gsub(x=inv_name,pattern=",",replacement=", ",fixed=TRUE)]
  names_observed_patstat[,inv_name:=gsub(x=inv_name,pattern=".",replacement=". ",fixed=TRUE)]
  names_observed_patstat[,inv_name:=gsub(x=inv_name,pattern="  ",replacement=" ",fixed=TRUE)]
  names_observed_patstat[,inv_name:=gsub(x=inv_name,pattern="  ",replacement=" ",fixed=TRUE)]
  clean_string_for_city_matching(data_table=names_observed_patstat ,variable="inv_name",comma_delete=FALSE)
  print(names_observed_patstat[inv_eee_hrm_id==18063640])
  names_observed_patstat[,comma_count:= str_count(inv_name, ',')]
  names_observed_patstat[,semicolon_count:= str_count(inv_name, ';')]
  names_observed_patstat[,.N,by="comma_count"][order(comma_count)]
  names_observed_patstat[,.N,by="semicolon_count"][order(semicolon_count)]
  names_observed_patstat[semicolon_count==1]

  names_observed_patstat[comma_count==1]
  names_observed_patstat[comma_count==2]
  names_observed_patstat[comma_count==3]
  names_observed_patstat[comma_count==4]
  
  
  #fread(file=paste(path_to_raw_downloaded_data,"/patents_inv.csv",sep=""))[
  #  inv_eee_hrm_id==31741736
  #]

  names_observed_patstat[inv_eee_hrm_id==302]
  areas_inventors_are_active_in  <- unique(fread(file = paste(path_to_raw_downloaded_data, "/", "patents_inv",".csv", sep=""))[,list(inv_eee_hrm_id,appln_auth)])
  
  view <- names_observed_patstat[1:10000][grepl(inv_name, pattern=",",fixed=TRUE)]

  names_observed_patstat_first_last <- unique(rbindlist(list(
    names_observed_patstat[grepl(inv_name, pattern=", ",fixed=TRUE),
                         list(inv_eee_hrm_id,comma_count,str_split_fixed(inv_name, pattern = ", ", n = 4))][,
                               list(inv_eee_hrm_id,
                                    comma_count,
                                    string_before_comma_1=V1,
                                    string_before_comma_2=V2,
                                    string_before_comma_3=V3,
                                    string_before_comma_4=V4)]
    ,
    names_observed_patstat[(grepl(inv_name, pattern=", ",fixed=TRUE)==FALSE),
                              list(inv_eee_hrm_id,comma_count,string_before_comma_1=inv_name)]
  ),use.names=TRUE, fill=TRUE))
  
  DT.m1 = melt(names_observed_patstat_first_last,
               id.vars = c("inv_eee_hrm_id","comma_count"),
               measure.vars = c("string_before_comma_1", "string_before_comma_2", "string_before_comma_3", "string_before_comma_4"))
  setnames(DT.m1,"variable","name_type")
  setnames(DT.m1,"value","inv_name")
  DT.m1 <- DT.m1[!is.na(inv_name) & inv_name!=""]
  DT.m1[inv_eee_hrm_id==25590169]
  DT.m1[inv_eee_hrm_id==18458890]
  DT.m1[inv_eee_hrm_id==302]
  setkey(DT.m1, inv_eee_hrm_id,name_type,inv_name)
  DT.m1[,chunck:=round(seq_len(.N)/.N * 5)]
  list_of_chunks <- split(DT.m1,
                          by=c("chunck"))
  
  list_of_name_components <-rbindlist(lapply(
    FUN=extract_list_of_name_components,
         X=list_of_chunks,
         id_variables = c("inv_eee_hrm_id","name_type","comma_count"),
         variable_containing_names = "inv_name"
         ))

  
  names_observed_patstat <- merge(names_observed_patstat,
                                  list_of_name_components,
                                  by=c("inv_eee_hrm_id","comma_count"))
  
  
  
  
  word_frequency_table      <- unique(merge(
    list_of_name_components,
    areas_inventors_are_active_in,
    by="inv_eee_hrm_id")
  )        
  
  setkey(word_frequency_table,inv_eee_hrm_id,name_type,order_word  )
  
  word_frequency_table[,frequency:=uniqueN(inv_eee_hrm_id),by=c("name_component","appln_auth")]
  word_frequency_table[,mentions_of_country := uniqueN(inv_eee_hrm_id), by=c("appln_auth") ]
  word_frequency_table[,probability_name_component := frequency/mentions_of_country, ]
  
  nrow(unique(word_frequency_table[,list(inv_eee_hrm_id)]))
  
  
  setkey(word_frequency_table,appln_auth,inv_eee_hrm_id,probability_name_component)
  view_data <-unique(word_frequency_table[appln_auth=="GR",list(name_component,appln_auth,frequency,mentions_of_country,probability_name_component)])[order(-probability_name_component)]
  view_data[name_component=="john"]
  unique(word_frequency_table[,list(appln_auth)])
  
  fwrite(word_frequency_table, file=paste(path_to_raw_downloaded_data,"/data_preparation/name_frequency_table.csv",sep=""))
  
  word_frequency_table <- fread( file=paste(path_to_raw_downloaded_data,"/data_preparation/name_frequency_table.csv",sep=""))
  
  
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
list_of_name_component_frequencies <- unique(fread( file=paste(path_to_raw_downloaded_data,"/data_preparation/name_frequency_table.csv",sep=""),
                                                    encoding = "UTF-8")[,list(inv_eee_hrm_id,highest_probability_name_component=max(probability_name_component)),by = "name_component" ] ) 
setkey(list_of_name_component_frequencies,inv_eee_hrm_id)


#merge information on frequency to data
names_observed_patstat <- merge(names_observed_patstat,
                                unique(unique(list_of_name_component_frequencies[,list(name_component,highest_probability_name_component)])),
                                by = c("name_component"),
                                all.x = TRUE
)
names_observed_patstat[inv_eee_hrm_id==302|inv_eee_hrm_id==655|5660==inv_eee_hrm_id|35340571==inv_eee_hrm_id|5657==inv_eee_hrm_id][order(inv_eee_hrm_id ,name_type,order_word )]


#---------------------------------
#Manually correct most common words in names
#---------------------------------  


#Select the most common words
list_of_very_frequent_words <- unique(
  list_of_name_component_frequencies[highest_probability_name_component > 0.001,list(name_component,highest_probability_name_component)  ])[
    order(-highest_probability_name_component)][,is_title:=0]


#write them to disk to manually check for titles and other undesirables
fwrite(list_of_very_frequent_words, file=paste(path_to_raw_downloaded_data, "//data_preparation/", "most_frequent_words_patstat_automatic",".csv", sep=""))
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
list_of_very_frequent_words_manual <- fread( file=paste(path_to_raw_downloaded_data, "//data_preparation/", "most_frequent_words_patstat_manual",".csv", sep=""),
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
names_observed_patstat[grepl(x=name_component,pattern = "dipl.",fixed=TRUE),is_title:=1]
names_observed_patstat[grepl(x=name_component,pattern = "ing.",fixed=TRUE),is_title:=1]
names_observed_patstat[grepl(x=name_component,pattern = "dr.",fixed=TRUE),is_title:=1]
names_observed_patstat[grepl(x=name_component,pattern = "dr.med.",fixed=TRUE),is_title:=1]
names_observed_patstat[grepl(x=name_component,pattern = "b.sc.",fixed=TRUE),is_title:=1]
names_observed_patstat[grepl(x=name_component,pattern = "m.ba",fixed=TRUE),is_title:=1]
names_observed_patstat[grepl(x=name_component,pattern = "econ.",fixed=TRUE),is_title:=1]
names_observed_patstat[grepl(x=name_component,pattern = "grad.",fixed=TRUE),is_title:=1]
names_observed_patstat[grepl(x=name_component,pattern = "jr.",fixed=TRUE),is_title:=1]
names_observed_patstat[grepl(x=name_component,pattern = "sr.",fixed=TRUE),is_title:=1]
names_observed_patstat[grepl(x=name_component,pattern = "[0-9][0-9][0-9]",fixed=FALSE),is_title:=1]
names_observed_patstat[nchar(gsub(name_component,pattern=".",replacement="",fixed=TRUE))<=1,is_title:=1]

names_observed_patstat[inv_eee_hrm_id>300 &inv_eee_hrm_id<320]

names_observed_patstat[inv_eee_hrm_id==302|inv_eee_hrm_id==655|5660==inv_eee_hrm_id|35340571==inv_eee_hrm_id|5657==inv_eee_hrm_id][order(inv_eee_hrm_id ,name_type,order_word )]
names_observed_patstat[inv_eee_hrm_id==303]
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
for (prob_word in unlist(list_of_common_non_name_words)) {
  print(prob_word)
  print(names_observed_patstat[name_component==prob_word ])
}
for (prob_word in unlist(list_of_common_non_name_words)) {
  names_observed_patstat[name_component==prob_word,is_title:=1]
  
}

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
                                unique(problematic_name_components[,list(name_component,inv_eee_hrm_id,name_component_problematic_correlation)]),
                                by = c("name_component","inv_eee_hrm_id"),
                                all.x=TRUE
)  


names_observed_patstat[inv_eee_hrm_id==302|inv_eee_hrm_id==655|5660==inv_eee_hrm_id|35340571==inv_eee_hrm_id|5657==inv_eee_hrm_id]

print(names_observed_patstat[inv_eee_hrm_id==18063640])
#merge information on title status of name_component to data
names_observed_patstat <- merge(names_observed_patstat,
                                unique(fread(file=paste(path_tools_data,"/localities_data/city_data_",version_of_localizations,".csv",sep=""),
                                      encoding="UTF-8")[,list(name_component=c(country_code_city,"su","yu"),is_country_code=1)]),
                                by = c("name_component"),
                                all.x=TRUE
)
print(names_observed_patstat[inv_eee_hrm_id==18063640])

names_observed_patstat[is_country_code==1 & (name_type != "string_before_comma_1"),is_title:=1]
names_observed_patstat[is_country_code==1 & (name_type != "string_before_comma_1"),is_title:=1]


#----------------------------------------
#Create data with inventor problem status
#----------------------------------------

#set missing values to unproblematic => I have only merged problem flags
names_observed_patstat[is.na(is_title)==1,is_title:=0]
names_observed_patstat[is.na(name_component_problematic_correlation)==1,name_component_problematic_correlation:=0]


names_observed_patstat[inv_eee_hrm_id==302|inv_eee_hrm_id==655|5660==inv_eee_hrm_id|35340571==inv_eee_hrm_id|5657==inv_eee_hrm_id]


#create a cleaned name without titles, corporation names, etc.
setkey(names_observed_patstat,inv_eee_hrm_id,name_type ,order_word)


names_observed_patstat[inv_eee_hrm_id==302|inv_eee_hrm_id==655|5660==inv_eee_hrm_id|35340571==inv_eee_hrm_id|5657==inv_eee_hrm_id]



#select the right sections
names_observed_patstat[,section_contains_title:=max(is_title),by=c("inv_eee_hrm_id","name_type")]
names_observed_patstat[,name_contains_title:=max(is_title),by=c("inv_eee_hrm_id")]
names_observed_patstat[,words_without_title:=sum(as.numeric(is_title==0 & name_component_problematic_correlation ==0)),by=c("inv_eee_hrm_id")]
names_observed_patstat[comma_count==3]
names_observed_patstat[comma_count==4][100:150]


names_observed_patstat[inv_eee_hrm_id==302|inv_eee_hrm_id==303|304==inv_eee_hrm_id|575==inv_eee_hrm_id|22414==inv_eee_hrm_id]

names_observed_patstat[name_component=="david",name_component_problematic_correlation:=0]


cleaned_name_components <- dcast(names_observed_patstat[#22414==inv_eee_hrm_id
                                                        ][
  is_title==0 & name_component_problematic_correlation==0 ,list(cleaned_name_comp=paste0(name_component,collapse=" "),
                                                                section_contains_title=as.numeric(max(section_contains_title+name_component_problematic_correlation ,na.rm=TRUE)>=1)),
  by=.(inv_eee_hrm_id,comma_count,name_type )],
      inv_eee_hrm_id +comma_count ~ name_type, value.var = c("cleaned_name_comp","section_contains_title"))
cleaned_name_components[is.na(section_contains_title_string_before_comma_1),section_contains_title_string_before_comma_1:=0]
cleaned_name_components[is.na(section_contains_title_string_before_comma_2),section_contains_title_string_before_comma_2:=0]
cleaned_name_components[is.na(section_contains_title_string_before_comma_3),section_contains_title_string_before_comma_3:=0]
cleaned_name_components[is.na(section_contains_title_string_before_comma_4),section_contains_title_string_before_comma_4:=0]
cleaned_name_components[inv_eee_hrm_id==302|inv_eee_hrm_id==303|304==inv_eee_hrm_id|575==inv_eee_hrm_id|22414==inv_eee_hrm_id]
setnames(cleaned_name_components,
         old=c("cleaned_name_comp_string_before_comma_1", "cleaned_name_comp_string_before_comma_2", "cleaned_name_comp_string_before_comma_3", "cleaned_name_comp_string_before_comma_4"),
         new=c("string_before_comma_1","string_before_comma_2","string_before_comma_3","string_before_comma_4"))



#create firstname for those with comma
#---#---#---#---#---#---#---#---#---#
{
#create firstname for those with comma
cleaned_name_components[(!is.na(string_before_comma_4) & section_contains_title_string_before_comma_4==0) &
                          (as.numeric(!is.na(string_before_comma_1)) + as.numeric(!is.na(string_before_comma_2)) + as.numeric(!is.na(string_before_comma_3)) ==0
                          ),string_to_split:="string_before_comma_4"]
cleaned_name_components[(!is.na(string_before_comma_4) & section_contains_title_string_before_comma_4==0) &
                          (as.numeric(!is.na(string_before_comma_1)) + as.numeric(!is.na(string_before_comma_2)) + as.numeric(!is.na(string_before_comma_3)) ==0
                          ),to_split:=string_before_comma_4]
cleaned_name_components[(!is.na(string_before_comma_4) & section_contains_title_string_before_comma_4==0) &
                          (as.numeric(!is.na(string_before_comma_1)) + as.numeric(!is.na(string_before_comma_2)) + as.numeric(!is.na(string_before_comma_3)) >=1
                          ),firstname:=string_before_comma_4]

cleaned_name_components[(!is.na(string_before_comma_3) & section_contains_title_string_before_comma_3==0) & is.na(firstname) &
                          (as.numeric(!is.na(string_before_comma_1)) + as.numeric(!is.na(string_before_comma_2)) ==0
                          ),string_to_split:="string_before_comma_3"]
cleaned_name_components[(!is.na(string_before_comma_3) & section_contains_title_string_before_comma_3==0) & is.na(firstname) &
                          (as.numeric(!is.na(string_before_comma_1)) + as.numeric(!is.na(string_before_comma_2)) ==0
                          ),to_split:=string_before_comma_3]
cleaned_name_components[(!is.na(string_before_comma_3) & section_contains_title_string_before_comma_3==0) & is.na(firstname) &
                          (as.numeric(!is.na(string_before_comma_1)) + as.numeric(!is.na(string_before_comma_2)) >=1
                          ),firstname:=string_before_comma_3]


cleaned_name_components[!is.na(string_before_comma_2) & is.na(firstname) &
                          (as.numeric(!is.na(string_before_comma_1)) ==0
                          ),string_to_split:="string_before_comma_2"]
cleaned_name_components[!is.na(string_before_comma_2) & is.na(firstname) &
                          (as.numeric(!is.na(string_before_comma_1)) ==0
                          ),to_split:=string_before_comma_2]
cleaned_name_components[!is.na(string_before_comma_2) & is.na(firstname) &
                          (as.numeric((!is.na(string_before_comma_1))) >=1
                          ),firstname:=string_before_comma_2]



cleaned_name_components[!is.na(string_before_comma_1) & is.na(firstname),string_to_split:="string_before_comma_1"]
cleaned_name_components[!is.na(string_before_comma_1) & is.na(firstname),to_split:=string_before_comma_1]



cleaned_name_components[inv_eee_hrm_id==302|inv_eee_hrm_id==303|304==inv_eee_hrm_id|575==inv_eee_hrm_id|5654==inv_eee_hrm_id]
}

#create firstname and lastname for those without comma
#---#---#---#---#---#---#---#---#---#---#---#---#---#---#
{

list_of_all_firstnames <- fread(file=paste(path_tools_data,"/localities_data/firstnames.csv",sep=""),
                                encoding="UTF-8")
clean_string_for_city_matching(data_table=list_of_all_firstnames,variable="name",comma_delete=FALSE)
list_of_all_firstnames <- melt(list_of_all_firstnames[name!=""], id.vars = c("name", "gender"),
                               measure.vars = colnames(list_of_all_firstnames)[3:57])
list_of_all_firstnames[name=="heinrich"]
list_of_all_firstnames[name=="allen"]
list_of_all_firstnames[name=="wynn"]
list_of_all_firstnames <- list_of_all_firstnames[!is.na(value),list(name_component=name,is_first_name=1)]



name_components_merged_to_firstnames <- merge(cleaned_name_components[,list(inv_eee_hrm_id,string_to_split)],
                                              names_observed_patstat[,list(name_component,order_word, string_to_split=name_type ,inv_eee_hrm_id,is_title,name_component_problematic_correlation,words_without_title)],
                                              by=c("inv_eee_hrm_id","string_to_split"))
name_components_merged_to_firstnames <- merge(name_components_merged_to_firstnames,
                                              unique(list_of_all_firstnames),
                                              by=c("name_component"),
                                              all.x=TRUE)
setkey(name_components_merged_to_firstnames,inv_eee_hrm_id,order_word )
name_components_merged_to_firstnames[is.na(is_first_name),is_first_name:=0]


setkey(name_components_merged_to_firstnames,inv_eee_hrm_id,string_to_split, order_word)

name_components_merged_to_firstnames[,is_potential_word:=nchar(gsub(name_component,pattern=".",replacement="",fixed=TRUE))>1]
name_components_merged_to_firstnames[is_potential_word==TRUE & (words_without_title<2 | (is_title ==0 & name_component_problematic_correlation==0)),first_word:=min(order_word),by=c("inv_eee_hrm_id")]
name_components_merged_to_firstnames[is_potential_word==TRUE & (words_without_title<2 | (is_title ==0 & name_component_problematic_correlation==0)),last_word:=max(order_word),by=c("inv_eee_hrm_id")]
name_components_merged_to_firstnames[,up_until_first_word:=order_word<=mean(first_word,na.rm=TRUE),by=c("inv_eee_hrm_id")]
name_components_merged_to_firstnames[,after_and_icln_last_word:=order_word>=mean(last_word,na.rm=TRUE),by=c("inv_eee_hrm_id")]

name_components_merged_to_firstnames[,firstname_last_word:=max(last_word==order_word&is_first_name==1,na.rm=TRUE),
                                     by="inv_eee_hrm_id"]
name_components_merged_to_firstnames[,firstname_first_word:=max(first_word==order_word&is_first_name==1,na.rm=TRUE),
                                     by="inv_eee_hrm_id"]


ids_reversed <- unique(name_components_merged_to_firstnames[(firstname_last_word==1 & firstname_first_word==0),list(inv_eee_hrm_id)])
ids_not_reversed <- unique(name_components_merged_to_firstnames[(firstname_last_word!=1 | firstname_first_word!=0),list(inv_eee_hrm_id)])


names_had_to_be_split <- merge(
  name_components_merged_to_firstnames[ids_reversed][
                                       up_until_first_word ==FALSE,
                                       list(firstname_split = paste0(name_component,collapse=" ")),
                                      by=c("inv_eee_hrm_id")],
name_components_merged_to_firstnames[ids_reversed][
                                     up_until_first_word ==TRUE,
                                     list(lastname_split = paste0(name_component,collapse=" ")),
                                     by=c("inv_eee_hrm_id")],
by="inv_eee_hrm_id",
all=TRUE
)[,reversed:=TRUE]

name_components_merged_to_firstnames[inv_eee_hrm_id==135|302==inv_eee_hrm_id|98==inv_eee_hrm_id]
names_had_to_be_split[inv_eee_hrm_id==135|302==inv_eee_hrm_id|98==inv_eee_hrm_id]


name_components_merged_to_firstnames[ids_not_reversed][15==inv_eee_hrm_id][
]
names_had_to_be_split <- rbindlist(list(
  names_had_to_be_split,
  merge(
  name_components_merged_to_firstnames[ids_not_reversed][
                                         after_and_icln_last_word ==FALSE,
                                       list(firstname_split = paste0(name_component,collapse=" ")),
                                       by=c("inv_eee_hrm_id")],
  name_components_merged_to_firstnames[ids_not_reversed][
                                         after_and_icln_last_word ==TRUE,
                                       list(lastname_split = paste0(name_component,collapse=" ")),
                                       by=c("inv_eee_hrm_id")],
  by="inv_eee_hrm_id",
  all=TRUE
)[,reversed:=FALSE]

))

name_components_merged_to_firstnames[inv_eee_hrm_id==135|302==inv_eee_hrm_id|98==inv_eee_hrm_id|15==inv_eee_hrm_id]
names_had_to_be_split[inv_eee_hrm_id==135|302==inv_eee_hrm_id|98==inv_eee_hrm_id|15==inv_eee_hrm_id]

cleaned_name_components <- merge(cleaned_name_components,
                                 names_had_to_be_split,
                                 by="inv_eee_hrm_id",
                                 all.x=TRUE)

cleaned_name_components[!is.na(to_split),firstname:=firstname_split ]
cleaned_name_components[!is.na(to_split),lastname:=lastname_split ]
cleaned_name_components[!is.na(to_split)&is.na(lastname),lastname:=firstname ]
cleaned_name_components[!is.na(to_split)&is.na(firstname),firstname:=lastname ]

cleaned_name_components[!is.na(to_split)&is.na(lastname),lastname:=to_split ]
cleaned_name_components[!is.na(to_split)&is.na(firstname),firstname:=to_split  ]

print(cleaned_name_components[!is.na(to_split) & is.na(lastname)])

}


#create lastname for those with comma
#---#---#---#---#---#---#---#---#---#
{
#prepare to paste together sections for those with comma
cleaned_name_components[is.na(lastname) ,lastname:=""]
cleaned_name_components[is.na(string_before_comma_1) ,string_before_comma_1:=""]
cleaned_name_components[is.na(string_before_comma_2) ,string_before_comma_2:=""]
cleaned_name_components[is.na(string_before_comma_3) ,string_before_comma_3:=""]
cleaned_name_components[is.na(string_before_comma_4) ,string_before_comma_4:=""]

#create lastname for those with comma
cleaned_name_components[(string_before_comma_4!="" & section_contains_title_string_before_comma_4==0) &(is.na(lastname)|lastname==""),lastname:=paste(string_before_comma_1,string_before_comma_2,string_before_comma_3)]
cleaned_name_components[(string_before_comma_3!="" & section_contains_title_string_before_comma_3==0) &(is.na(lastname)|lastname==""),lastname:=paste(string_before_comma_1,string_before_comma_2)]
cleaned_name_components[string_before_comma_2!="" &(is.na(lastname)|lastname==""),lastname:=string_before_comma_1]

cleaned_name_components[is.na(lastname)|lastname==""]

print(
merge(unique(names_observed_patstat[inv_eee_hrm_id==302|inv_eee_hrm_id==303|304==inv_eee_hrm_id|575==inv_eee_hrm_id|22414==inv_eee_hrm_id,
                             list(inv_eee_hrm_id,inv_name)]),
      cleaned_name_components,
      by="inv_eee_hrm_id"),
all=TRUE
)
}



cleaned_data_names <- merge(cleaned_name_components[,list(inv_eee_hrm_id ,firstname,lastname)],
                       names_observed_patstat[is_title==0 & name_component_problematic_correlation==0 ,list(cleaned_names_alphabet=paste0(sort(name_component),collapse=" ")),
                                              by=.(inv_eee_hrm_id )],
                       by=c("inv_eee_hrm_id"))
print(cleaned_data_names[inv_eee_hrm_id==18063640])

cleaned_data_names[is.na(firstname),firstname:=""]
cleaned_data_names[is.na(lastname),lastname:=""]
cleaned_data_names[,cleaned_names:=paste(firstname,lastname)]
cleaned_data_names[,cleaned_id:=as.numeric(factor(cleaned_names_alphabet))]

#analyze how many cleaned names there are
setkey(cleaned_data_names,cleaned_id,inv_eee_hrm_id)
cleaned_data_names[,number_fused_idents_through_cleaning_names:=.N,by="cleaned_id"]
print("Nr. of names fused together")
print(cleaned_data_names[,.N, by=c("number_fused_idents_through_cleaning_names")][10>=number_fused_idents_through_cleaning_names][order(number_fused_idents_through_cleaning_names)])
cleaned_data_names[number_fused_idents_through_cleaning_names>1][order(cleaned_id)]



cleaned_data_names[inv_eee_hrm_id==302|inv_eee_hrm_id==655|5660==inv_eee_hrm_id|35340571==inv_eee_hrm_id|5657==inv_eee_hrm_id]

print(cleaned_data_names[inv_eee_hrm_id==18063640])
#visual inspection of names that united especially many inv_eee_hrm_id 
cleaned_data_names <- merge(cleaned_data_names,
                       unique(names_observed_patstat[,list(inv_eee_hrm_id,inv_name)]),
                       all.x=TRUE,
                       by="inv_eee_hrm_id")
visually_inspect_fusion_results <- cleaned_data_names[number_fused_idents_through_cleaning_names>10,]
setkey(visually_inspect_fusion_results,cleaned_id,inv_eee_hrm_id)

length(unique(cleaned_data_names$inv_eee_hrm_id))
length(unique(cleaned_data_names$cleaned_id))


cleaned_data_names[inv_eee_hrm_id==302|inv_eee_hrm_id==655|5660==inv_eee_hrm_id|35340571==inv_eee_hrm_id|5657==inv_eee_hrm_id]


cleaned_data_names <- merge(unique(cleaned_data_names[,list(cleaned_id,cleaned_names_alphabet,cleaned_names,firstname,lastname,number_fused_idents_through_cleaning_names,inv_eee_hrm_id)])
                            ,
                            unique(names_observed_patstat[,list(
                              inv_name,
                              nr_good_name_components=sum(is_title==0 & name_component_problematic_correlation==0)),
                              by="inv_eee_hrm_id"])
                            ,
                            by="inv_eee_hrm_id")

cleaned_data_names[inv_eee_hrm_id==303|inv_eee_hrm_id==655|5660==inv_eee_hrm_id|35340571==inv_eee_hrm_id|5657==inv_eee_hrm_id]
print(cleaned_data_names[inv_eee_hrm_id==18063640])

setkey(cleaned_data_names,cleaned_id, inv_eee_hrm_id)




add_to_data<- TRUE
if (add_to_data==FALSE){
  fwrite(cleaned_data_names, 
         file=paste(path_to_raw_downloaded_data, "//", "/data_preparation/inventor_cleaned_names_list",".csv", sep=""),
         encoding="UTF-8")
}


if (add_to_data==TRUE){
  
  fwrite(cleaned_data_names[1], 
         file=paste(path_to_raw_downloaded_data, "//", "/data_preparation/inventor_cleaned_names_list",".csv", sep=""),
         encoding="UTF-8")
  
  cleaned_data_names_read_in_exist <-fread(file=paste(path_to_raw_downloaded_data, "//", "/data_preparation/inventor_cleaned_names_list",".csv", sep=""),
                                     encoding="UTF-8")
  already_existing_row <- nrow(cleaned_data_names_read_in_exist)
  
  max_row <- nrow(cleaned_data_names)
  
  packets <- already_existing_row + (0:ceiling((max_row - already_existing_row)/100000))*100000

for (current_packet in packets) {
  print(current_packet)

  if (current_packet<=max_row) {
  fwrite(cleaned_data_names[current_packet:min(current_packet+100000,max_row)], 
         file=paste(path_to_raw_downloaded_data, "//", "/data_preparation/inventor_cleaned_names_list",".csv", sep=""),
         encoding="UTF-8",
         append=TRUE)
  }
}
}

cleaned_data_names_read_in <-fread(file=paste(path_to_raw_downloaded_data, "//", "/data_preparation/inventor_cleaned_names_list",".csv", sep=""),
                           encoding="UTF-8")
cleaned_data_names_read_in[,random :=runif(nrow(cleaned_data_names_read_in),0,1)]
setkey(cleaned_data_names_read_in,random)
view <-cleaned_data_names_read_in[1:50,list(inv_name,inv_eee_hrm_id ,firstname,lastname)]
cleaned_data_names_read_in[inv_eee_hrm_id==18063640]

control <- merge(names_observed_patstat[],view,by="inv_eee_hrm_id")

control2 <- merge(cleaned_name_components[],view,by="inv_eee_hrm_id")


view2 <- cleaned_data_names_read_in[grepl(inv_name,pattern=", su$")]
}
  
}

