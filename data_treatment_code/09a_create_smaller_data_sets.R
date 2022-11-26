list_of_data_paths <- list.dirs(path = path_to_output_data,
                          recursive = FALSE)
list_of_data_paths <- paste0(list_of_data_paths[grepl(x=list_of_data_paths,pattern="_relevant_data")],"/")



print("raw_data_inv_patent")
inv_patent <- fread(paste(path_to_output_data,"/","/raw_data_inv_patent",".csv",sep=""),
                    encoding="UTF-8")
setkey(inv_patent)


for (sub_data_path in list_of_data_paths) {
  sub_data_name <-gsub(x=sub_data_path,pattern="s:/PROJEKTE/Epo_micro_data/data/data_preparation//",replacement="")
  sub_data_name <-gsub(x=sub_data_name,pattern="_relevant_data/",replacement="")
  
  
  print(sub_data_name)

  #patent files
  list_patents <- fread(file = paste0(sub_data_path,"list_patents_",sub_data_name,".csv"),
                        encoding = "UTF-8")
  
  
  
}

