record_linkage_data  <- fread(file = paste("s:/PROJEKTE/Inventor_Matching_PATSTAT/", "data/data_preparation/", "rl_id_data.csv", sep=""),
                              encoding="UTF-8")

rich1 <- fread(file = paste("s:/PROJEKTE/Inventor_Matching_PATSTAT/", "data/profit_data/linked/", "rich1.csv", sep=""),
               encoding="UTF-8")
rich1[,person_id:=as.numeric(person_id)]


test_1 <- merge(rich1,
                fread(paste(path_to_raw_downloaded_data,"lexicon_apl.csv",sep=""),
                      encoding="UTF-8"),
                by.x="person_id",
                by.y="apl_person_id",
                all.x=TRUE)

test_1[,list(person_name,apl_name)]



rich2 <- fread(file = paste("s:/PROJEKTE/Inventor_Matching_PATSTAT/", "data/profit_data/linked/", "rich2.csv", sep=""),
               encoding="UTF-8")
rich2[,person_id:=as.numeric(person_id)]



test_2 <- merge(rich2,
                fread(paste(path_to_raw_downloaded_data,"lexicon_apl.csv",sep=""),
                      encoding="UTF-8"),
                by.x="person_id",
                by.y="apl_person_id",
                all.x=TRUE)

test_2[,list(person_name,apl_name)]

