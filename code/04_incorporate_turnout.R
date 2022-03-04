
# for(i in c(2001:2021)){
#   ## have to manually download post-2014
#   if(i <= 2014){
#     download.file(paste0("https://sos.ga.gov/elections/VoterHistoryFiles/", i, ".zip"),
#                   destfile = paste0("raw_data/history_files/history", i, ".zip"))
#   }
# 
#   unzip(paste0("raw_data/history_files/history", i, ".zip"),
#         exdir = paste0("raw_data/history_files/history", i))
# }


f10 <- readRDS("temp/combine_10_20.rds")

ids <- as.data.table(f10$REGISTRATION_NBR)

j <- sapply(seq(2001, 2021, 2), function(y){
  print(y)
  h <- read_fwf(list.files(paste0("raw_data/history_files/history", y), full.names = T),
                col_positions = fwf_widths(c(3, 8, 8, 3, 1, 1),
                                           col_names = c("county", "reg_num", "date",
                                                         "type", "party", "absentee"))) %>%  
    filter(type %in% c("007", "GEN", "003"))
  
  if(substring(h$date[1], 1, 2) == "20"){
    h <- filter(h, substring(date, 5, 6) == "11")
  }else{
    h <- filter(h, substring(date, 1, 2) == "11")
  }
  
  to <- ids$V1 %in% as.integer(h$reg_num)
  
  return(to)
})

colnames(j) <- c("to_01", "to_03", "to_05", "to_07", "to_09",
                 "to_11", "to_13", "to_15", "to_17", "to_19", "to_21")

f10 <- cbind(f10, j)

j <- sapply(seq(2002, 2020, 2), function(y){
  print(y)
  h <- read_fwf(list.files(paste0("raw_data/history_files/history", y), full.names = T),
                col_positions = fwf_widths(c(3, 8, 8, 3, 1, 1),
                                           col_names = c("county", "reg_num", "date",
                                                         "type", "party", "absentee"))) %>%  
    filter(type %in% c("007", "GEN", "003", "G"))
  
  if(substring(h$date[1], 1, 2) == "20"){
    h <- filter(h, substring(date, 5, 6) == "11")
  }else{
    h <- filter(h, substring(date, 1, 2) == "11")
  }
  
  to <- ids$V1 %in% as.integer(h$reg_num)
  
  return(to)
})

colnames(j) <- c("to_02", "to_04", "to_06", "to_08", "to_10",
                 "to_12", "to_14", "to_16", "to_18", "to_20")

f10 <- cbind(f10, j)

f10 <- f10 %>% 
  mutate(across(starts_with("to"), as.integer))

saveRDS(f10, "temp/pre_match.rds")
