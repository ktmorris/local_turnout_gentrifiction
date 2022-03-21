
## set seed for reproducibility. this is a random 5-digit number: floor(runif(1, min = 10000, max = 99999))
set.seed(64751)

on_nyu <- T

if(on_nyu){
  library(Matching)
  library(data.table)
  library(snow)
  library(parallel)
  library(scales)
  library(kableExtra)
  library(caret)
  library(tidyverse)
  
  NodeFile = Sys.getenv("MY_HOSTFILE")
  
  cl <- makeCluster(c(readLines(NodeFile)), type="SOCK")
}else{
  source("./code/misc/AutoCluster4.R")
  cl <- NCPUS(detectCores() - 1)
}

full_set <- readRDS("temp/pre_match.rds") %>% 
  filter(potential) %>% 
  mutate(yob = as.integer(substring(DATE_OF_BIRTH, 1, 4)),
         at_20 = ifelse(is.na(at_20), F, at_20),
         black = 1*(RACE == "B"),
         white = 1*(RACE == "W"),
         reg_date = as.Date(as.character(REGISTRATION_DATE), "%Y%m%d"),
         reg_date = as.integer(reg_date - as.Date("2010-01-01")),
         male = 1*(GENDER == "M"),
         dist = dist * 0.000621371,
         dist = ifelse(is.na(dist), 999999999999, dist),
         moved = ifelse(dist < 1, 1,
                        ifelse(at_20, 2,
                               ifelse(dist < 999999999999, 3, 4)))) %>% 
  select(reg_num = REGISTRATION_NBR,
         yob, black, white, reg_date, male,
         ends_with("_2010"), gfied,
         to_01, to_02, to_04, to_05, to_06, to_08, to_09,
         moved) %>% 
  group_by(reg_num) %>% 
  filter(row_number() == 1)


full_set <- full_set[complete.cases(full_set),]

full_set <- full_set %>% 
  group_by(gfied, black, white, male) %>% 
  slice_sample(prop = 0.5) %>% 
  ungroup()

Tr <- full_set$gfied


X = full_set %>%
  select(moved, black, white, male, starts_with("to_"),
         everything(), -gfied, -reg_num)

genout <- GenMatch(Tr = Tr, X = X, pop.size = 150, ties = F, cluster = cl,
                   exact = c(rep(T, 11), rep(F, 8)))

saveRDS(genout, "./temp/genout_to50p.rds")
