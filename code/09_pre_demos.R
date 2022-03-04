full_set <- readRDS("temp/pre_match.rds") %>% 
  mutate(yob = as.integer(substring(DATE_OF_BIRTH, 1, 4)),
         black = 1*(RACE == "B"),
         white = 1*(RACE == "W"),
         reg_date = as.Date(as.character(REGISTRATION_DATE), "%Y%m%d"),
         reg_date = as.integer(reg_date - as.Date("2010-01-01")),
         male = 1*(GENDER == "M"),
         dist = ifelse(is.na(dist), 999999999999, dist),
         moved = ifelse(dist < 1, "Did Not Move",
                        ifelse(dist < 999999999999, "Moved in Georgia", "No Longer in Data")),
         group = ifelse(gfied, "Gentrified",
                        ifelse(potential, "Gentrifiable", "Not Gentrifiable"))) %>% 
  select(reg_num = REGISTRATION_NBR,
         yob, black, white, reg_date, male,
         ends_with("_2010"), gfied, potential, group)


ll <- full_set %>% 
  filter(!is.na(group)) %>% 
  group_by(group) %>% 
  summarize(across(c(white, black, male, yob,
                   median_income_2010, median_gross_rent_2010,
                   share_renter_2010, median_home_value_2010),
            mean, na.rm = T),
            n = n()) %>% 
  mutate(across(c(white, black, male, share_renter_2010), percent, accuracy = 0.1),
         yob = as.character(round(yob, digits = 1)),
         across(c(median_income_2010, median_gross_rent_2010,
                  median_home_value_2010), dollar),
         n = comma(n)) %>% 
  pivot_longer(c(white, black, male, yob,
                 median_income_2010, median_gross_rent_2010,
                 share_renter_2010, median_home_value_2010, n)) %>% 
  pivot_wider(names_from = group) %>% 
  mutate(name = ifelse(name == "white", "% White", name),
         name = ifelse(name == "black", "% Black", name),
         name = ifelse(name == "male", "% Male", name),
         name = ifelse(name == "yob", "Birth Year", name),
         name = ifelse(name == "median_income_2010", "Median Income", name),
         name = ifelse(name == "median_gross_rent_2010", "Median Gross Rent", name),
         name = ifelse(name == "share_renter_2010", "% Renters", name),
         name = ifelse(name == "median_home_value_2010", "Median Home Value", name),
         name = ifelse(name == "n", "N", name)) %>% 
  rename(Variable = name)

saveRDS(ll, "temp/pre_demos.rds")
