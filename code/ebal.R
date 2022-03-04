

full_set <- readRDS("temp/pre_match.rds") %>% 
  filter(potential) %>% 
  mutate(yob = as.integer(substring(DATE_OF_BIRTH, 1, 4)),
         black = 1*(RACE == "B"),
         white = 1*(RACE == "W"),
         latino = 1*(RACE == "H"),
         reg_date = as.Date(as.character(REGISTRATION_DATE), "%Y%m%d"),
         reg_date = as.integer(reg_date - as.Date("2010-01-01")),
         male = 1*(GENDER == "M"),
         moved = is.na(dist) | dist > 0) %>% 
  select(reg_num = REGISTRATION_NBR,
         yob, black, white, reg_date, male,
         ends_with("_2010"), gfied,
         moved, latino, starts_with("to_"),
         -to_03, -to_07)


#####################
stayers <- filter(full_set, moved,
                  !is.na(gfied), white == 1)
stayers <- stayers[complete.cases(select(stayers,
                                         yob, reg_date, median_income_2010,
                                         black, white, latino, male,
                                         starts_with("to_0"))), ]
mb <- ebalance(stayers$gfied,
               select(stayers,
                      yob, reg_date, median_income_2010,
                      male,
                      to_02, to_04, to_06, to_08, to_10))

stayers <- bind_rows(
  filter(stayers, gfied) %>%
    mutate(weight = 1),
  filter(stayers, !gfied) %>%
    mutate(weight = mb$w)
)
ll <- stayers %>%
  group_by(gfied) %>%
  summarize(across(starts_with("to_"), ~ weighted.mean(., weight))) %>%
  pivot_longer(starts_with("to_")) %>%
  mutate(year = 2000 + as.integer(gsub("to_", "", name)))

ggplot(filter(ll, year %% 2 == 0), aes(x = year, y = value, color = gfied)) + geom_line()

