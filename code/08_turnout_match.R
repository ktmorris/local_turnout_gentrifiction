

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
                        ifelse(dist < 10, 2,
                               ifelse(dist < 999999999999, 3, 4)))) %>% 
  rename(reg_num = REGISTRATION_NBR) %>% 
  group_by(reg_num) %>% 
  filter(row_number() == 1)

slim_set <- full_set %>% 
  select(reg_num,
         yob, black, white, reg_date, male,
         ends_with("_2010"), gfied,
         to_01, to_02, to_04, to_05, to_06, to_08, to_09,
         moved)

slim_set <- slim_set[complete.cases(slim_set),]

Tr <- slim_set$gfied

X = slim_set %>%
  ungroup() %>% 
  select(moved, black, white, male, starts_with("to_"),
         everything(), -gfied, -reg_num)

# genout <- readRDS("./temp/genout_to50p.rds")
# 
# mout <- Match(Tr = Tr, X = X, Weight.matrix = genout, ties = F,
#               exact = c(rep(T, 11), rep(F, 8)))
# 
# saveRDS(mout, "temp/turnout_mout.rds")

mout <- readRDS("temp/turnout_mout.rds")
ids <- data.table(reg_num = slim_set$reg_num,
                  id = c(1:nrow(slim_set)))

matches <- data.table(group = c(mout$index.treated, unique(mout$index.treated)),
                      voter = c(mout$index.control, unique(mout$index.treated)),
                      weight = c(mout$weights, rep(1, length(unique(mout$index.treated)))))

matches <- left_join(matches, ids, by = c("group" = "id")) %>% 
  select(-group) %>% 
  rename(group = reg_num)

matches <- left_join(matches, ids, by = c("voter" = "id")) %>% 
  select(-voter) %>% 
  rename(voter = reg_num)

matches <- inner_join(matches, full_set, by = c("voter" = "reg_num")) %>% 
  mutate(treated = voter == group)

matches <- matches %>%  
  mutate(race = ifelse(white, "White",
                       ifelse(black, "Black",
                              "Other")))
cleanup("matches", "full_set", "mout")

saveRDS(matches, "temp/to_matches_clean.rds")
#####

ll <- matches %>%  
  group_by(treated, moved) %>% 
  summarize(across(starts_with("to_"), mean)) %>% 
  pivot_longer(starts_with("to_")) %>% 
  mutate(name = as.integer(substring(name, 4, 5)) + 2000) %>% 
  filter(!(name %in% c(2003, 2007, 2011, 2015, 2019)),
         name %% 2 == 1) %>% 
  mutate(moved = ifelse(moved == 1,
                        "Did Not Move",
                        ifelse(moved == 2,
                               "Moved in Atlanta",
                               ifelse(moved == 3,
                                      "Moved out of Atlanta",
                                      "Out of Data"))),
         treated = ifelse(treated, "Treated", "Control"))

ll$moved <- factor(ll$moved, levels = c("Did Not Move",
                                        "Moved in Atlanta",
                                        "Moved out of Atlanta",
                                        "Out of Data"))

ll$treated <- factor(ll$treated, levels = c("Treated", "Control"))

ggplot(ll, aes(x = name, y = value, linetype = treated,
               shape = treated)) + geom_line() + geom_point() +
  facet_grid(. ~ moved) +
  theme_bc(base_family = "LM Roman 10",
           legend.position="bottom") +
  scale_x_continuous(breaks = seq(2001, 2021, 8)) +
  guides(linetype = guide_legend(title.position = "top", title.hjust = 0.5)) +
  labs(x = "Year", y = "Turnout",
       linetype = "Treatment Group",
       shape = "Treatment Group") +
  scale_y_continuous(labels = percent)

#####

ll <- matches %>%  
  group_by(treated, moved, race) %>% 
  summarize(across(starts_with("to_"), mean)) %>% 
  pivot_longer(starts_with("to_")) %>% 
  mutate(name = as.integer(substring(name, 4, 5)) + 2000) %>% 
  filter(!(name %in% c(2003, 2007, 2011, 2015, 2019)),
         name %% 2 == 1) %>% 
  mutate(moved = ifelse(moved == 1,
                        "Did Not Move",
                        ifelse(moved == 2,
                               "Moved in Atlanta",
                               ifelse(moved == 3,
                                      "Moved out of Atlanta",
                                      "Out of Data"))),
         treated = ifelse(treated, "Treated", "Control"))

ll2 <- matches %>%  
  group_by(treated, moved) %>% 
  summarize(across(starts_with("to_"), mean)) %>% 
  pivot_longer(starts_with("to_")) %>% 
  mutate(name = as.integer(substring(name, 4, 5)) + 2000) %>% 
  filter(!(name %in% c(2003, 2007, 2011, 2015, 2019)),
         name %% 2 == 1) %>% 
  mutate(moved = ifelse(moved == 1,
                        "Did Not Move",
                        ifelse(moved == 2,
                               "Moved in Atlanta",
                               ifelse(moved == 3,
                                      "Moved out of Atlanta",
                                      "Out of Data"))),
         treated = ifelse(treated, "Treated", "Control"),
         race = "Overall")

ll <- bind_rows(ll, ll2)

ll$moved <- factor(ll$moved, levels = c("Did Not Move",
                                        "Moved in Atlanta",
                                        "Moved out of Atlanta",
                                        "Out of Data"))

ll$treated <- factor(ll$treated, levels = c("Treated", "Control"))
ll$race <- factor(ll$race, levels = c("Black", "White", "Other", "Overall"))

time_series <- ggplot(ll, aes(x = name, y = value, linetype = treated,
                              shape = treated)) + geom_line() + geom_point() +
  facet_grid(race ~ moved) +
  theme_bc(base_family = "LM Roman 10",
           legend.position="bottom") +
  scale_x_continuous(breaks = seq(2001, 2021, 8)) +
  guides(linetype = guide_legend(title.position = "top", title.hjust = 0.5)) +
  labs(x = "Year", y = "Turnout",
       linetype = "Treatment Group",
       shape = "Treatment Group") +
  scale_y_continuous(labels = percent, breaks = seq(0, 0.5, 0.2))
time_series
saveRDS(time_series, "temp/local_turnout_timeseries.rds")

###################################
m_long <- matches %>% 
  select(starts_with("to_"), weight, group, voter,
         moved, male,
         yob, reg_date, median_income_2010,
         median_gross_rent_2010,
         share_renter_2010, median_home_value_2010,
         nh_white_2010, nh_black_2010, treated,
         GEOID, race, weight) %>% 
  pivot_longer(starts_with("to_"), names_to = "year",
               values_to = "turnout") %>% 
  mutate(year = as.integer(substring(year, 4, 5)) + 2000,
         tp = treated * (year > 2009))


mods <- list()

for(r in c("White", "Black", "Other", "Overall")){
  for(m in c(1:4)){
    for(t in c(1:2)){
      if(r == "Overall"){
        d <- filter(m_long, moved == m,
                    year %% 2 == 1)
      }else{
        d <- filter(m_long, race == r, moved == m,
                    year %% 2 == 1)
      }
      if(t == 1){
        nm <- feols(turnout ~ tp | year + GEOID, data = d,
                    weights = ~weight, cluster = c("group", "voter", "GEOID"))
        mods <- append(mods, list(nm))
      }else{
        nm <- feols(turnout ~ tp +
                      male + yob + reg_date |
                      year + GEOID, data = d,
                    weights = ~weight, cluster = c("group", "voter", "GEOID"))
        mods <- append(mods, list(nm))
      }
    }
  }
}


modelsummary(mods)


######################################

cints <- rbindlist(lapply(c(1:length(mods)), function(i){
  j <- confint(mods[[i]])
  j <- j[rownames(j) == "tp", ] %>%  
    mutate(race = case_when(i <= 8 ~ "White",
                            i > 8 & i <= 16 ~ "Black",
                            i > 16 & i <= 24 ~ "Other",
                            i > 24 ~ "Overall"),
           moved = ceiling(i / 2) %% 4,
           moved = ifelse(moved == 0, 4, moved),
           covs = i %% 2 == 0)
}))

colnames(cints) <- c("l", "u", "race", "moved", "covs")

cints <- cints %>% 
  mutate(est = (l + u) / 2,
         moved = ifelse(moved == 1,
                        "Did Not Move",
                        ifelse(moved == 2,
                               "Moved in Atlanta",
                               ifelse(moved == 3,
                                      "Moved out of Atlanta",
                                      "Out of Data"))),
         covs = ifelse(covs, "Inc. Covariates", "No Covariates"))

cints$race <- factor(cints$race, levels = rev(c("Black", "White", "Other", "Overall")))
cints$covs <- factor(cints$covs, levels = c("No Covariates", "Inc. Covariates"))
cints$moved <- factor(cints$moved, levels = c("Did Not Move",
                                              "Moved in Atlanta",
                                              "Moved out of Atlanta",
                                              "Out of Data"))

local_cints <- ggplot(cints) +
  facet_grid(. ~ moved) +
  ggstance::geom_pointrangeh(aes(y = race, x = est, linetype = covs,
                                 xmin = l, xmax = u, color = covs, 
                                 shape = covs),
                             position = ggstance::position_dodgev(height = -.5), 
                             fill = "white", fatten = 3,
                             size = 0.8, show.legend = T) +
  geom_vline(xintercept = 0, linetype = "dashed") +
  geom_hline(yintercept = 1.5) +
  scale_x_continuous(breaks = seq(-.05, 0.1, .05), limits = c(-.05, .1),
                     labels = c("-5%", "0%", "5%", "10%")) +
  theme_bc(base_family = "LM Roman 10",
           legend.position = "bottom",
           legend.title = element_blank()) +
  labs(x = "Estimated Turnout Effect", y = "Race",
       caption = "Models including covariates include gender, age, and registration date.") +
  guides(linetype = guide_legend(title.position = "top", title.hjust = 0.5)) +
  scale_color_manual(values = c("#9F142C", "black"))

saveRDS(local_cints, "temp/local_cints.rds")


################################
cleanup("matches", "full_set", "mout")

full_set <- left_join(full_set,
                      matches %>% 
                        group_by(reg_num = voter) %>% 
                        tally()) %>% 
  mutate(n = ifelse(is.na(n), 0, n))

ll <- full_set %>% 
  group_by(gfied) %>% 
  summarize(across(c(yob, black, white, reg_date, male,
                     ends_with("_2010"),
                     to_01, to_02, to_04, to_05, to_06, to_08, to_09),
                   list(m = ~ mean(., na.rm = T),
                        w = ~ weighted.mean(., n, na.rm = T))))
