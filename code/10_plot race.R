matches <- readRDS("temp/to_matches_clean.rds")

tsr <- rbindlist(lapply(c(2009:2019), function(y){
  census_race_ethnicity("tract", year = y, state = "GA", county = c("FULTON", "DEKALB")) %>% 
    select(GEOID, nh_black, nh_white) %>% 
    mutate(year = y)
}))

dec <- get_decennial(geography = "tract", year = 2000, variables = c("white" = "H007003",
                                                                     "black" = "H007004"),
                     summary_var = "H007001", state = "GA",
                     county = c("FULTON", "DEKALB"), output = "wide") %>% 
  mutate(nh_black = black / summary_value,
         nh_white = white / summary_value,
         year = 2000) %>% 
  select(GEOID, year, nh_white, nh_black)

tsr <- bind_rows(dec, tsr)

ws <- matches %>% 
  group_by(GEOID) %>% 
  summarize(weight = sum(weight),
            tr = mean(treated))

tsr <- left_join(tsr, ws) %>% 
  filter(!is.na(weight))

ll <- tsr %>% 
  group_by(tr, year) %>% 
  summarize(across(c(nh_black, nh_white), list(mean = mean,
                                               weighted = ~ weighted.mean(., weight)))) %>% 
  mutate(tr = ifelse(tr == 1, "Treated", "Control")) %>% 
  pivot_longer(cols = starts_with("nh"), names_to = "race", values_to = "share")

ll$tr <- factor(ll$tr, levels = c("Treated", "Control"))

plot_race <- ggplot(filter(ll, !grepl("mean", race)),
       aes(x = year, y = share,
           color = tr, linetype = tr, shape = tr)) +
  facet_grid(. ~ race, labeller = as_labeller(c(`nh_black_weighted` = "Nonhispanic Black",
                                                `nh_white_weighted` = "Nonhispanic White"))) +
  geom_point() +
  geom_line() +
  theme_bc(base_family = "LM Roman 10",
           legend.position = "bottom") +
  scale_y_continuous(labels = percent) +
  labs(x = "Year",
       y = "Share",
       color = NULL,
       linetype = NULL,
       shape = NULL,
       caption = "2000 estimates come from decennial census; others come from 5-year ACS estimates.
Tract characteristics weighted by output of matching excersise.") +
  scale_color_manual(values = c("#9F142C", "black"))
plot_race
saveRDS(plot_race, "temp/race_time_trend.rds")

summary(lm(nh_black_2010 ~ treated, matches))
