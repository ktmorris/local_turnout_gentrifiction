
full_set <- readRDS("temp/pre_match.rds") %>% 
  filter(potential) %>% 
  mutate(dist = dist * 0.000621371,
         at_20 = ifelse(is.na(at_20), F, at_20),
         yob = as.integer(substring(DATE_OF_BIRTH, 1, 4)),
         black = 1*(RACE == "B"),
         white = 1*(RACE == "W"),
         reg_date = as.Date(as.character(REGISTRATION_DATE), "%Y%m%d"),
         reg_date = as.integer(reg_date - as.Date("2010-01-01")),
         male = 1*(GENDER == "M"),
         dist = ifelse(is.na(dist), 999999999999, dist),
         moved = ifelse(dist < 1, "Did Not Move",
                        ifelse(at_20, "Moved in Atlanta",
                               ifelse(dist < 999999999999, "Moved in Georiga", "No Longer in Data"))),
         group = ifelse(gfied, "Gentrified",
                        ifelse(potential, "Gentrifiable", "Not Gentrifiable"))) %>% 
  select(reg_num = REGISTRATION_NBR,
         yob, black, white, reg_date, male, moved,
         ends_with("_2010"), gfied, potential, group)


m1 <- multinom(moved ~ group, full_set)

m2 <- multinom(moved ~ group +
                 yob + black + white +
                 reg_date + male + median_income_2010 +
                 median_gross_rent_2010 +
                 share_renter_2010 + median_home_value_2010, full_set)

marg1 <- ggeffect(model = m1, "group [all]") %>% 
  mutate(response.level = gsub("[.]", " ", response.level),
         type = "No Covariates")

marg2 <- ggeffect(model = m2, "group [all]") %>% 
  mutate(response.level = gsub("[.]", " ", response.level),
         type = "With Covariates")

marg <- bind_rows(marg1, marg2)

mef <- ggplot(marg, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  geom_errorbar(width = .1) +
  facet_grid(type ~ response.level) +
  labs(x = "Treatment Group", y = "Predicted Probability") +
  scale_y_continuous(labels = percent) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10"),
        legend.position = "bottom",
        panel.border = element_rect(fill = NA, 
                                    colour = "grey20"),
        strip.background = element_rect(fill = NA, 
                                        colour = "grey20"),
        plot.caption = element_text(hjust = 0))
mef
saveRDS(mef, "temp/move_marg.rds")

###############################

movers <- filter(full_set, moved %in% c("Moved lt 10 Miles", "Moved mt 10 Miles"))

cd_2020 <- readRDS("temp/combine_10_20.rds") %>% 
  select(reg_num = REGISTRATION_NBR,
         median_income_2020) %>% 
  distinct()

movers <- left_join(movers, cd_2020) %>% 
  mutate(change = (median_income_2020*0.820289667) / median_income_2010)

m3 <- lm(change ~ group, movers)

m4 <- lm(change ~ group +
                 yob + black + white +
                 reg_date + male + median_income_2010 +
                 median_gross_rent_2010 +
                 share_renter_2010 + median_home_value_2010, movers)

marg3 <- ggeffect(model = m3, "group [all]") %>% 
  mutate(type = "No Covariates")

marg4 <- ggeffect(model = m4, "group [all]") %>% 
  mutate(type = "With Covariates")

marg <- bind_rows(marg3, marg4)

mef <- ggplot(marg, aes(x = x, y = predicted, ymin = conf.low, ymax = conf.high)) +
  geom_point() +
  geom_errorbar(width = .1) +
  facet_grid(. ~ type) +
  labs(x = "Treatment Group", y = "Predicted Income Change") +
  scale_y_continuous(labels = percent, breaks = c(1.5, 1.75, 2)) +
  theme_bw() +
  theme(text = element_text(family = "LM Roman 10", size = 13),
        legend.position = "bottom",
        panel.border = element_rect(fill = NA, 
                                    colour = "grey20"),
        strip.background = element_rect(fill = NA, 
                                        colour = "grey20"),
        plot.caption = element_text(hjust = 0))
mef

saveRDS(mef, "temp/income_mef.rds")
