
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
                               ifelse(dist < 999999999999, "Moved in Georgia", "No Longer in Data"))),
         group = ifelse(gfied, "Gentrified",
                        ifelse(potential, "Gentrifiable", "Not Gentrifiable"))) %>% 
  select(reg_num = REGISTRATION_NBR,
         yob, black, white, reg_date, male, moved,
         ends_with("_2010"), gfied, potential, group, GEOID, GEOID_20,
         median_gross_rent_2020, median_income_2020)

m1 <- multinom(moved ~ group, full_set)

m2 <- multinom(moved ~ group +
                 yob + black + white +
                 reg_date + male + median_income_2010 +
                 median_gross_rent_2010 +
                 share_renter_2010 + median_home_value_2010, full_set)

marg1 <- ggeffect(model = m1, "group [all]") %>% 
  mutate(response.level = gsub("[.]", " ", response.level),
         type = "No Covariates")

marg2 <- ggeffect(model = m2, terms = "group [all]") %>% 
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

movers <- mutate(full_set, m = moved %in% c("Moved in Atlanta", "Moved in Georgia")) %>% 
  filter(moved != "No Longer in Data") %>% 
  mutate(change = (median_income_2020*0.820289667) / median_income_2010)

m3 <- feols(median_income_2020 ~ group*m +
              yob + black + white +
              reg_date + male + median_income_2010 +
              median_gross_rent_2010 +
              share_renter_2010 + median_home_value_2010, movers, cluster = c("GEOID"))

m4 <- feols(median_gross_rent_2020 ~ group*m +
           yob + black + white +
           reg_date + male + median_income_2010 +
           median_gross_rent_2010 +
           share_renter_2010 + median_home_value_2010, movers, cluster = c("GEOID"))

marg3 <- ggeffect(model = m3, terms = c("group", "m")) %>% 
  mutate(type = "With Covariates",
         group = ifelse(group == "TRUE", "Moved", "Did not Move"))

marg4 <- ggeffect(model = m4, terms = c("group", "m")) %>% 
  mutate(type = "With Covariates",
         group = ifelse(group == "TRUE", "Moved", "Did not Move"))

p1 <- ggplot(marg3, aes(x = x, y = predicted,
                        color = group,
                        ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(aes(y = predicted, x = x, linetype = group,
                                 ymin = conf.low, ymax = conf.high, color = group, 
                                 shape = group),
                             position = position_dodge(width = .5), 
                             fill = "white", fatten = 3,
                             size = 0.8, show.legend = T) +
  labs(x = "Treatment Group", y = "Predicted Median Income",
       color = "Moved?",
       linetype = "Moved?",
       shape = "Moved?") +
  scale_y_continuous(labels = dollar) +
  theme_bc(legend.position = "none", base_family = "LM Roman 10") +
  scale_color_manual(values = c("#9F142C", "black"))
p1

p2 <- ggplot(marg4, aes(x = x, y = predicted,
                        color = group,
                        ymin = conf.low, ymax = conf.high)) +
  geom_pointrange(aes(y = predicted, x = x, linetype = group,
                      ymin = conf.low, ymax = conf.high, color = group, 
                      shape = group),
                  position = position_dodge(width = .5), 
                  fill = "white", fatten = 3,
                  size = 0.8, show.legend = T) +
  labs(x = "Treatment Group", y = "Predicted Median Rent",
       color = "Moved?",
       linetype = "Moved?",
       shape = "Moved?") +
  scale_y_continuous(labels = dollar) +
  theme_bc(legend.position = "none", base_family = "LM Roman 10") +
  scale_color_manual(values = c("#9F142C", "black"))
p2

########################
legend_b <- get_legend(
  p1 + theme(legend.position = "bottom")
)
p <- plot_grid(p1 +
                 ggtitle("(A) Outcome Median Income"),
               p2 +
                 ggtitle("(B) Outcome Median Rent"),
               label_size = 12, ncol = 2,
               label_fontfamily = "LM Roman 10")

p
j <- plot_grid(p, legend_b, ncol = 1, rel_heights = c(1, .1))
j
f <- ggdraw(add_sub(j,
"Covariates include age, race, gender, registration date,
and 2010 neighborhood characteristics (median income, median rent,
share renter, median home value).
       
Standard errors clustered by 2010 tracts.", x = .05, hjust = 0,
                    size = 12, fontfamily = "LM Roman 10"))
f
saveRDS(f, "temp/income_mef.rds")
