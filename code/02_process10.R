

f10 <- read_fwf("raw_data/GA-2010/GA/09-27-10/voters/DailyVoterBase_SQL08.txt",
                col_positions = fwf_widths(c(3, 8, 1, 20, 20, 20, 3, 3, 6, 30, 2, 8, 17, 5, 4,
                                             1, 1, 1, 6, 2, 8, 8, 1, 1, 1, 3, 4, 8, 9, 5, 5, 5,
                                             3, 3, 3, 3, 3, 3, 13, 3, 13, 3, 17,
                                             3, 13, 3, 13, 3, 13, 3, 13, 3, 13, 3, 13, 3,
                                             8, 3, 1, 8, 6, 30, 2, 8, 17, 23, 5, 4, 2, 30, 30, 20,
                                             8, 8, 3, 3, 8, 60)))%>% 
  filter(X13 == "ATLANTA", X1 %in% c("060", "044"))

colnames(f10) <- fread("raw_data/GA-2010/GA/09-27-10/colnames_2010.csv", header = F)$V1

f10 <- select(f10, VOTER_FIRST_NAME, VOTER_LAST_NAME,
              REGISTRATION_NBR, DATE_OF_BIRTH, RACE, GENDER,
              RES_HOUSE_NBR, RES_STREET_NAME, RES_ZIPCODE, PARTY_LAST_VOTED,
              REGISTRATION_DATE)

f10 <- f10 %>% 
  mutate(street = paste(RES_HOUSE_NBR, RES_STREET_NAME))

addresses <- fread("raw_data/GA-2010/GA/09-27-10/voters/geo_ads_tamu.csv")

f10 <- left_join(f10, addresses[, c(38, 39, 9, 5, 6)] %>% 
                   select(street_raw, zipcode_raw, lat = Latitude, lon = Longitude, MatchScore),
                 by = c("street" = "street_raw", "RES_ZIPCODE" = "zipcode_raw"))

f10 <- filter(f10, is.finite(lat), is.finite(lon))
###############

tracts <- tracts("GA", c("FULTON", "DEKALB"), year = 2010, class = "sp")

pings  <- SpatialPoints(f10[c("lon", "lat")],
                        proj4string = tracts@proj4string)

f10$GEOID <- over(pings, tracts)$GEOID

p <- places("GA", class = "sp")
p <- subset(p, NAME == "Atlanta")

pings  <- SpatialPoints(f10[c("lon", "lat")],
                        proj4string = p@proj4string)

f10$at <- over(pings, p)$GEOID
f10 <- filter(f10, !is.na(at)) %>% 
  select(-at)
cleanup("f10", "tracts")
################
vars <- c("median_income" = "B06011_001",
          "renter_occ" = "B25007_012",
          "renter_count_denom" = "B25007_001",
          "median_gross_rent" = "B25113_001",
          "median_home_value" = "B25109_001")

census_1 <- get_acs(geography = "tract", variables = vars, year = 2010, state = "GA",
                    output = "wide") %>% 
  select(-ends_with("M")) %>% 
  rename_at(vars(ends_with("E")), ~ gsub("E", "", .)) %>% 
  mutate(share_renter = renter_occ / renter_count_denom) %>% 
  select(GEOID, median_income_2010 = median_income,
         median_gross_rent_2010 = median_gross_rent,
         share_renter_2010 = share_renter,
         median_home_value_2010 = median_home_value)

education <- census_education("tract", year = 2010, state = "GA") %>% 
  select(GEOID, some_college_10 = some_college)

race <- census_race_ethnicity("tract", 2010, "GA") %>% 
  select(GEOID, nh_white_2010 = nh_white, nh_black_2010 = nh_black)

citywide1 <- get_acs(geography = "place", variables = vars, year = 2010, state = "GA",
                     output = "wide") %>% 
  select(-ends_with("M")) %>% 
  rename_at(vars(ends_with("E")), ~ paste0(gsub("E", "", .), "_10")) %>% 
  mutate(share_renter_10 = renter_occ_10 / renter_count_denom_10) %>% 
  select(-renter_occ_10, -renter_count_denom_10) %>% 
  filter(NAM_10 == "Atlanta city, Georgia") %>% 
  select(-NAM_10)

citywide2 <- get_acs(geography = "place", variables = vars, year = 2019, state = "GA",
                     output = "wide") %>% 
  select(-ends_with("M")) %>% 
  rename_at(vars(ends_with("E")), ~ paste0(gsub("E", "", .), "_20")) %>% 
  mutate(share_renter_20 = renter_occ_20 / renter_count_denom_20) %>% 
  select(-renter_occ_20, -renter_count_denom_20) %>% 
  filter(NAM_20 == "Atlanta city, Georgia") %>% 
  select(-NAM_20)

citywide3 <- census_education("place", year = 2010, state = "GA") %>% 
  filter(NAME == "Atlanta city, Georgia") %>% 
  select(GEOID, some_college_10 = some_college)

citywide4 <- census_education("place", year = 2019, state = "GA") %>% 
  filter(NAME == "Atlanta city, Georgia") %>% 
  select(GEOID, some_college_20 = some_college)

citywide <- inner_join(citywide1,
                       inner_join(citywide2,
                                  inner_join(citywide3, citywide4)))

#####################################################

vars <- c("median_income" = "B06011_001",
          "renter_occ" = "B25007_012",
          "renter_count_denom" = "B25007_001",
          "median_gross_rent" = "B25113_001",
          "median_home_value" = "B25109_001")

census_20 <- get_acs(geography = "tract", variables = vars, year = 2019, state = "GA",
                     output = "wide") %>% 
  select(-ends_with("M")) %>% 
  rename_at(vars(ends_with("E")), ~ gsub("E", "", .)) %>% 
  mutate(share_renter = renter_occ / renter_count_denom) %>% 
  select(GEOID, median_income_2020 = median_income,
         median_gross_rent_2020 = median_gross_rent,
         share_renter_2020 = share_renter,
         median_home_value_2020 = median_home_value)

education_20 <- census_education("tract", year = 2019, state = "GA") %>% 
  select(GEOID, some_college_20 = some_college)

race_20 <- census_race_ethnicity("tract", 2019, "GA") %>% 
  select(GEOID, nh_white_2020 = nh_white, nh_black_2020 = nh_black)

census <- full_join(census_1,
                    full_join(census_20,
                              full_join(education,
                                        full_join(race,
                                                  full_join(education_20, race_20)))))

p <- places("GA", class = "sp")
p <- subset(p, NAME == "Atlanta")

tracts@data <- mutate(tracts@data,
                      lat = as.numeric(INTPTLAT10),
                      lon = as.numeric(INTPTLON10))

pings  <- SpatialPoints(tracts@data[c("lon", "lat")],
                        proj4string = p@proj4string)

tracts@data$at <- over(pings, p)$GEOID

census <- filter(census, GEOID %in% filter(tracts@data, !is.na(at))$GEOID)
#################################


census <- census %>% 
  mutate(potential = median_income_2010 < citywide$median_income_10,
         change_rent = (median_gross_rent_2020 - median_gross_rent_2010) / median_gross_rent_2010,
         change_home_value = (median_home_value_2020 - median_home_value_2010) / median_home_value_2010,
         change_ed = (some_college_20 - some_college_10) / some_college_10)

census$rg <- census$change_rent > quantile(census$change_rent, 0.5, na.rm = T)
census$vg <- census$change_home_value > quantile(census$change_home_value, 0.5, na.rm = T)
census$eg <- census$change_ed > quantile(census$change_ed, 0.5, na.rm = T)

census <- census %>% 
  mutate(gfied = (potential) & (rg | vg) & eg)

###################################################

f10 <- left_join(f10, census %>% 
                   select(GEOID, ends_with("_2010"),
                          potential, gfied))

ll <- f10 %>% 
  group_by(GEOID) %>% 
  summarize(white = mean(RACE == "W", na.rm = T),
            black = mean(RACE == "B", na.rm = T),
            dem = mean(PARTY_LAST_VOTED == "D", na.rm = T))

saveRDS(f10, "temp/processed_10.rds")

######################################
cleanup(c("census", "tracts", "f10", "ll"))

fort <- full_join(
  fortify(tracts),
  tracts@data %>% 
    mutate(id = rownames(tracts@data)) %>% 
    select(id, GEOID = GEOID10)
)

fort <- left_join(fort, census) %>% 
  mutate(set = ifelse(gfied, "Gentrified",
                      ifelse(potential, "Potential", "Not Gentrifiable"))) %>% 
  filter(!is.na(set))

fort$set <- factor(fort$set, levels = c("Not Gentrifiable", "Potential", "Gentrified"))

fort <- left_join(fort, ll)

saveRDS(fort, "temp/map_data.rds")

fort <- readRDS("temp/map_data.rds")
f10 <- readRDS("temp/processed_10.rds")

p1 <- ggplot() + 
  geom_polygon(data = filter(fort, !is.na(group), GEOID %in% f10$GEOID),
               aes(x = long, y = lat, group = group, fill = set),
               color = "black") +
  coord_map() +
  theme_bc(base_family = "LM Roman 10") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "bottom") +
  scale_fill_manual(values = c("white", "gray", "black")) +
  guides(fill = guide_legend(title.position = "top", title.hjust = 0.5)) +
  labs(fill = "Group\n") +
  ggtitle("(a) Neighborhood Classification")
p1
saveRDS(p1, "temp/gentrification_map.rds")

p2 <- ggplot(filter(fort, !is.na(group)), aes(x = long, y = lat, group = group, fill = nh_black_2010)) + 
  geom_polygon(color = "black") +
  coord_map() +
  theme_bc(base_family = "LM Roman 10") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "bottom") +
  scale_fill_gradient(low = "white", high = "black", labels = percent) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  labs(fill = "Share Black, 2010") +
  ggtitle("(b) Share Black in 2010")
p2
saveRDS(p2, "temp/black_map.rds")

p3 <- ggplot(filter(fort, !is.na(group)), aes(x = long, y = lat, group = group, fill = dem)) + 
  geom_polygon(color = "black") +
  coord_map() +
  theme_bc(base_family = "LM Roman 10") +
  theme(panel.background = element_blank(),
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        axis.line=element_blank(),axis.text.x=element_blank(),
        axis.text.y=element_blank(),axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        legend.position = "bottom") +
  scale_fill_gradient(low = "white", high = "black", labels = percent) +
  guides(fill = guide_colorbar(title.position = "top", title.hjust = 0.5)) +
  labs(fill = "Share Democrats")
p3
saveRDS(p3, "temp/dem_map.rds")


maps <- plot_grid(p1, p2)
saveRDS(maps, "temp/map_fig.rds")
