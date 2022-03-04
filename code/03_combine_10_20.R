
f10 <- readRDS("temp/processed_10.rds")

r20 <- fread("../rolls/georgia/GA 2020-11-02/Georgia_Daily_VoterBase.txt",
             select = c("REGISTRATION_NUMBER", "RESIDENCE_HOUSE_NUMBER",
                        "RESIDENCE_STREET_NAME", "RESIDENCE_ZIPCODE")) %>% 
  filter(REGISTRATION_NUMBER %in% as.integer(f10$REGISTRATION_NBR)) %>% 
  mutate(street = paste(RESIDENCE_HOUSE_NUMBER, RESIDENCE_STREET_NAME),
         RESIDENCE_ZIPCODE = as.integer(RESIDENCE_ZIPCODE))

addresses <- fread("raw_data/GA-2010/GA/09-27-10/voters/geo_ads_tamu.csv")

r20 <- left_join(r20, addresses[, c(38, 39, 9, 5, 6)] %>% 
                   select(street_raw, zipcode_raw, lat = Latitude, lon = Longitude, MatchScore),
                 by = c("street" = "street_raw", "RESIDENCE_ZIPCODE" = "zipcode_raw"))


r20 <- filter(r20, is.finite(lat), is.finite(lon))
###############
p <- places("GA", class = "sp")
p <- subset(p, NAME == "Atlanta")

pings  <- SpatialPoints(select(r20, lon, lat),
                        proj4string = p@proj4string)

r20$at_20 <- !is.na(over(pings, p)$GEOID)

tracts <- tracts("GA", year = 2019, class = "sp")

pings  <- SpatialPoints(r20[,c("lon", "lat")],
                        proj4string = tracts@proj4string)

r20$GEOID_20 <- over(pings, tracts)$GEOID

r20 <- select(r20, REGISTRATION_NBR = REGISTRATION_NUMBER,
              lat_20 = lat, lon_20 = lon, GEOID_20, at_20)

f10 <- left_join(f10 %>% 
                   mutate(REGISTRATION_NBR = as.integer(REGISTRATION_NBR)), r20)
cleanup("f10")
##########################################

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

census <- full_join(census_20,
                    full_join(education_20, race_20))

f10 <- left_join(f10, census, by = c("GEOID_20" = "GEOID"))

############################

f10$dist <- pointDistance(dplyr::select(f10, lon,
                                        lat),
                          dplyr::select(f10, lon_20, lat_20), lonlat = T)


ll <- f10 %>% 
  mutate(change_income = median_income_2020 / median_income_2010,
         move = dist > 10,
         b = RACE == "B",
         at20 = GEOID_20 %in% f10$GEOID) %>% 
  group_by(potential, gfied, b) %>%
  summarize(mi = mean(is.na(GEOID_20)), n = n(),
            change_income = mean(change_income, na.rm = T),
            move = mean(move, na.rm = T),
            at20 = mean(at20, na.rm = T),
            dist = mean(dist, na.rm = T),
            inc_10 = mean(median_income_2010, na.rm = T))

saveRDS(f10, "temp/combine_10_20.rds")
