r10 <- read_fwf("raw_data/GA-2010/GA/09-27-10/voters/DailyVoterBase_SQL08.txt",
                col_positions = fwf_widths(c(3, 8, 1, 20, 20, 20, 3, 3, 6, 30, 2, 8, 17, 5, 4,
                                             1, 1, 1, 6, 2, 8, 8, 1, 1, 1, 3, 4, 8, 9, 5, 5, 5,
                                             3, 3, 3, 3, 3, 3, 13, 3, 13, 3, 17,
                                             3, 13, 3, 13, 3, 13, 3, 13, 3, 13, 3, 13, 3,
                                             8, 3, 1, 8, 6, 30, 2, 8, 17, 2, 5, 4, 2, 30, 30, 20,
                                             8, 8, 3, 3, 8, 60)))

r20 <- fread("../rolls/georgia/GA 2020-11-02/Georgia_Daily_VoterBase.txt",
             select = c("REGISTRATION_NUMBER", "RESIDENCE_HOUSE_NUMBER",
                        "RESIDENCE_STREET_NAME", "RESIDENCE_ZIPCODE")) %>% 
  filter(REGISTRATION_NUMBER %in% as.integer(r10$X2)) %>% 
  mutate(street = paste(RESIDENCE_HOUSE_NUMBER, RESIDENCE_STREET_NAME))


hold <- fread("raw_data/GA-2010/GA/09-27-10/voters/geo_ads_tamu.csv")

r20 <- unique(r20 %>% 
                select(street, RESIDENCE_ZIPCODE))

r20 <- filter(r20,
              !(paste0(street, RESIDENCE_ZIPCODE) %in% paste0(hold$street_raw, hold$zipcode_raw)))

for(i in c(1:nrow(r20))){
  te <- filter(r20, row_number() == i)
  
  print(i)
  es = GET("https://geoservices.tamu.edu/Services/Geocode/WebService/GeocoderWebServiceHttpNonParsed_V04_01.aspx?",
           query = list(streetAddress= te$street,
                        city="Atlanta",
                        state="GA",
                        zip=te$RESIDENCE_ZIPCODE,
                        apiKey="34134ae2bb26464991daef81e3d7cfd2",
                        format="json",
                        version="4.01",
                        census = "true",
                        censusYear = "2010"))
  
  data = fromJSON(rawToChar(es$content))
  
  n <- cbind(data$InputAddress,
             data[["OutputGeocodes"]][["OutputGeocode"]],
             data[["OutputGeocodes"]][["CensusValues"]][[1]][["CensusValue1"]])
  n$street_raw = te$street
  n$zipcode_raw = te$RESIDENCE_ZIPCODE
  
  hold <- rbind(hold, n)
}

fwrite(hold, "raw_data/GA-2010/GA/09-27-10/voters/geo_ads_tamu.csv")

