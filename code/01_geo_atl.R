
j1 <- read_fwf("raw_data/GA-2010/GA/09-27-10/voters/DailyVoterBase_SQL08.txt",
               col_positions = fwf_widths(c(3, 8, 1, 20, 20, 20, 3, 3, 6, 30, 2, 8, 17, 5, 4,
                                            1, 1, 1, 6, 2, 8, 8, 1, 1, 1, 3, 4, 8, 9, 5, 5, 5,
                                            3, 3, 3, 3, 3, 3, 13, 3, 13, 3, 17,
                                            3, 13, 3, 13, 3, 13, 3, 13, 3, 13, 3, 13, 3,
                                            8, 3, 1, 8, 6, 30, 2, 8, 17, 2, 5, 4, 2, 30, 30, 20,
                                            8, 8, 3, 3, 8, 60)))

atl <- filter(j1, X13 == "ATLANTA", X1 %in% c("060", "044"))

hold <- fread("raw_data/GA-2010/GA/09-27-10/voters/geo_ads_tamu.csv")

atl <- atl %>% 
  mutate(street = paste(X9, X10)) %>% 
  select(street, zipcode = X14)

atl <- unique(atl)

atl <- filter(atl,
              !(paste0(street, zipcode) %in% paste0(hold$street_raw, hold$zipcode_raw)))

for(i in c(1:nrow(atl))){
  te <- filter(atl, row_number() == i)
  
  print(i)
  es = GET("https://geoservices.tamu.edu/Services/Geocode/WebService/GeocoderWebServiceHttpNonParsed_V04_01.aspx?",
           query = list(streetAddress= te$street,
                        city="Atlanta",
                        state="GA",
                        zip=te$zipcode,
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
  n$zipcode_raw = te$zipcode
  
  hold <- rbind(hold, n)
}

fwrite(hold, "raw_data/GA-2010/GA/09-27-10/voters/geo_ads_tamu.csv")
