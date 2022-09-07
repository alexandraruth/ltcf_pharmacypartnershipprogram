#### This is a cleaning script that pulls November 2020 data for 
#### skilled nursing facilities from the CMS federal dataset.

### PREPARE ENVIRONMENT ----

# load packages

library(tidyverse)
library(lubridate)
library(tidycensus)

# load csv file downloaded from CMS website with ALL 2020 data for SNFs

fac_2020 <- read.csv("cms_fac_2020.csv", check.names=FALSE)


### PREPARE FIPS CODES for index ----

# download and prep county fips codes

data(fips_codes) # get fips codes

fips_codes <- fips_codes %>% # generate full fips code
  mutate(fips_code = paste(state_code, county_code)) %>%
  mutate(fips_code = gsub(" ", "", fips_code))

fips_codes <- fips_codes %>%  # prep county strings for merge
  mutate(county2 = gsub(" County", "", county)) %>%
  mutate(county2 = gsub(" and Borough", "", county2)) %>%
  mutate(county2 = gsub(" Borough", "", county2)) %>%
  mutate(county2 = gsub(" Census Area", "", county2)) %>%
  mutate(county2 = gsub(" Parish", "", county2)) %>%
  mutate(county2 = gsub(" Municipality", "", county2)) %>%
  mutate(county2 = gsub(" Gateway", "", county2)) %>%
  mutate(county2 = gsub(" North Star", "", county2)) 




### SNF DATAFRAME CLEANING ----

# keep only relevant variables

fac <- fac_2020 %>%
  select(`Week Ending`, `Federal Provider Number`, `Federal Provider Number`, `Provider Name`, `Provider Address`, `Provider City`, `Provider State`, `Provider Zip Code`, County, `Submitted Data`, `Passed Quality Assurance Check`, `Number of All Beds`, `Total Number of Occupied Beds`, `Residents Weekly Confirmed COVID-19`, `Residents Total Confirmed COVID-19`, `Residents Weekly COVID-19 Deaths`, `Residents Total COVID-19 Deaths`, `Weekly Resident Confirmed COVID-19 Cases Per 1,000 Residents`, `Total Resident Confirmed COVID-19 Cases Per 1,000 Residents`, `Weekly Resident COVID-19 Deaths Per 1,000 Residents`, `Total Resident COVID-19 Deaths Per 1,000 Residents`, `Staff Weekly Confirmed COVID-19`, `Staff Total Confirmed COVID-19`, `Staff Weekly COVID-19 Deaths`, `Staff Total COVID-19 Deaths`) 
  
# keep only weeks in November

fac_nov2020 <- fac %>%
  mutate(week_ending=mdy(`Week Ending`)) %>%   # converts character dates to date format
  subset(week_ending > "2020-11-01" & week_ending < "2020-11-30") # subsets for Nov 2020

# make variable names easier to use
fac_nov2020 <- fac_nov2020 %>%
  rename(state = `Provider State`)

# fix Virginia county names to lowercase for merge

fac_nov2020 <- fac_nov2020 %>%
  mutate(County = ifelse(state=="VA", gsub("City", "city", County), County)) 


# fix other county names for merge

fac_nov2020 <- fac_nov2020 %>%
  mutate(County = gsub("District Of Columbia", "District of Columbia", County)) %>%
  mutate(County = ifelse(state=="NH" & County=="Hillsboro", gsub("Hillsboro", "Hillsborough", County), County)) %>%
  mutate(County = gsub("Wrangell-Petersburg", "Wrangell City", County), County) %>%
  mutate(County = gsub("Kenai-Cook Inlet", "Kenai Peninsula", County), County) %>%
  mutate(County = ifelse(state=="AK" & County=="Juneau", gsub("Juneau", "Juneau City", County), County)) %>%
  mutate(County = gsub("Cordova-Mccarthy", "Valdez-Cordova", County), County) %>%
  mutate(County = gsub("Kodiak Island Borough", "Kodiak Island", County)) %>%
  mutate(County = gsub("Sitka", "Sitka City", County)) %>%
  mutate(County = gsub("Not Available", "Northwest Arctic", County)) %>%
  mutate(County = gsub("Valdez-Chitina-Whittier", "Valdez-Cordova", County)) %>%
  mutate(County = ifelse(state=="AK" & County=="Seward", gsub("Seward", "Kenai Peninsula", County), County)) %>%
  mutate(County = gsub("Albermarle", "Albemarle", County)) %>%
  mutate(County = gsub("Baltimore City", "Baltimore city", County)) %>%
  mutate(County = gsub("St. Louis City", "St. Louis city", County)) %>%
  mutate(County = gsub("James city", "James City", County)) %>%
  mutate(County = gsub("Poquoson", "Poquoson city", County)) %>%
  mutate(County = ifelse(state=="LA" & County=="LaSalle", gsub("LaSalle", "La Salle", County), County)) 
  


  # remove puerto rico and other regions not of interest
  
  fac_nov2020 <- fac_nov2020 %>%
  filter(state != "PR") %>%
  filter(state != "GU") %>%
  filter(state != "AS") %>%
  filter(state != "MP") %>%
  filter(state!= "VI") %>%
  filter(state != "UM") 


# MERGE: County FIPS Codes into SNF dataframe ----

fac_nov2020 <- fac_nov2020 %>% 
  rename(county2 = County)
  
fac_nov2020 <- left_join(fac_nov2020, fips_codes, by = c("county2", "state"), keep = TRUE)

# check for counties that didn't merge

cty_na <- fac_nov2020 %>% 
  subset(week_ending == "2020-11-08" ) %>%
  filter(is.na(fips_code))

# remove extraneous variables and clean up variable names

fac_nov2020 <- fac_nov2020 %>%
  select(-state.y, -state_code, -county_code, -county, -county2.y) %>%
  rename(state = state.x) %>%
  rename(county = county2.x)
  


# save

save(fac_nov2020, file = "fac_nov2020.Rda")

