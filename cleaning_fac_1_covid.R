# Cleaning script for skilled nursing facility covid data in November 2020

library(tidyverse)
library(lubridate)
library(tidycensus)

fac_2020 <- read.csv("faclevel_2020.csv", check.names=FALSE)


# keep relevant variables

fac <- fac_2020 %>%
  select(`Week Ending`, `Federal Provider Number`, `Federal Provider Number`, `Provider Name`, `Provider Address`, `Provider City`, `Provider State`, `Provider Zip Code`, County, `Submitted Data`, `Passed Quality Assurance Check`, `Number of All Beds`, `Total Number of Occupied Beds`, `Weekly Resident Confirmed COVID-19 Cases Per 1,000 Residents`, `Total Resident Confirmed COVID-19 Cases Per 1,000 Residents`, `Weekly Resident COVID-19 Deaths Per 1,000 Residents`, `Total Resident COVID-19 Deaths Per 1,000 Residents`, `Staff Weekly Confirmed COVID-19`, `Staff Total Confirmed COVID-19`, `Staff Weekly COVID-19 Deaths`, `Staff Total COVID-19 Deaths`) 
  

# keep only weeks in November

fac_nov2020 <- fac %>%
  mutate(week_ending=mdy(`Week Ending`)) %>%   # converts character dates to date format
  subset(week_ending > "2020-11-01" & week_ending < "2020-11-30") # subsets for Nov 2020

# download and prep county fips codes

data(fips_codes) # get fips codes

fips_codes <- fips_codes %>% # generate full fips code
  mutate(fips_code = paste(state_code, county_code)) %>%
  mutate(fips_code = gsub(" ", "", fips_code))

fips_codes <- fips_codes %>%  # prep county strings for merge
 mutate(county2 = gsub(" County", "", county)) %>%
  mutate(county2 = gsub(" Borough", "", county2)) %>%
  mutate(county2 = gsub(" Census Area", "", county2)) %>%
  mutate(county2 = gsub(" Parish", "", county2)) %>%
  mutate(county2 = gsub(" Municipality", "", county2)) %>%
  mutate(county2 = gsub(" and", "", county2)) 

# merge in county fips codes

fac_nov2020 <- fac_nov2020 %>% 
  rename(state = `Provider State`, county2 = County)
  
fac_nov2020 <- left_join(fac_nov2020, fips_codes, by = c("county2", "state"), keep = TRUE)

# remove extraneous variabls


# rename variables


# save



