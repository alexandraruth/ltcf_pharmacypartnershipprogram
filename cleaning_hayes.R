############ MAPPING NURSING HOMES AND PHARMACIES 
############ Data preparation - extracting pharmacy info from Hayes database
############ Alexandra Ruth
############ July 2021

## Prepare working environment ----

# install packages
library(tidyverse)
library(janitor)

## Read in data ----

hayes <- read_csv("hayes_2021.csv")

## Get variables of interest

# get CVS and Walgreens; keep list of all CVS & Walgreens locations

hayes <- hayes %>%
  select(-PHONE, -FAX, -CHQ, -STORENUM, -BRANCH, -OTHER, - POP, -FULLSTATE, -`NEW STORE`)
 
cvs <- hayes %>%
  filter(str_detect(NAME, "CVS")) %>%
  mutate(cvs = 1) %>%
  mutate(walgreen = 0)

walgreen <- hayes %>%
  filter(str_detect(NAME, "Walgreen")) %>%
  mutate(cvs = 0) %>%
  mutate(walgreen = 1)
  
cvs_wal <- rbind(cvs, walgreen)
save(cvs_wal, file = "cvs_wal.Rda")


# get counts of CVS and Walgreen facilities by state

cvs_count <- cvs %>% 
  count(STATE, sort=TRUE) %>%
  rename(cvs_num = n)
  
wal_count <- walgreen %>% 
  count(STATE, sort=TRUE) %>%
  rename(wal_num = n)

cvswal_count <- merge(cvs_count, wal_count, by='STATE')

save(cvswal_count, file = "cvswal_count.Rda")

# convert addresses to geolocations

# keep: pharmacy name, geolocation, state, county

# save 


##### RESOURCES FOR 




# converting addresses to geocodes - resources:
## https://towardsdatascience.com/geocode-with-python-161ec1e62b89
## https://towardsdatascience.com/pythons-geocoding-convert-a-list-of-addresses-into-a-map-f522ef513fd6 


