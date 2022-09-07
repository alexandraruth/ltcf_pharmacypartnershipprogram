#################################
#This is a script to geocode SNFs with missing coordinates in the analytic sample and get nearest-neighbor calculations for those that were missing. 
#################################

### Prepare working environment ----

# load packages
library(tidyverse)
library(janitor)
library(tidygeocoder)
library(nngeo)


# only run once: API key

Sys.setenv(GOOGLEGEOCODE_API_KEY = "AIzaSyBD21xS5Y2YgE0yuDqRam4HVSgw6QALmKw")


# load data
load("snfs_covidpharm.Rda")

load("~/Documents/GitHub/geocoding_pharm_snf/nn_snf_geo_all.Rda")
load("~/Documents/GitHub/geocoding_pharm_snf/pharm_geo_all.Rda")



# Merge & inspect ----

merge <- merge(snfs_covidpharm, snf_geo_all, by = "federal_provider_number", all.x = TRUE)
sum(is.na(merge$lon))


# Geocode missing SNF rows ----

snfs_missinggeo <- merge %>% filter(is.na(lon))

snfs_hasgeo <- merge %>% filter(!is.na(lon))

# prepare df for geocoding

snfs_missinggeo <- snfs_missinggeo %>%
  select(-provider_name.y, -lon, -lat, -`as.character(nn)`, -pharm_within75, -dist_meters, -dist_miles) %>%
  rename(provider_name = provider_name.x) %>%
  mutate(address_long = paste0(provider_name, " ", provider_address, ", ", provider_city, " ", provider_state, " ", provider_zip_code))

# geocode

snfs_geocoded <- snfs_missinggeo %>%
  geocode(address_long, method = 'google', lat = latitude , long = longitude)

# NN calc for newly geocoded rows ----

# convert to point layers

snf_sf <- st_as_sf(snfs_geocoded, coords = c("longitude", "latitude"), crs = 4326, na.fail = FALSE, agr = "constant")
pharm_sf <- st_as_sf(pharm_geo_all, coords = c("lon", "lat"), crs = 4326, na.fail = FALSE, agr = "constant")


# get distance to nearest neighbor
dist=st_nn(snf_sf, pharm_sf, k=1, returnDist = TRUE, progress = FALSE)
dist


# get yes/no nearest neighbor within radius of 75mi
nn = st_nn(snf_sf, pharm_sf, k=1, maxdist = 120702, progress = FALSE)
nn

# merge distances into df

dist_df <- as.data.frame(dist$dist)
rownames(dist_df) = NULL
dist_df <- as.data.frame(t(dist_df))
ids = c(1:34)
rownames(dist_df) = ids

snfs_geocoded_nn <- cbind(snfs_geocoded, dist_df)

snfs_geocoded_nn <- snfs_geocoded_nn %>% 
  rename(dist_meters = V1) %>%
  mutate(dist_miles = dist_meters / 1609) %>%
  mutate(pharm_within75 = ifelse(dist_miles < 75, 1, 0)) %>%
  select(-address_long)
  

# Bind newly geocoded and NN-ed df back in ----


# prepare original df for binding

snfs_hasgeo <- snfs_hasgeo %>%
  select(-provider_name.y, -`as.character(nn)`) %>%
  rename(provider_name = provider_name.x) %>%
  rename(longitude = lon) %>%
  rename(latitude = lat)

snfs_nn_all <- rbind(snfs_hasgeo, snfs_geocoded_nn)

# Save ----

save(snfs_nn_all, file = "snfs_nn_all.Rda")
