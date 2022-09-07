#### This script generates simple maps for appendices.

# Prepare environment ----

library(usmap)
library(ggplot2)
library(tidyverse)

# load data
load("~/Documents/GitHub/geocoding_pharm_snf/snf_geo_all.Rda")
load("~/Documents/GitHub/geocoding_pharm_snf/pharm_geo_all.Rda")
load("snfs_fullsample.Rda")
load("cty_covid_nov.Rda")

# convert dfs

snf_geo <- snf_geo_all %>%
  select(lon, lat)
transform_snf <- usmap_transform(snf_geo, input_names = c("lon", "lat"), output_names = c("x", "y"))

pharm_geo <- pharm_geo_all %>%
  select(lon, lat)

transform_pharm <- usmap_transform(pharm_geo, input_names = c("lon", "lat"), output_names = c("x", "y"))


# plot SNFs

plot_usmap("states") + 
  geom_point(data = transform_snf, 
             aes(x = x, y = y), 
             color = "red",
             size = 0.001)


# plot pharmacies

plot_usmap("states") + 
  geom_point(data = transform_pharm, 
             aes(x = x, y = y), 
             color = "blue",
             size = 0.001)

# plot those that did not match (pharm_within75==0)

ineligible <- snfs_nn_all %>%
  filter(pharm_within75==0) %>%
  select(longitude, latitude)

transform_ineligible <- usmap_transform(ineligible, input_names = c("longitude", "latitude"), output_names = c("x", "y"))


plot_usmap("states") + 
  geom_point(data = transform_ineligible, 
             aes(x = x, y = y), 
             color = "green",
             size = 0.5)


# overlay pharmacies with SNFs

plot_usmap("states") + 
  geom_point(data = transform_snf, 
             aes(x = x, y = y), 
             color = "red",
             size = 0.001) + 
  geom_point(data = transform_pharm, 
             aes(x = x, y = y), 
             color = "blue",
             size = 0.001)


# try plotting facilities & locations


plot_usmap("states") + 
  geom_point(data = transform_ineligible, 
             aes(x = x, y = y), 
             color = "green",
             size = 0.5) +
  geom_point(data = transform_pharm, 
             aes(x = x, y = y), 
             color = "blue",
             size = 0.05)
  


# map counties in US

plot_usmap(regions = "counties") + 
  labs(title = "US Counties",
       subtitle = "This is a blank map of the counties of the United States.") + 
  theme(panel.background = element_rect(color = "black", fill = "lightblue"))

# map covid spread in counties in U.S. 
cty_covid_nov <- cty_covid_nov %>%
  rename(fips = fips_code)


plot_usmap(
  data = cty_covid_nov, values = "nov_cases_avg_per_100k", color = "black", size = 0.05) + 
  scale_fill_continuous(
    low = "white", high = "red", name = "Case rates", label = scales::comma
  ) + 
  labs(title = "Covid rates in U.S. counties (7-day rolling averages per 100k)") +
  theme(legend.position = "right")


