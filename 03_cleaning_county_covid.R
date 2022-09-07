##### This is a script that pulls November 2020 county-level covid transmission data to prepare
##### for a merge with facility-level data.

## Source: NYT Covid data
## Github repo: https://github.com/nytimes/covid-19-data/tree/master/rolling-averages


library(tidyverse)
library(lubridate)

# pull county-level case and death data from NYT Github repo

urlfile ="https://raw.githubusercontent.com/nytimes/covid-19-data/master/rolling-averages/us-counties-2020.csv"
nyt_covid <- read_csv(url(urlfile))

head(nyt_covid)

# filter November - December counts only

cty_covid <- nyt_covid %>%
  subset(date >= "2020-11-01" & date <= "2020-11-30")

cty_covid <- cty_covid %>%
  arrange(county) %>%
  arrange(state)

rm(nyt_covid)

# get average case rate by county for November

cty_covid <- cty_covid %>%
  arrange(state, county)

cty_covid_nov <- cty_covid %>%
  group_by(geoid) %>%
  summarise(nov_cases_avg_per_100k = round(mean(cases_avg_per_100k), digits = 2))

# clean up names and variables

cty_covid_nov <- cty_covid_nov %>%
  mutate(fips_code = gsub("USA-", "", geoid)) %>%
  select(-geoid)

# check for outliers or implausible values 

## Crowley County in CO: Has an implausibly high 571.62 value; more likely to be 57.2 and a decimal point error
## FIPS = 08025

cty_covid_nov <- cty_covid_nov %>%
  mutate(nov_cases_avg_per_100k = ifelse(fips_code=="08025", 57.16, nov_cases_avg_per_100k))
  

# need to fill in missing NYC counties

nyc_row <- cty_covid_nov %>% filter(fips_code=="36998")

fips_code <- c("36005", "36047", "36061", "36081", "36085")
nov_cases_avg_per_100k <- c(0,0,0,0,0)

missing_nyc <- data.frame(fips_code, nov_cases_avg_per_100k)
missing_nyc <- missing_nyc %>% mutate(nov_cases_avg_per_100k = nyc_row$nov_cases_avg_per_100k)

cty_covid_nov <- rbind(cty_covid_nov, missing_nyc)

# save file with county fips codes and average November covid case rates

save(cty_covid_nov, file = "cty_covid_nov.Rda")
