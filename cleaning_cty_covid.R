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

# save file with county fips codes and average November covid case rates

save(cty_covid_nov, file = "cty_covid_nov.Rda")
