#### This is a script that gets state-evel average bed counts for facilities.

library(tidyverse)

# load nursing home file

load(file = "fac_nov2020.Rda")

# condense to just one week for calculating averages

fac_nov2020 <- fac_nov2020 %>%
  subset(week_ending == "2020-11-08" ) # subsets for first week of Nov 2020

nrow(fac_nov2020)

# check data

sum(is.na(fac_nov2020$`Number of All Beds`)) # checks how many missing bed counts there are

# create new df with average bed count by state

st_facsize <- fac_nov2020 %>% group_by(state.x) %>% summarise(mean_facsize=mean(`Number of All Beds`, na.rm=TRUE))

# save df





# ALTERNATE: Get counts of beds >=100

fac_nov2020 <- fac_nov2020 %>%
  mutate(bed_100 = if_else(`Number of All Beds` >=100, 1, 0))

st_fac100 <- fac_nov2020 %>% group_by(state.x) %>% summarise(count_100plusbeds=sum(bed_100 == 1, na.rm=TRUE))
