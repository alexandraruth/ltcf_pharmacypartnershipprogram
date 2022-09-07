##### This is a script that creates a county-level dataframe for generating table 3.

###  Prepare environment ----

# load packages

library(tidyverse)
library(janitor)
library(tidycensus)

# load data

load("hayes_cty.Rda") # Hayes Directories database of community pharmacies in US
load("fac_nov2020.Rda") # Facility-level data for COVID in skilled nursing facilities in Nov. 2020
load("cty_covid_nov.Rda") # County-level COVID data in Nov. 2020

nchs <- read_csv("nchs_codes.csv")
ltcfocus_county <- read_csv("ltcfocus_county_2020.csv")

data(fips_codes) # county-level FIPS codes for merges


# 1. Create root df with counties as id for each row

county <- data.frame(fips_codes) 
county <- county %>%
  mutate(fips_code = paste(state_code, county_code, sep = ""))


# 2. Merge in: County-level CVS and Walgreens info ----

# get df with cvs and walgreens counts

county_cvs <- hayes_cty %>%
  filter(str_detect(name, "CVS"))

county_wal <- hayes_cty %>%
  filter(str_detect(name, "Walgreen"))

rm(hayes_cty)

county_cvs_num <- county_cvs %>%
  group_by(fips_code) %>%
  summarise(cvs_n = n())

county_wal_num <- county_wal %>%
  group_by(fips_code) %>%
  summarise(wal_n = n())

county <- merge(county, county_cvs_num, by="fips_code", all = T)
county <- merge(county, county_wal_num, by="fips_code", all = T)

county[is.na(county)] <- 0

# generate variable for presence of any cvs or walgreens

county <- county %>% 
  mutate(cvswal_any = ifelse(cvs_n>0 | wal_n>0, 1, 0))

# save a copy of this df for later work
save(county, file = "cty_cvswal_n.Rda")

# 3. Merge in: County-level NCHS urban-rural continuum codes ----

nchs_codes <- nchs %>%
  select(`FIPS code`, `2013 code`) %>%
  rename(fips_code = `FIPS code`) %>%
  rename(nchs_code = `2013 code`) %>%
  mutate(fips_code = str_pad(fips_code, 5, pad = "0"))

county <- merge(county, nchs_codes, by="fips_code", all=T)

# generate metro & non-metro variable

county <- county %>%
  mutate(metro = ifelse(nchs_code == "5" | nchs_code =="6", 0, 1))

# 4. Merge in: County-level COVID info ----

county <- merge(county, cty_covid_nov, by="fips_code", all = T)

# remove Puerto Rico, Guam, etc.

county <- county %>%
  filter(state != "PR") %>%
  filter(state != "GU") %>%
  filter(state != "AS") %>%
  filter(state != "MP") %>%
  filter(state != "VI") %>%
  filter(state != "UM") 

# 5. Merge in relevant census info ----

# 6. Merge in LTCFocus data ----

ltcfocus_county <- ltcfocus_county %>%
  select(state, county, totbeds_cty, paymcaid_cty, pctblack_mds3_cty, pcthisp_mds3_cty, pctwhite_mds3_cty)

state_fips <- fips_codes %>%
  select(state, state_code) %>%
  distinct(state, state_code) 

ltcfocus_county <- merge(ltcfocus_county, state_fips, by="state") %>%
  mutate(fips_code = paste(state_code, "", county)) %>%
  mutate(fips_code = gsub(" ", "", fips_code)) %>%
  select(-state_code, -county, -state)

county <- merge(county, ltcfocus_county, by="fips_code", all=T)

# 7. Merge in: SNF-related info for each county  ----

# remove SNFs that did not pass quality check



# get week of interest; one SNF per row
snf_county_num <- fac_nov2020 %>%
  subset(week_ending == "2020-11-08" ) %>%
  group_by(fips_code) %>%
  summarise(snfs_num = n())

county <- merge(county, snf_county_num, by="fips_code", all=T)

county[is.na(county)] <- 0 # set NA values equal to 0


df <- county %>%   # CHECK: see how many SNFs were in counties w/o a cvs or wal
  group_by(cvswal_any) %>% 
  summarise(snfcount = sum(snfs_num, na.rm=TRUE))



### NEED TO FIGURE OUT: What happened to row with 227 SNFs and no other info?...

# get # snfs in counties without CVS or Wal

snfct <- county %>%
  group_by(cvswal_any) %>%
  summarise(snfcount = sum(snfs_num))



############################
# TABLE VALUES
############################

#### TABLE 3A

# total counties with and without CVS/Wal
tabyl(county$cvswal_any) 

# total number of snfs in counties

county %>% summarise(totsnfs = sum(snfs_num))
county %>% group_by(cvswal_any) %>% summarise(totsnfs = sum(snfs_num))

# county covid transmission
mean(county$nov_cases_avg_per_100k, na.rm=TRUE)

county %>% group_by(cvswal_any) %>% summarise(mean=mean(nov_cases_avg_per_100k, na.rm=TRUE))

# Avg facility size (beds)



# % of counties classified as metropolitan
t_metro <- county %>%
  tabyl(cvswal_any, metro) %>%
  adorn_percentages("row") %>%
  adorn_pct_formatting(digits = 2) %>%
  adorn_ns()
t_metro


# county population

