####### This is a script that adds information for region, rurality, and county-level covid spread to the snf-level df.

# Prepare environ ----

# load packages
library(tidyverse)
library(janitor)

# load data
load("snfs_nn_all.Rda")
load("cty_covid_nov.Rda")
nchs <- read_csv("nchs_codes.csv")

## Add in U.S. regions ----

# get vectors for regions by state
st_region <- read_csv("https://raw.githubusercontent.com/cphalpert/census-regions/master/us%20census%20bureau%20regions%20and%20divisions.csv") %>%
  rename(state = `State Code`, region = `Region`) %>%
  select(state, region)

# merge regions into df

snfs_nn_all <- merge(snfs_nn_all, st_region, by="state", all=TRUE)
sum(is.na(snfs_nn_all$region))


# Add in rurality codes ----

# clean nchs df 

nchs_codes <- nchs %>%
  select(`FIPS code`, `2013 code`) %>%
  rename(fips_code = `FIPS code`) %>%
  rename(nchs_code = `2013 code`) %>%
  mutate(fips_code = str_pad(fips_code, 5, pad = "0"))


# generate metro & non-metro variable

nchs_codes <- nchs_codes %>%
  mutate(metro = ifelse(nchs_code == "5" | nchs_code =="6", 0, 1))

# merge in rurality codes

snfs_nn_all <- merge(snfs_nn_all, nchs_codes, by = "fips_code")

sum(is.na(snfs_nn_all$nchs_code))

# Add in county covid data ----

# fix VA fips code for SNFs

snfs_nn_all <- snfs_nn_all %>%
  mutate(fips_code = ifelse(fips_code=="51515", "51019", fips_code)) 


# merge and check
snfs_nn_all <- merge(snfs_nn_all, cty_covid_nov, by = "fips_code", all.x=TRUE)
sum(is.na(snfs_nn_all$nov_cases_avg_per_100k))


# Explore ----

quantile(snfs_nn_all$dist_miles)
upper_fence = 2.517 + (1.5*(2.517-0.57))

under5 <- snfs_nn_all %>% filter(dist_miles < 5)

over5 <- snfs_nn_all %>% filter(dist_miles > 5)

under5 <- under5 %>%
  mutate(any_res_case = ifelse(residents_total_confirmed_covid_19 > 0, 1, 0))

over5 <- over5 %>%
  mutate(any_res_case = ifelse(residents_total_confirmed_covid_19 > 0, 1, 0))

res <- prop.test(x = c(3434, 1572), n= c(12172, 2987))

res2 <- prop.test(x = c(57, 4949), n=c(105, 15054))

### SAVE FILE

save(snfs_nn_all, file = "snfs_fullsample.Rda")
