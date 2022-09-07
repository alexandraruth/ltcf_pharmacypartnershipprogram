#################################
# TABLE 3b: 
#This is a script to generate SNF-level information for table 3b.
#################################

### Prepare working environment ----

# load packages
library(tidyverse)
library(janitor)
library(table1)

# load data
load("fac_nov2020.Rda")
load("cty_cvswal_n.Rda")

ltcfocus_fac <- read_csv(file = "ltcfocus_fac_2020.csv")

# Prepare data frames ----

fac_nov2020 <- fac_nov2020 %>%
  clean_names()

ltcfocus_fac <- ltcfocus_fac %>%
  select(accpt_id, PROV1680, PROV0475, state, county, totbeds, nresid, paymcaid, pctblack_mds3, pcthisp_mds3, pctfluvax, pctwhite_mds3) %>%
  rename(federal_provider_number = accpt_id) 

### Generate exclusion codes for data quality ----

# include if: 
# - data reported for all four weeks
# - data submitted for all four weeks
# - data passed QA for all four weeks
# - all case data complete
# - LTCFocus data matched
# - county-level pharmacy info matched


# inclusion variables:
# included = 1 (yes); 0 (no)
# reason_exclusion = data_dns; qa_dnp; week_missing; sus_value


# check completeness of 1) Covid data 2) data submission 3) qa pass and generate inclusion/exclusion variables

inclusion_snfs <- fac_nov2020 %>%
  mutate(include = NA) %>%
  mutate(reason_excluded = as.character(NA))

provider_ids <- c(unique(fac_nov2020$federal_provider_number))

results <- data.frame(inclusion_snfs[1,]) 
results <- results[-1,]


for (i in provider_ids) {
  
  single_snf <- inclusion_snfs %>%
    filter(federal_provider_number == paste0(i))
  
  n_weeks = nrow(single_snf)
  
  single_snf <- single_snf %>% mutate(reason_excluded = ifelse(any(submitted_data=="N"), "data_dns", reason_excluded))
  single_snf <- single_snf %>% mutate(reason_excluded = ifelse(n_weeks < 4, "weeks_missing", reason_excluded))
  single_snf <- single_snf %>% mutate(reason_excluded = ifelse(any(passed_quality_assurance_check=="N"), "qa_dnp", reason_excluded))
  single_snf <- single_snf %>% mutate(include = ifelse(is.na(reason_excluded), 1, 0))
  
  results <- rbind(results, single_snf)
}


save(results,  file = "results.Rda")

# Get Covid data and make single row for each SNF ----

results_covid <- data.frame(results[1,]) %>%
  mutate(mean_wkly_resident_cases = NA) %>%
  mutate(mean_wkly_resident_deaths = NA) %>%
  mutate(tot_resident_cases = NA) %>%
  mutate(tot_resident_deaths = NA) %>%
  mutate(mean_wkly_staff_cases = NA) %>%
  mutate(tot_staff_cases = NA)
results_covid <- results_covid[-1,]


for (i in provider_ids) {
  
  single_snf <- results %>%
    filter(federal_provider_number == paste0(i))
  
  row <- single_snf[1,]
  
  row <- row %>% 
    mutate(mean_wkly_resident_cases = mean(single_snf$residents_weekly_confirmed_covid_19)) %>%
    mutate(mean_wkly_resident_deaths = mean(single_snf$residents_weekly_covid_19_deaths)) %>%
    mutate(tot_resident_cases = max(single_snf$residents_total_confirmed_covid_19)) %>%
    mutate(tot_resident_deaths = max(single_snf$residents_total_covid_19_deaths)) %>%
    mutate(mean_wkly_staff_cases = mean(single_snf$staff_weekly_confirmed_covid_19)) %>%
    mutate(tot_staff_cases = max(single_snf$staff_total_confirmed_covid_19))
  
results_covid <-  rbind(results_covid, row)
  
}

save(results_covid, file = "results_covid.Rda")


# Merge in county pharmacy data ----

snfs_covidpharm <- merge (results_covid, county, by="fips_code")

# clean column names

snfs_covidpharm <- snfs_covidpharm %>%
  select(-week_ending, -state.y, -state_code, -county_code, -county.y, -state_name.y, -state_name.x, -week_ending_2) %>%
  rename(state = state.x) %>%
  rename(county = county.x)
  
# save df 

save(snfs_covidpharm, file = "snfs_covidpharm.Rda")


# Merge in LTCFOCUS data ----
# ****NEEDS FIXING; OMIT CODE CHUNK FOR NOW


fac_merge <- merge(covid_snfs, ltcfocus_fac, by = "federal_provider_number", all.x = TRUE)

# inspect facilities that didn't match; try other provider number from LTCFocus data
fac_matched <- fac_merge %>% filter(!is.na(pctwhite_mds3))
fac_nomatch <- fac_merge %>% filter(is.na(pctwhite_mds3)) 

#%>%select(-PROV0475, -PROV0475, -state.y, -county.y, -totbeds, -nresid, -paymcaid, -pctwhite_mds3)

alternate_prov_id <- ltcfocus_fac %>%
  select(-federal_provider_number) %>%
  rename(federal_provider_number = PROV1680)

fac_merge2 <- merge(fac_nomatch, alternate_prov_id, by = "federal_provider_number", all.x=TRUE)

# clean column headings for merge
fac_matched <- fac_matched %>%
  rename(state = state.y) %>%
  rename(county = county.y)

# bind data frames
facilities <- rbind(fac_matched, fac_merge2)

# clean column headings
facilities <- facilities %>%
  select(-state.x, -county.x, -PROV1680, -PROV0475) 
  
# generate exclusion variable for missing LTCF data

facilities <- facilities %>%
  mutate(reason_excluded = ifelse(is.na(pctwhite_mds3), "ltcfocus_dnm", reason_excluded)) %>%
  mutate(include = ifelse(is.na(pctwhite_mds3), 0, include))

sum(is.na(facilities$pctwhite_mds3))

# Merge in county pharmacy data 
facilities <- merge (facilities, county, by="fips_code")

# clean column names
facilities <- facilities %>%
  select(-state_code, -state_name, -county_code) %>%
  rename(state = state.y) %>%
  rename(county = county.y)

facilities <- facilities %>%
  select(-state.x, -county.x) 

facilities <- facilities %>%
  relocate(state, .after=provider_name) %>%
  relocate(county, .after=state) %>%
  relocate(cvs_n, .after=county) %>%
  relocate(wal_n, .after=cvs_n) %>%
  relocate(cvswal_any, .after=wal_n)

# Save df 
save(facilities, file = "facilities.Rda")

rm(alternate_prov_id, fac_matched, fac_merge, fac_merge2, fac_nomatch, provider_ids, fac_nov2020)









