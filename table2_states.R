
##### This is a script that calculates state-level correlations between SNF enrollment in the federal 
##### pharmacy partnership program and other state characteristics. The output in the manuscript 
##### associated with this R script is TABLE 2.


### Load packages ----
library(tidyverse)
library(ggpubr)

### Load data ----

load("states_ppp_enroll.Rda")
load("hayes_cty.Rda")
load("fac_nov2020.Rda")
load("cvswal_count.Rda")
load("ltcfocus_state.Rda")


### Construct variable: # SNFs in state with >100 beds ----

# condense to just one week 
fac_nov2020 <- fac_nov2020 %>%
  subset(week_ending == "2020-11-08" ) %>% # subsets for first week of Nov 2020
  filter(state != "PR") %>% # removes places not of interest
  filter(state != "GU") %>%
  filter(state != "AS") %>%
  filter(state != "MP") %>%
  filter(state != "VI") %>%
  filter(state != "UM") 

nrow(fac_nov2020) # count rows
fac_nov2020$`Federal Provider Number`[duplicated(fac_nov2020$`Federal Provider Number`)] # check for duplicate SNFs; none found

# binary variable for bed counts over 100
fac_nov2020 <- fac_nov2020 %>%
  mutate(beds_over100 = ifelse(`Number of All Beds` > 100, 1, 0))

# binary variable for bed counts over 50
fac_nov2020 <- fac_nov2020 %>%
  mutate(beds_over50 = ifelse(`Number of All Beds` > 50, 1, 0))

sum(is.na(fac_nov2020$`Number of All Beds`)) # checks how many missing bed counts there are

# create new df with # facilities that have >100 or > 50 beds by state

state_bedsover100 <- fac_nov2020 %>% group_by(state) %>% summarise(sum_beds_over100=sum(beds_over100, na.rm=TRUE))
state_bedsover50 <- fac_nov2020 %>% group_by(state) %>% summarise(sum_beds_over50=sum(beds_over50, na.rm=TRUE))



# merge new bed count variables into main df

states_ppp_enroll_table2 <- merge(states_ppp_enroll, state_bedsover100, by= "state")
states_ppp_enroll_table2 <- merge(states_ppp_enroll_table2, state_bedsover50, by= "state")

### Construct variable: Ratio of pharmacy partners to SNFs ----

states_ppp_enroll_table2 <- states_ppp_enroll_table2 %>%
  mutate(ratio_pharmtosnfs = ((cvs_num + wal_num)/ snfs_num))

### Construct variable: Proportion of pharmacies in state that are CVS/Walgreens ----

state_pharm <- hayes_cty %>%  # get total pharm count by state
  group_by(state) %>%
  count(pharm_count = state)

states_ppp_enroll_table2 <- merge(states_ppp_enroll_table2, state_pharm, by= "state", all=T)
states_ppp_enroll_table2 <- states_ppp_enroll_table2 %>%
  mutate(proppharm_cvswal = ((cvs_num + wal_num) / n))

### Merge in LTCFocus variables of interest ----

ltcfocus_state <- ltcfocus_state %>%
  select(state, totbeds_sta, paymcaid_sta, pctblack_mds3_sta, pcthisp_mds3_sta, pctwhite_mds3_sta, pctfluvax_sta)
  
states_ppp_enroll_table2 <- merge(states_ppp_enroll_table2, ltcfocus_state, by= "state", all=T)

# need to add bed counts for DC and AK
# DC bed count = 2590
# AK bed count = 827 
# (source: The Nursing Home Site)

states_ppp_enroll_table2 <- states_ppp_enroll_table2 %>%
  mutate(totbeds_sta = ifelse(state=="AK",827, totbeds_sta)) %>%
  mutate(totbeds_sta = ifelse(state=="DC",2590, totbeds_sta)) 
  

### Keep only variables and dfs of interest ----


states_ppp_enroll_table2 <- states_ppp_enroll_table2 %>%
  select(-pharm_count) %>%
  rename(pharm_count = n)
  
# remove dfs not needed for correlations

rm(state_pharm, states_ppp_enroll, cvswal_count, hayes_cty, fac_nov2020, ltcfocus_state, state_bedsover100, state_bedsover50)
  
### Save new dataframe ----

save(states_ppp_enroll_table2, file = "states_ppp_enroll_table2.Rda")

### Plot correlations for analysis ----

# number of SNFs
ggscatter(states_ppp_enroll_table2, x = "snfs_num", y = "partnered_pct", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Number of SNFs in state", ylab = "% Facilities enrolled in program")

# total SNF beds in state
ggscatter(states_ppp_enroll_table2, x = "totbeds_sta", y = "partnered_pct", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Total number of SNF beds in state", ylab = "% Facilities enrolled in program")


# number of facilities with >100 beds
ggscatter(states_ppp_enroll_table2, x = "sum_beds_over100", y = "partnered_pct", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Number of SNFs in state with bed counts >100", ylab = "% Facilities enrolled in program")


# ratio of pharmacies to SNFs
ggscatter(states_ppp_enroll_table2, x = "ratio_pharmtosnfs", y= "partnered_pct", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Ratio of pharmacy partners to SNFs", ylab = "% Facilities enrolled in program")

# overall share of pharmacies that are CVS or Walgreens
ggscatter(states_ppp_enroll_table2, x = "proppharm_cvswal", y="partnered_pct", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Share of pharmacies in state that are CVS or Walgreens", ylab = "% Facilities enrolled in program")


# proportion paying w/Medicaid
ggscatter(states_ppp_enroll_table2, x = "paymcaid_sta", y = "partnered_pct", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Percent of SNF patients covered by Medicaid", ylab = "% Facilities enrolled in program")

# proportion Black residents on April 1 2020
ggscatter(states_ppp_enroll_table2, x = "pctblack_mds3_sta", y = "partnered_pct", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Percent of Black SNF patients on April 1", ylab = "% Facilities enrolled in program")

# proportion White residents on April 1 2020 
ggscatter(states_ppp_enroll_table2, x = "pctwhite_mds3_sta", y = "partnered_pct", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Percent of White SNF patients on April 1", ylab = "% Facilities enrolled in program")

# percent residents vaccinated against flu
ggscatter(states_ppp_enroll_table2, x = "pctfluvax_sta", y = "partnered_pct", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Percent of SNF patients who received flu vax as of April 1", ylab = "% Facilities enrolled in program")


##################

# Try Spearman's correlation test for monotonic relationships

fluvax_corr <- cor.test(x=states_ppp_enroll_table2$pctfluvax_sta, y=states_ppp_enroll_table2$partnered_pct, method = 'spearman')
fluvax_corr

snfsnum_corr <- cor.test(x=states_ppp_enroll_table2$snfs_num, y=states_ppp_enroll_table2$partnered_pct, method = 'spearman')
snfsnum_corr

totbeds_corr <- cor.test(x=states_ppp_enroll_table2$totbeds_sta, y=states_ppp_enroll_table2$partnered_pct, method = 'spearman')
totbeds_corr


# Building a linear regression model

model <- lm(formula = partnered_pct ~ snfs_num + sum_beds_over100 + ratio_pharmtosnfs + proppharm_cvswal + totbeds_sta + paymcaid_sta, data = states_ppp_enroll_table2)

summary(model)
