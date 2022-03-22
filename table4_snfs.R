#################################
# TABLE 4: 
#This is a script to generate SNF-level information for table 4.
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

### Prepare: SNF data ----



### Prepare LTCFocus data ----

ltcfocus_fac <- ltcfocus_fac %>%
  select(accpt_id, PROV1680, PROV0475, state, county, totbeds, nresid, paymcaid, pctwhite_mds3) %>%
  rename(provider_id = accpt_id) 
    



### Merge in: LTCFocus data ----

### Merge in: county pharmacy data ----

### Clean up & save dataframe ----


############################
### GENERATE TABLE 4 ####### ----
############################


