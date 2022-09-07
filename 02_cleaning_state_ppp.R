##### This is a cleaning script that prepares the state-level federal pharmacy partnership enrollment dataframe for table 1 analyses. 

# Inputs:
# - "states_ppp.csv" (SNF enrollment counts pulled from CVS and Walgreens pdfs)
# - "cvswal_count.Rda" (df with state-level counts of CVS and Walgreens locations)

# Outputs: 
# - "states_ppp_enroll.Rda" (df with state-level info needed for table 1)

# packages
library(tidyverse)


# load data and prep
states_ppp = read.csv(file = "states_ppp.csv")

states_ppp <- states_ppp %>%
  mutate(partnered_cvs = as.numeric(partnered_cvs)) %>%
  mutate(partnered_walgreen = as.numeric(partnered_walgreen)) %>%
  mutate(snfs_cvs_pct = round(partnered_cvs/snfs_num, 3)) %>%
  mutate(snfs_walgreen_pct = round(partnered_walgreen/snfs_num, 3)) %>%
  mutate(partnered_pct = snfs_walgreen_pct + snfs_cvs_pct)



## Explore associations between pharmacy counts

load("cvswal_count.Rda")


states_ppp <- merge(states_ppp, cvswal_count, by="state")

states_ppp <- states_ppp %>%
  mutate(cvs_snf_ratio = cvs_num / snfs_num) %>%
  mutate(wal_snf_ratio = wal_num / snfs_num)


# save updated dataframe 

states_ppp_enroll <- states_ppp
save(states_ppp_enroll, file = "states_ppp_enroll.Rda" )


