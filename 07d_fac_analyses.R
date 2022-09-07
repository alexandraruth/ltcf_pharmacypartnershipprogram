##### This is a script that explores and analyzes SNF-level data. 

# Prepare environment ----

# load packages
library(tidyverse)
library(janitor)
library(arsenal)
library(knitr)

# load data
load("snfs_fullsample.Rda")

# explore # prepare data for analyses ----

## flag & exclude rows with missing bed counts;
## generate additional distance variable & Covid variables

snfs <- snfs_nn_all %>%
  mutate(reason_excluded = ifelse(is.na(number_of_all_beds), "missing_bedct", reason_excluded)) %>%
  mutate(include = ifelse(is.na(reason_excluded), 1, 0)) %>%
  mutate(pharm_within5 = ifelse(dist_miles<=5, 1, 0)) %>%
  mutate(resident_tot_cases_per100beds = round(100*(residents_total_confirmed_covid_19/number_of_all_beds), digits = 2)) %>%
  mutate(staff_tot_cases_per100beds = round(100*(staff_total_confirmed_covid_19/number_of_all_beds), digits = 2))

# get analytic sample

include <- snfs %>%
  filter(include==1) 

# get tables for analytic sample

tab1 <- tableby(~ pharm_within75 + region + metro + resident_tot_cases_per100beds  + staff_tot_cases_per100beds,
               data = include,
               control = tableby.control(digits = 2, digits.pct = 2))

summary(tab1, text=TRUE)



tab2 <- tableby(pharm_within5 ~ region + metro + resident_tot_cases_per100beds + staff_tot_cases_per100beds,
                data = include,
                control = tableby.control(digits = 2, digits.pct = 2))

summary(tab2, text=TRUE)

# get inclusion table

tab_inclusion <- tableby( reason_excluded ~ region + metro + number_of_all_beds + nov_cases_avg_per_100k  + pharm_within75 + pharm_within5, 
                          data = snfs,
                          control = tableby.control(digits = 2, digits.pct = 2))

summary(tab_inclusion, text = TRUE)


# Run individual Z-tests by region

midwest <- prop.test(x = c(sum(include$region == "Midwest" & include$pharm_within75==1), sum(include$region == "Midwest" & include$pharm_within75==0)), n = c(sum(include$pharm_within75==1), sum(include$pharm_within75==0)))
midwest$p.value

northeast <- prop.test(x = c(sum(include$region == "Northeast" & include$pharm_within75==1), sum(include$region == "Northeast" & include$pharm_within75==0)), n = c(sum(include$pharm_within75==1), sum(include$pharm_within75==0)))
northeast$p.value

south <- prop.test(x = c(sum(include$region == "South" & include$pharm_within75==1), sum(include$region == "South" & include$pharm_within75==0)), n = c(sum(include$pharm_within75==1), sum(include$pharm_within75==0)))
south$p.value

west <- prop.test(x = c(sum(include$region == "West" & include$pharm_within75==1), sum(include$region == "West" & include$pharm_within75==0)), n = c(sum(include$pharm_within75==1), sum(include$pharm_within75==0)))
west$p.value

metro <- prop.test(x = c(sum(include$metro == 1 & include$pharm_within75==1), sum(include$metro == 1 & include$pharm_within75==0)), n = c(sum(include$pharm_within75==1), sum(include$pharm_within75==0)))
metro
metro$p.value

nonmetro <- prop.test(x = c(sum(include$metro == 0 & include$pharm_within75==1), sum(include$metro == 0 & include$pharm_within75==0)), n = c(sum(include$pharm_within75==1), sum(include$pharm_within75==0)))
nonmetro
nonmetro$p.value

# get state counts for out-of-range

st_75 <- exclude_75 %>% group_by(state) %>% summarise(count = n())

## some plots ----


a <- ggplot(include, aes(x = dist_miles))
p <- a + geom_density(aes(y = ..count..), fill = "lightgray") 
p

# get plot with and without upper quartile

include_2.5 <- include %>%
  filter(dist_miles < 2.5)

a <- ggplot(include_2.5, aes(x = dist_miles))
p <- a + geom_density(aes(y = ..count..), fill = "lightgray") 
p

exclude_2.5 <- include %>%
  filter(dist_miles >=2.5)
a <- ggplot(exclude_2.5, aes(x = dist_miles))
p <- a + geom_density(aes(y = ..count..), fill = "lightgray") 
p

# plots for 75 miles

include_75 <- include %>%
  filter(dist_miles < 75)

a <- ggplot(include_75, aes(x = dist_miles))
p <- a + geom_density(aes(y = ..count..), fill = "lightgray") 
p


exclude_75 <- include %>%
  filter(dist_miles >=75)

a <- ggplot(exclude_75, aes(x = dist_miles))
p <- a + geom_density(aes(y = ..count..), fill = "lightgray") 
p
  
# plots for 75 miles

include_100 <- include %>%
  filter(dist_miles < 100)

a <- ggplot(include_100, aes(x = dist_miles))
p <- a + geom_density(aes(y = ..count..), fill = "lightgray") 
p


exclude_100 <- include %>%
  filter(dist_miles >=100)

a <- ggplot(exclude_100, aes(x = dist_miles))
p <- a + geom_density(aes(y = ..count..), fill = "lightgray") 
p

# plot bed counts in out-of-range

a <- ggplot(data = include_75, aes(x=number_of_all_beds))
p <- a + geom_density(aes(y=..count..), fill = "lightgray")
p


a <- ggplot(data = exclude_75, aes(x=number_of_all_beds))
p <- a + geom_density(aes(y=..count..), fill = "lightgray")
p


# plot res cases in out-of-range

a <- ggplot(data = include_75, aes(x=resident_tot_cases_per100beds))
p <- a + geom_density(aes(y=..count..), fill = "lightgray")
p


a <- ggplot(data = exclude_75, aes(x=resident_tot_cases_per100beds))
p <- a + geom_density(aes(y=..count..), fill = "lightgray")
p

# inclusion/exclusion z tests

incl_75 <- prop.test(x = c(14816, 102), n = c(15227, 105))
incl_75 

dns_75 <- prop.test(x = c(165, 1), n = c(15227, 105))
dns_75 

missingwk_75 <- prop.test(x = c(57, 0), n = c(15227, 105))
missingwk_75


