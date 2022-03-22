############ MAPPING NURSING HOMES AND PHARMACIES 
############ Data preparation - extracting pharmacy info from Hayes database
############ Alexandra Ruth
############ July 2021

## Prepare working environment ----

# install packages
library(tidyverse)
library(janitor)
library(tidycensus)

## Read in Hayes Directories data ----

hayes <- read_csv("hayes_2021.csv")

## Clean Hayes Directories data ----

# keep variables of interest

hayes <- hayes %>%
  select(-PHONE, -FAX, -CHQ, -STORENUM, -BRANCH, -OTHER, - POP, -FULLSTATE, -`NEW STORE`)

# clean up county names for fips codes merge

hayes <- hayes %>%
  mutate(COUNTY = gsub("Saint", "St.", COUNTY)) %>%
  mutate(COUNTY = gsub("Miami Dade", "Miami-Dade", COUNTY)) %>% 
  mutate(COUNTY = gsub("De Kalb", "DeKalb", COUNTY)) %>% 
  mutate(COUNTY = gsub("Dekalb", "DeKalb", COUNTY)) %>% 
  mutate(COUNTY = gsub("Cullmann", "Cullman", COUNTY)) %>% 
  mutate(COUNTY = gsub("Vadez-Cordova", "Valdez-Cordova", COUNTY)) %>% 
  mutate(COUNTY = gsub("Prince Wales", "Prince of Wales-Hyder", COUNTY)) %>% 
  mutate(COUNTY = gsub("Fairbanks", "Fairbanks North Star", COUNTY)) %>% 
  mutate(COUNTY = ifelse(STATE=="AK" & COUNTY=="Juneau", gsub("Juneau", "Juneau City", COUNTY), COUNTY)) %>%
  mutate(COUNTY = gsub("Ketchikan", "Ketchikan Gateway", COUNTY)) %>% 
  mutate(COUNTY = gsub("Matanuska", "Matanuska-Susitna", COUNTY)) %>% 
  mutate(COUNTY = gsub("Wrangell-Petersburg", "Wrangell City", COUNTY)) %>% 
  mutate(COUNTY = gsub("Mojave", "Mohave", COUNTY)) %>% 
  mutate(COUNTY = gsub("Quachita", "Ouachita", COUNTY)) %>%
  mutate(COUNTY = gsub("Du Page", "DuPage", COUNTY)) %>%
  mutate(COUNTY = gsub("Dekalb", "DeKalb", COUNTY)) %>% 
  mutate(COUNTY = gsub("Sitka", "Sitka City", COUNTY)) %>% 
  mutate(COUNTY = gsub("Desoto", "DeSoto", COUNTY)) %>% 
  mutate(COUNTY = ifelse(STATE=="FL" & COUNTY=="De Soto", gsub("De Soto", "DeSoto", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="MS" & COUNTY=="De Soto", gsub("De Soto", "DeSoto", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="LA" & COUNTY=="DeSoto", gsub("DeSoto", "De Soto", COUNTY), COUNTY)) %>%
  mutate(COUNTY = gsub("Doughterty", "Dougherty", COUNTY)) %>% 
  mutate(COUNTY = gsub("Mcintosh", "McIntosh", COUNTY)) %>% 
  mutate(COUNTY = gsub("Putham", "Putnam", COUNTY)) %>% 
  mutate(COUNTY = gsub("Pekin", "Tazewell", COUNTY)) %>% 
  mutate(COUNTY = gsub("Mchenry", "McHenry", COUNTY)) %>% 
  mutate(COUNTY = ifelse(STATE=="IL" & COUNTY=="La Salle", gsub("La Salle", "LaSalle", COUNTY), COUNTY)) %>%
  mutate(COUNTY = gsub("Moultre", "Moultrie", COUNTY)) %>% 
  mutate(COUNTY = gsub("Delavan", "Tazewell", COUNTY)) %>% 
  mutate(COUNTY = gsub("Greenfield", "Hancock", COUNTY)) %>% 
  mutate(COUNTY = gsub("Vanderbugh", "Vanderburgh", COUNTY)) %>% 
  mutate(COUNTY = gsub("Lagrange", "LaGrange", COUNTY)) %>% 
  mutate(COUNTY = gsub("La Porte", "LaPorte", COUNTY)) %>%
  mutate(COUNTY = gsub("Prince Georges", "Prince George's", COUNTY))  %>%
  mutate(COUNTY = ifelse(STATE=="AR" & COUNTY=="Hot Springs", gsub("Hot Springs", "Hot Spring", COUNTY), COUNTY)) %>%
  mutate(COUNTY = gsub("Lewis & Clark", "Lewis and Clark", COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="NH" & COUNTY=="Hillsboro", gsub("Hillsboro", "Hillsborough", COUNTY), COUNTY)) %>%
  mutate(COUNTY = gsub("St. Marys", "St. Mary's", COUNTY)) %>%
  mutate(COUNTY = gsub("Alexandria", "Alexandria city", COUNTY)) %>%
  mutate(COUNTY = gsub("Danville", "Danville city", COUNTY)) %>%
  mutate(COUNTY = gsub("Charlottesville", "Charlottesville city", COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Bristol", gsub("Bristol", "Bristol city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Buena Vista", gsub("Buena Vista", "Buena Vista city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Chesapeake", gsub("Chesapeake", "Chesapeake city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Colonial Heights", gsub("Colonial Heights", "Colonial Heights city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Covington", gsub("Covington", "Covington city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Covington ", gsub("Covington ", "Covington city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Covington City", gsub("Covington City", "Covington city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Danville", gsub("Danville", "Danville city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Falls Church", gsub("Falls Church", "Falls Church city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Galax", gsub("Galax", "Galax city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Hampton", gsub("Hampton", "Hampton city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Harrisonburg", gsub("Harrisonburg", "Harrisonburg city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Newport News", gsub("Newport News", "Newport News city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Hopewell", gsub("Hopewell", "Hopewell city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Hopewell City", gsub("Hopewell City", "Hopewell city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Emporia", gsub("Emporia", "Emporia city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Lexington", gsub("Lexington", "Lexington city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Lexington City", gsub("Lexington City", "Lexington city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Lynchburg", gsub("Lynchburg", "Lynchburg city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Martinsville", gsub("Martinsville", "Martinsville city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Norfolk", gsub("Norfolk", "Norfolk city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Norton", gsub("Norton", "Norton city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Petersburg", gsub("Petersburg", "Petersburg city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Portsmouth", gsub("Portsmouth", "Portsmouth city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Radford", gsub("Radford", "Radford city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Salem", gsub("Salem", "Salem city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Staunton", gsub("Staunton", "Staunton city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Suffolk", gsub("Suffolk", "Suffolk city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Virginia Beach", gsub("Virginia Beach", "Virginia Beach city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Waynesboro", gsub("Waynesboro", "Waynesboro city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="VA" & COUNTY=="Winchester", gsub("Winchester", "Winchester city", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="WA" & COUNTY=="Island San Juan", gsub("Island San Juan", "San Juan", COUNTY), COUNTY)) %>%
  mutate(COUNTY = gsub("Fond Du Lac", "Fond du Lac", COUNTY)) %>%
  mutate(COUNTY = gsub("Obrien", "O'Brien", COUNTY)) %>%
  mutate(COUNTY = gsub("Mcpherson", "McPherson", COUNTY)) %>%
  mutate(COUNTY = gsub("Mcpherson", "McPherson", COUNTY)) %>%
  mutate(COUNTY = gsub("Muhlenburg", "Muhlenberg", COUNTY)) %>%
  mutate(COUNTY = gsub("Garrad", "Garrard", COUNTY)) %>%
  mutate(COUNTY = gsub("Rock Castle", "Rockcastle", COUNTY)) %>%
  mutate(COUNTY = gsub("Mccracken", "McCracken", COUNTY)) %>%
  mutate(COUNTY = gsub("Mccreary", "McCreary", COUNTY)) %>%
  mutate(COUNTY = gsub("Clakamas", "Clackamas", COUNTY)) %>%
  mutate(COUNTY = gsub("Mcnairy", "McNairy", COUNTY)) %>%
  mutate(COUNTY = gsub("De Witt", "DeWitt", COUNTY)) %>%
  mutate(COUNTY = gsub("Shakelford", "Shackelford", COUNTY)) %>%
  mutate(COUNTY = gsub("Mclennan", "McLennan", COUNTY)) %>%
  mutate(COUNTY = gsub("Japser", "Jasper", COUNTY)) %>%
  mutate(COUNTY = gsub("Mcdowell", "McDowell", COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="LA" & COUNTY=="Vermillion", gsub("Vermillion", "Vermilion", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="TN" & COUNTY=="Humphries", gsub("Humphries", "Humphreys", COUNTY), COUNTY)) %>%
  mutate(COUNTY = gsub("Hiawatha", "Linn", COUNTY)) %>%
  mutate(COUNTY = gsub("Chicksaw", "Chickasaw", COUNTY)) %>%
  mutate(COUNTY = gsub("Powan", "Rowan", COUNTY)) %>%
  mutate(COUNTY = gsub("Queen Annes", "Queen Anne's", COUNTY)) %>%
  mutate(COUNTY = gsub("Lac Qui Parle", "Lac qui Parle", COUNTY)) %>%
  mutate(COUNTY = gsub("Cotton Wood", "Cottonwood", COUNTY)) %>%
  mutate(COUNTY = gsub("Kittsom", "Kittson", COUNTY)) %>%
  mutate(COUNTY = gsub("St.e Genevieve", "Ste. Genevieve", COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="MO" & COUNTY=="Francois", gsub("Francois", "St. Francois", COUNTY), COUNTY)) %>%
  mutate(COUNTY = gsub("Humbodlt", "Humboldt", COUNTY)) %>%
  mutate(COUNTY = gsub("St. John The Baptist", "St. John the Baptist", COUNTY)) %>%
  mutate(COUNTY = gsub("Putnom", "Putnam", COUNTY)) %>%
  mutate(COUNTY = gsub("Putnum", "Putnam", COUNTY)) %>%
  mutate(COUNTY = gsub("Luniata", "Juniata", COUNTY)) %>%
  mutate(COUNTY = gsub("La Moure", "LaMoure", COUNTY)) %>%
  mutate(COUNTY = gsub("Lamoure", "LaMoure", COUNTY)) %>%
  mutate(COUNTY = gsub("McPhearson", "McPherson", COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="TX" & COUNTY=="Patricio", gsub("Patricio", "San Patricio", COUNTY), COUNTY)) %>%
  mutate(COUNTY = gsub("South Boston", "Halifax", COUNTY)) %>%
  mutate(COUNTY = gsub("Fredrick", "Frederick", COUNTY)) %>%
  mutate(COUNTY = gsub("Pocohontas", "Pocahontas", COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="WV" & COUNTY=="Harris", gsub("Harris", "Harrison", COUNTY), COUNTY)) %>%
  mutate(COUNTY = gsub("Windom", "Windham", COUNTY)) %>%
  mutate(COUNTY = gsub("Redwillow", "Red Willow", COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="MI" & COUNTY=="Montgomery", gsub("Montgomery", "Montmorency", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="OH" & COUNTY=="Starke", gsub("Starke", "Stark", COUNTY), COUNTY)) %>%
  mutate(COUNTY = gsub("McClean", "McLean", COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="SD" & COUNTY=="Davidson", gsub("Davidson", "Davison", COUNTY), COUNTY)) %>%
  mutate(COUNTY = ifelse(STATE=="TX" & COUNTY=="Wallace", gsub("Wallace", "Willacy", COUNTY), COUNTY)) %>%
  mutate(COUNTY = gsub("Neuces", "Nueces", COUNTY)) %>%
  mutate(COUNTY = gsub("Clifton Forge", "Alleghany", COUNTY)) %>%
  mutate(COUNTY = gsub("Schuyer", "Schuyler", COUNTY)) %>%
  mutate(COUNTY= gsub("Renssaleaer", "Rensselaer", COUNTY)) %>%
  mutate(COUNTY = gsub("Stueben", "Steuben", COUNTY)) %>%
  mutate(COUNTY = gsub("Merrmack", "Merrimack", COUNTY)) 

  
# Load and prepare county fips codes for merge

data(fips_codes) # get county fips codes

fips_codes <- fips_codes %>% # generate full fips code
  mutate(fips_code = paste(state_code, county_code)) %>%
  mutate(fips_code = gsub(" ", "", fips_code))

fips_codes <- fips_codes %>%  # prep county strings for merge
  mutate(county2 = gsub(" County", "", county)) %>%
  mutate(county2 = gsub(" and Borough", "", county2)) %>%
  mutate(county2 = gsub(" Borough", "", county2)) %>%
  mutate(county2 = gsub(" Census Area", "", county2)) %>%
  mutate(county2 = gsub(" Parish", "", county2)) %>%
  mutate(county2 = gsub(" Municipality", "", county2)) 
  
 
hayes <- hayes %>%                         
  rename(state = STATE, county2 = COUNTY) # prep hayes variable names for merge

hayes <- left_join(hayes, fips_codes, by = c("county2", "state"), keep = TRUE) # merges county fips codes into hayes

## Checking for unmatched counties 

sum(is.na(hayes$fips_code)) # check how many pharmacies didn't match up with a fips code 

dnm_hayes <- hayes %>% # selects the unmatched rows for inspection
  filter(is.na(fips_code)) 


# clean up variable names

hayes_cty <- hayes %>%
  rename(county_short = county2.x) %>%
  rename(state = state.x) %>%
  rename(name = NAME) %>%
  rename(address = ADDRESS) %>%
  rename(mail = MAIL) %>%
  rename(city = CITY) %>%
  rename(zip = ZIP) %>%
  rename(chain = CHAIN) %>%
  select(-state.y, -state_code, -county_code, -county2.y) %>%
  relocate(name, state, county_short, fips_code)
  

# save

save(hayes_cty, file = "hayes_cty.Rda")





######################################################################### 
## Get CVS and Walgreens; keep list of ONLY all CVS & Walgreens locations ----
 
cvs <- hayes_cty %>%
  filter(str_detect(name, "CVS")) %>%
  mutate(cvs = 1) %>%
  mutate(walgreen = 0)

walgreen <- hayes_cty %>%
  filter(str_detect(name, "Walgreen")) %>%
  mutate(cvs = 0) %>%
  mutate(walgreen = 1)
 
cvs_wal <- rbind(cvs, walgreen)
save(cvs_wal, file = "cvs_wal.Rda")


# get counts of CVS and Walgreen facilities by state

cvs_count <- cvs %>% 
  count(state, sort=TRUE) %>%
  rename(cvs_num = n)
  
wal_count <- walgreen %>% 
  count(state, sort=TRUE) %>%
  rename(wal_num = n)

cvswal_count <- merge(cvs_count, wal_count, by='state', all=T)

cvswal_count[is.na(cvswal_count)] <- 0 #replaces the "NA" in ND with a zero



# save 

save(cvswal_count, file = "cvswal_count.Rda")

