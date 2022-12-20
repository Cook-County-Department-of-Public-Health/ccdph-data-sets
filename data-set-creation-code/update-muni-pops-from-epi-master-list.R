
library(tidyverse)
library(readxl)


#####ROUND 1######


#load in current files from Github repo
current_munis <- read_csv("https://github.com/Cook-County-Department-of-Public-Health/ccdph-data-sets/blob/main/Municipality%20Populations.csv?raw=TRUE") 

current_districts <- read_csv("https://github.com/Cook-County-Department-of-Public-Health/ccdph-data-sets/blob/main/Districts.csv?raw=TRUE") %>%
  mutate(Name = recode(Name, Mccook = "McCook"))

#add districts to the muni file
muni_districts <- full_join(current_munis, current_districts, by = c("Municipality" = "Name")) %>% select(-Municipality.y)

#load in 2010 muni pops from most recent epi master file (verified by Nhan)
epi_pops <- read_excel('S:/Enhanced Surveillance/CookCountyPopulationMaster_09_25_20_Nhan.xlsx', sheet = 1, range = "A1:K138")

#join epi file to github file, create new flags, complete district field
munis <- full_join(muni_districts, epi_pops, by = c("Municipality" = "NAME")) %>%
  mutate(check_pop = Population2010 - PopInCook2010Census,  #identify any discrepant population counts
         noncook_pop = TotPop - PopInCook2010Census,  #identify partial towns
         noncook_percent = ((noncook_pop / TotPop) * 100), #characterize extent of population in/out of cook
         cook_percent = 100 - noncook_percent,
         Partial = ifelse(cook_percent != 100, T, F), #can adjust percentage later if data standards change (e.g. > 99)
         ExcludeFromAnalysis = ifelse(PopInCook2010Census < 200 & Partial == T, T, F)) %>% #cross-border towns with minimal population in cook excluded from tables/maps
  mutate(District = case_when(is.na(District) & CCDPH_District == "NO" ~ "North",
                              is.na(District) & CCDPH_District == "WE" ~ "West",
                              is.na(District) & CCDPH_District == "SW" ~ "Southwest",
                              is.na(District) & CCDPH_District == "SO" ~ "South",
                              is.na(District) & CCDPH_District == "ZZ" ~ "OOJ",
                              TRUE ~ District)) %>%
  mutate(Population2010 = PopInCook2010Census) #use epi group's populations, not CD -- consensus from Inter-Epi group

#select fields for final file to be saved in Github
revised_munis <- munis %>% select(Municipality, Population2010, District, Partial, ExcludeFromAnalysis) %>% arrange(Municipality)

#re-save github repo file
write_csv(revised_munis, "S:/Enhanced Surveillance/R Package/ccdph-data-sets/Municipality Populations.csv")  
  
  
  
#####ROUND 2###### 

#load in current files from Github repo
current_munis <- read_csv("https://github.com/Cook-County-Department-of-Public-Health/ccdph-data-sets/blob/main/Municipality%20Populations.csv?raw=TRUE") 

#load in 2010 muni pops from most recent epi master file (verified by Nhan)
epi_pops <- read_excel('C:/Users/Kelley/Documents/CookCountyPopulationMaster_09_25_20_Nhan.xlsx', sheet = 1, range = "A1:K138") %>%
  mutate(FIPS_Place = as.character(FIPS_Place),
         CensusPlaceCode = ifelse(nchar(FIPS_Place) == 4, paste0(0, FIPS_Place), FIPS_Place)) %>%
  select(NAME, CensusPlaceCode)

revised_munis <- full_join(current_munis, epi_pops, by = c("Municipality" = "NAME")) 

#re-save github repo file
write_csv(revised_munis, "Municipality Populations.csv")  


#####ROUND 3######

#per 2021 pull of GIS department master muni shapefile, Highland Park and Riverwoods have tiny bits that extend into Cook -- adding to entries to master muni file

#load in current file and add towns
current_munis <- read_csv("Municipality Populations.csv") %>%
  add_row(Municipality = "Highland Park", Population2010 = 0, District = "North", Partial = T, ExcludeFromAnalysis = T, CensusPlaceCode = 34722) %>%
  add_row(Municipality = "Riverwoods", Population2010 = 0, District = "North", Partial = T, ExcludeFromAnalysis = T, CensusPlaceCode = 64538) %>%
  arrange(Municipality)

#re-save github repo file
write_csv(current_munis, "Municipality Populations.csv")  

#####ROUND 4######

#current file still has 200 population threshold for exlcuding from analysis, this has been increased to 500

#load in current file and update exclude flag
current_munis <- read_csv("Municipality Populations.csv") %>%
  mutate(ExcludeFromAnalysis = ifelse(Population2010 < 500 & Partial == T, T, F),
         CensusPlaceCode = ifelse(nchar(CensusPlaceCode) == 4, paste0(0, CensusPlaceCode), CensusPlaceCode))

#re-save github repo file
write_csv(current_munis, "Municipality Populations.csv")  


#####ROUND 5######

#Comparing mappings of muni to district from epi master file to website

#import muni mappings from CCDPH website
web_district <- read_csv("C:/Users/Kelley/Desktop/districts_from_ccdph_web.csv") %>%
  rename(district_web = District) %>%
  mutate(City = gsub("Hazelcrest", "Hazel Crest", City))

#load in current file and compare
current_munis <- read_csv("Municipality Populations.csv") %>%
  full_join(web_district, by = c("Municipality" = "City")) %>%
  mutate(district_match = District == district_web)

#Resolved without changing the file - after internal discussion, determined website should be changed, not file


