#Script for warehousing misc census pulls - most can be accomplished with censusapi wrapper
#Special cases for partials
#rishi.kowalski@cookcountyhealth.org, 3/14/22


#packages
library(censusapi)
library(tidyverse)
library(dplyr)
library(stringr)
library(keyring)


### INCOME BY ZCTA ###
#read in zip/zcta data
ccdph_zip_dat <- read.csv(paste0(key_get("rk-code-fpath"), "ccdph-data-sets/cook-county-zip-codes.csv"), stringsAsFactors = F)
ccdph_zctas <- unique(ccdph_zip_dat$zcta)

#use b19013, tried s1903_c02 but results didn't make a ton of sense
metadata_group_b19013 <- listCensusMetadata(name = "acs/acs5/", vintage = 2019, group = "B19013", type = "variables")

ccdph_B19013_5yr2019 <- getCensus(name = "acs/acs5/",
                                  vars = c("NAME", "group(B19013)"),
                                  region = "zip code tabulation area:*",
                                  regionin = "state:17",
                                  key = key_get("census_key")) %>%
                        filter(zip_code_tabulation_area %in% ccdph_zctas) %>%
                        select(zip_code_tabulation_area, B19013_001E, B19013_001M) %>%
                        mutate(B19013_001E = ifelse(B19013_001E < -10, NA, B19013_001E),
                               B19013_001M = ifelse(B19013_001M < -10, NA, B19013_001M),
                               zcta = zip_code_tabulation_area) %>%
                        select(-zip_code_tabulation_area) %>%
                        pivot_longer(cols = c(B19013_001E, B19013_001M), names_to = "variable", values_to = "value")

write.csv(ccdph_B19013_5yr2019,
          paste0(key_get("rk-code-fpath"), "ccdph-data-sets/acs-5yr-med-house-income-by-zcta.csv"), row.names = F)


### POVERTY LEVEL BY TRACT ###
pov_group <- "B17001" #variable = B17001_002E
pop_group <- "B01003"
year = "2021"

#call data 
# pov_metadata <- listCensusMetadata(
#                   name = "acs/acs5/",
#                   vintage = 2020,
#                   type = "groups",
#                   group = "B17001") %>% filter(name %in% c(pov_group, pop_group))

pov_tract <- getCensus(name = "acs/acs5",
               vintage = year,
               vars = c("GEO_ID", "NAME", "B17001_002E"),
               region = "tract:*", # tracts
               regionin="state:17+county:031", # places, counties, not msas
               key = key_get("census-key"))

pop_tract <- getCensus(name = "acs/acs5",
               vintage = year,
               vars = c("GEO_ID", "NAME", "B01003_001E"),
               region = "tract:*", # tracts
               regionin="state:17+county:031", # places, counties, not msas
               key = key_get("census-key"))

#join data
pov_tract <- left_join(pov_tract, pop_tract %>% select(GEO_ID, B01003_001E), by = "GEO_ID") %>%
              select(-state, -county) 

#apply data transformations - mostly copied from kelley old code
pov_tract <- pov_tract %>%
              mutate(
                percent_poverty = B17001_002E / B01003_001E,
                poverty_cat = cut(percent_poverty, breaks = c(0, .05, .10, .20, 1), labels = c("0-4.9%", "5-9.9%", "10-19.9%", "20-100%"), right = F),
                vintage = 2021) %>%
              rename(total_poverty = B17001_002E, total_population = B01003_001E, tract_name = tract, geoid = GEO_ID, name = NAME) %>%
              select(geoid, name, tract_name, vintage, total_population, total_poverty, percent_poverty, poverty_cat)

write.csv(pov_tract,
          paste0(key_get("rk-code-fpath"), "ccdph-data-sets/acs/acs-5yr-poverty-levels-by-tract.csv"), row.names = F)

