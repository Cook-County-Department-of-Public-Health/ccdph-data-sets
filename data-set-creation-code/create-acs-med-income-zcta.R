#Script for warehousing misc census pulls - most can be accomplished with censusapi wrapper
#Special cases for partials
#rishi.kowalski@cookcountyhealth.org, 3/14/22


#packages
library(censusapi)
library(tidyverse)
library(dplyr)
library(stringr)
library(keyring)


#pull income data by zcta
#read in zip/zcta data
ccdph_zip_dat <- read.csv(paste0(key_get("rk_code_fpath"), "ccdph-data-sets/cook-county-zip-codes.csv"), stringsAsFactors = F)
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
          paste0(key_get("rk_code_fpath"), "ccdph-data-sets/acs-5yr-med-house-income-by-zcta.csv"), row.names = F)
