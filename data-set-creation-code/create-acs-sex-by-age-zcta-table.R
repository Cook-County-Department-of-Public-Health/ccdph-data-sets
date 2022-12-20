#script to make sex by age table for all zctas in cook
#pull acs data -> merge onto existing list of cook zips -> pivot to long format and clean
#created 7/13 by rishi.kowalski@cookcountyhealth.org

library(dplyr)
library(censusapi)
library(tidyr)

#get zcta info
population_zcta <- read.csv(paste0(key_get("rk_code_fpath"), "ccdph-data-sets/decennial-2010-total-pop-zcta.csv"), stringsAsFactors = F)

vars_to_get <- c('B01001_003E', 'B01001_004E', 'B01001_005E', 'B01001_006E', 'B01001_007E', 'B01001_008E', 'B01001_009E', 'B01001_010E', 'B01001_011E', 'B01001_012E', 'B01001_013E', 'B01001_014E', 'B01001_015E', 'B01001_016E', 'B01001_017E', 'B01001_018E', 'B01001_019E', 'B01001_020E', 'B01001_021E', 'B01001_022E', 'B01001_023E', 'B01001_024E', 'B01001_025E', 'B01001_027E', 'B01001_028E', 'B01001_029E', 'B01001_030E', 'B01001_031E', 'B01001_032E', 'B01001_033E', 'B01001_034E', 'B01001_035E', 'B01001_036E', 'B01001_037E', 'B01001_038E', 'B01001_039E', 'B01001_040E', 'B01001_041E', 'B01001_042E', 'B01001_043E', 'B01001_044E', 'B01001_045E', 'B01001_046E', 'B01001_047E', 'B01001_048E', 'B01001_049E')

#get acs variable names then subset
acs_vars <- listCensusMetadata(name = "acs/acs5/",
                               vintage = ayear,
                               type = "variables")

acs_var_names <- acs_vars %>% filter(name %in% vars_to_get)

#get population estimates
age_by_sex_zcta <- getCensus("acs/acs5",
                             vintage = "2019",
                             vars = c("NAME", vars_to_get),
                             region = "zip code tabulation area:*", # tracts
                             regionin = "state:17", # places, counties, not msas
                             key = key_get("census_key"))

age_by_sex_zcta <- age_by_sex_zcta %>% mutate(zip_code_tabulation_area = as.numeric(zip_code_tabulation_area))

#join with existing dataset
zcta_joined <- left_join(population_zcta, age_by_sex_zcta %>% select(-c(state, NAME)), by = c("zip_code" = "zip_code_tabulation_area"))

#change names - paste names to variable label (for later)
names(zcta_joined)[-c(1:9)] <- paste(names(zcta_joined)[-c(1:9)], acs_var_names$label[match(names(zcta_joined[-c(1:9)]), acs_var_names$name)], sep = "|||")

#pivot longer, then split variable names to create variable to match up w/ Scott's data in github
age_by_zcta_long <- pivot_longer(zcta_joined %>% select(zip_code, contains("Estimate")), cols = contains("Estimate"), names_to = "varnamelong", values_to = "estimate")

#split variable names
age_by_zcta_long <- age_by_zcta_long %>% 
                      mutate(variable = sapply(strsplit(varnamelong, "[[:punct:]]{2,}"), "[[", 1),
                             sex = sapply(strsplit(varnamelong, "[[:punct:]]{2,}"), "[[", 4),
                             age = sapply(strsplit(varnamelong, "[[:punct:]]{2,}"), "[[", 5)) %>%
                      select(zip_code, variable, sex, age, estimate)                 
    
  
  
  
  
                                