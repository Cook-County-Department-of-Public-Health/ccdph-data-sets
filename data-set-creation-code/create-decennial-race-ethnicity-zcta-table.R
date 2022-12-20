#Script to grab 2010 Decennial race-ethnicity data for ZCTAs in SCC
#created 11/1/21 by rishi.kowalski@cookcountyhealth.org
#can't use censusapi to get partial zcta estimates, so reused kelley's code

#load packages
library(tidyverse)
library(jsonlite)
library(janitor)
library(keyring)


#get list of all zctas in cook w/ population for join later
cook_zctas_all <- read.csv("https://github.com/Cook-County-Department-of-Public-Health/ccdph-data-sets/blob/main/cook-county-zip-codes.csv?raw=TRUE") %>%
                    drop_na(zcta_2010_pop) %>%
                    select(zcta, zcta_2010_pop, zcta_2010_pop_cook)

  

#get list of zctas in cook - filter out zero population in cook or na population in cook/total
cook_zctas <- read.csv("https://github.com/Cook-County-Department-of-Public-Health/ccdph-data-sets/blob/main/cook-county-zip-codes.csv?raw=TRUE") %>%
                filter(!(is.na(zcta_2010_pop) | is.na(zcta_2010_pop_cook) | zcta_2010_pop_cook == 0)) %>%
                pull(zcta)

#relevant racial-ethinic variables from P5 group  
race_vars <- c("P005003", "P005004", "P005005", "P005006", "P005007", "P005008", "P005009", "P005011", "P005012", "P005013", "P005014", "P005015", "P005016", "P005017")


#get variable labels
dec_vars <- fromJSON("https://api.census.gov/data/2010/dec/sf1/variables") %>%
              as.data.frame() %>%
              row_to_names(row_number = 1) %>%
              filter(name %in% race_vars)


#initialize list
dec_race_pop_zcta_lst <- rep(list(NA), length(cook_zctas))

#loop to get partial zcta race counts into single list
#put data into list, skips over entries already downloaded: census api having some serious issues timing out so needed multiple passes needed
#can't use apply because connection seemingly breaks at random and want to save data as to not recall already accessed data
for (index in 1:length(cook_zctas)){
  zcta <- cook_zctas[index]
  if (is.na(dec_race_pop_zcta_lst[[index]])) {
    api_call <- paste0("https://api.census.gov/data/2010/dec/sf1?get=NAME,group(P5)&for=county%20(or%20part):031&in=state:17%20zip%20code%20tabulation%20area%20(or%20part):", zcta,
                       "&key=", key_get("census_key"))
    
    #call data, select relevant columns, pivot to longer format
    zcta_data <- fromJSON(api_call) %>% 
      as.data.frame() %>%
      row_to_names(row_number = 1) %>%
      select("zip code tabulation area (or part)",
             all_of(race_vars)) %>%
      rename("zcta_or_pt" = "zip code tabulation area (or part)") %>%
      pivot_longer(cols = -zcta_or_pt, names_to = "variable", values_to = "estimate")
    
    dec_race_pop_zcta_lst[[index]] <- zcta_data
    print(paste(zcta, "done"))
  } else {
    print(paste(zcta, "not added"))
  } 
}

#bind rows of list, join on labels and clean labels to race + ethnicity vars
dec_race_pop_zcta <- bind_rows(dec_race_pop_zcta_lst) %>%
                      left_join(., dec_vars %>% select(name, label), by = c("variable"= "name")) %>%
                      mutate(ethnicity = str_split(label, "!!", simplify = T)[, 2],
                             race = str_split(label, "!!", simplify = T)[, 3],
                             zcta_or_pt = as.numeric(zcta_or_pt),
                             estimate = as.numeric(estimate)) %>%
                      select(zcta_or_pt, variable, ethnicity, race, estimate)

#join with total zcta data to include zcta w/ no population in cook
dec_race_pop_zcta <- full_join(cook_zctas_all, dec_race_pop_zcta, by = c("zcta" = "zcta_or_pt"))

#recreate dataframe for zctas that were not called and join w/ estimate = NA
zctas_no_data <- expand.grid(zcta_or_pt = setdiff(cook_zctas_all$zcta, dec_race_pop_zcta$zcta_or_pt),
                  variable = unique(dec_race_pop_zcta$variable)) %>%
                  left_join(., unique(dec_race_pop_zcta[, 2:4])) %>%
                  mutate(estimate = NA)

dec_race_pop_zcta <- rbind(dec_race_pop_zcta, zctas_no_data)

#join w/ total pop data
dec_race_pop_zcta <- left_join(dec_race_pop_zcta, cook_zctas_all, by = c("zcta_or_pt" = "zcta")) %>%
                      select(zcta_or_pt, zcta_2010_pop, zcta_2010_pop_cook, variable, ethnicity, race, estimate)

write.csv(dec_race_pop_zcta, paste0(key_get("rk_code_fpath"), "ccdph-data-sets/decennial-2010-race-ethnicity-by-zcta.csv"), row.names = F)
