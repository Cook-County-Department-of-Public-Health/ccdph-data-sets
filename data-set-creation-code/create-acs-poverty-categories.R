
#load required packages
library(jsonlite)
library(tidyverse)
library(keyring)
library(janitor)
library(odbc)
library(DBI)

#store your census api key - only needs to be done once per computer
#key_set("census-api-key")

#source census functions
devtools::source_url("https://github.com/Cook-County-Department-of-Public-Health/ccdph-functions/blob/master/census-functions.R?raw=TRUE")

#set master API URL
#set api string
census_api <- "https://api.census.gov/data/2024/acs/acs5"
vintage <- "2024 5 YR"
end_year <- 2024

#store variables for master URL
census_var_names <- fromJSON(paste0(census_api, "/variables")) %>%
  as.data.frame() %>%
  row_to_names(row_number = 1)

#pull in list of ccdph tracts from inter-census database with total pop
pop_con <-  dbConnect(odbc::odbc(), Driver = "SQL Server", Server = key_get("ccdph_sql_server"), Database = "inter-census")
ccdph_tracts <- dbGetQuery(pop_con, "SELECT * FROM [inter-census].[ref].[decennial-2020-age-sex-race-ethnicity-by-tract]") %>%
  filter(location == "CCDPH") %>%
  mutate(tract_code = substr(geoid_tract_2020, 6,11)) %>%
  group_by(tract_code) %>%
  summarize(population = sum(population)) %>%
  ungroup()

#pull poverty table for all tracts
acs_tract_pov <- map(ccdph_tracts$tract_code, ~get_small_geo_pops_acs(census_api = census_api, place_code = .x, geo_type = "tract", variable = "B17001", group = T, pop_in_cook = F, var_key = census_var_names)) %>%
  discard(inherits, "character") %>%
  bind_rows()

#filter to totals, calc percent, bin into cats
#IMPORTANT: the total pop in this table is the population for whom poverty status is determined, it can be used to calculate the percent poverty but is not equal to the total population
acs_tract_pov_percent <- acs_tract_pov %>%
  #filter(estimate %in% c("B17001_001E", "B17001_002E")) %>%
  mutate(var_type = case_when(variable == "B17001_001E" ~ "total_pop",
                              variable == "B17001_002E" ~ "pov_pop")) %>%
  drop_na(var_type) %>%
  select(tract = census_place_code, name = NAME, var_type, estimate) %>%
  pivot_wider(id_cols = c(tract, name), names_from = var_type, values_from = estimate) %>%
  mutate(percent_poverty = pov_pop / total_pop) %>%
  mutate(poverty_cat = cut(percent_poverty, breaks = c(0, .05, .10, .20, 1), labels = c("0-4.9%", "5-9.9%", "10-19.9%", "20-100%"), right = F)) 

#pull the total populatopn
#join poverty categories back to total ACS tract populations and aggregate
#pull poverty table for all tracts
acs_tract_total <- fromJSON(paste0("https://api.census.gov/data/2024/acs/acs5?get=NAME,B01001_001E&for=tract:*&in=state:17&key=", key_get("census-api-key"))) %>%
  as.data.frame() %>%
  janitor::row_to_names(row_number = 1) %>%
  filter(tract %in% ccdph_tracts$tract_code) %>%
  mutate(total_acs_pop = as.numeric(B01001_001E)) %>%
  left_join(select(acs_tract_pov_percent, tract, poverty_cat)) %>%
  group_by(poverty_cat) %>%
  summarize(pop = sum(total_acs_pop)) %>%
  ungroup
                             
write_csv(acs_tract_total, "acs-5yr-2024-povery-cat-pops.csv")

#if desired, same calculation using decennial (ACS total is a fair bit under decennial)
# pov_cat_pops <- ccdph_tracts %>%
#   left_join(select(acs_tract_pov_percent, tract_code = tract, poverty_cat)) %>%
#   group_by(poverty_cat) %>%
#   summarize(pop = sum(population)) %>%
#   ungroup()
  


