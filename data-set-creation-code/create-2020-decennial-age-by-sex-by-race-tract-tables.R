---
output: html_document
editor_options: 
  chunk_output_type: console
---

# activate required packages
```{r}
library(censusapi)
library(DBI)
library(odbc)
library(tigris) 
library(tidyverse)
library(tidyr)
library(dplyr) 
library(sf)
library(data.table)
library(keyring)
```

# connect to inter-census and inter-spatial SQL Server databases
```{r}
#| label: connect to inter-census database

con_inter_census <- dbConnect(odbc::odbc(),
                               Driver   = "SQL Server",
                               Server   = key_get("ccdph_sql_server"),
                               Database = "inter-census")

con_inter_spatial <- dbConnect(odbc::odbc(),
                               Driver   = "SQL Server",
                               Server   = key_get("ccdph_sql_server"),
                               Database = "inter-spatial")

```

# import reference 2020 decennial datasets
```{r}
#| label: import reference tables from GitHub ccdph-datasets repository

example_df <- read_csv("https://github.com/Cook-County-Department-of-Public-Health/ccdph-data-sets/raw/main/2020/decennial-2020-race-ethnicity-by-muni-cleaned.csv")

example_df %>%
  select(race_ethnicity) %>%
  distinct()

```

# review 2020 decennial census APIs, groups and variables
```{r}
# return all census 2020 APIs
allCensusApis_df <- listCensusApis() %>% filter(vintage=="2020")

# return all groups in census 2020 demographic and housing counts
groups_census_2020_dhc <- listCensusMetadata(
  name = "dec/dhc",
  vintage = 2020,
  type = "groups",
  group = NULL,
  variable_name = NULL,
  include_values = FALSE
)

# select all P12 (age, sex, race) groups
groups_age_sex_race <- groups_census_2020_dhc %>%
  select(name, description) %>%
  filter(str_detect(name, "P12")) %>%
  distinct() %>%
  arrange(name)

# return all variables in census 2020 demographic and housing counts
variables_census_2020_dhc <- listCensusMetadata(
  name = "dec/dhc",
  vintage = 2020,
  type = "variables",
  group = NULL,
  variable_name = NULL,
  include_values = FALSE
)

```

# create 2020 decennial tracts by age, sex, race for Cook County
```{r}
# select all P12 (age, sex, race) variables
variables_age_sex_race <- variables_census_2020_dhc %>%
  select(name, label) %>%
  filter(str_detect(name, "P12")) %>%
  distinct() %>%
  arrange(name)

variables_age_groups <- variables_age_sex_race %>%
  filter(str_detect(name, "P12_")) %>%
  mutate(col_number = as.integer(substr(name,5,7)),
         label = str_replace_all(label, " !!Total:","Total"),
         label = str_replace_all(label, "Total!!Male:!!",""),
         label = str_replace_all(label, "Total!!Female:!!",""),
         label = str_replace_all(label, "!!","")) %>%
  select(-name, age = label)

# groups used for data tables
# name	description
# P12   SEX BY AGE FOR SELECTED AGE CATEGORIES
# P12H	SEX BY AGE FOR SELECTED AGE CATEGORIES (HISPANIC OR LATINO)
# P12I	SEX BY AGE FOR SELECTED AGE CATEGORIES (WHITE ALONE, NOT HISPANIC OR LATINO)
# P12J	SEX BY AGE FOR SELECTED AGE CATEGORIES (BLACK OR AFRICAN AMERICAN ALONE, NOT HISPANIC OR LATINO)
# P12K	SEX BY AGE FOR SELECTED AGE CATEGORIES (AMERICAN INDIAN AND ALASKA NATIVE ALONE, NOT HISPANIC OR LATINO)
# P12L	SEX BY AGE FOR SELECTED AGE CATEGORIES (ASIAN ALONE, NOT HISPANIC OR LATINO)
# P12M	SEX BY AGE FOR SELECTED AGE CATEGORIES (NATIVE HAWAIIAN AND OTHER PACIFIC ISLANDER ALONE, NOT HISPANIC OR LATINO)
# P12N	SEX BY AGE FOR SELECTED AGE CATEGORIES (SOME OTHER RACE ALONE, NOT HISPANIC OR LATINO)
# P12O	SEX BY AGE FOR SELECTED AGE CATEGORIES (TWO OR MORE RACES, NOT HISPANIC OR LATINO)

# Download tracts
grouplist <- c("P12H", "P12I", "P12J", "P12K", "P12L","P12M","P12N","P12O")
groupname <- c("Hispanic/Latino", "Non-Hispanic White", "Non-Hispanic Black", "Non-Hispanic Other", "Non-Hispanic Asian","Non-Hispanic Other","Non-Hispanic Other","Non-Hispanic Multiracial")
yearlist <- c(2020)
for (agroup in grouplist) {
  for (ayear in yearlist) {
    agroupname = paste("group(",agroup,")",sep="")
    census_group <- getCensus(name = "dec/dhc",
                           vintage = ayear,
                           vars = c(agroupname),
                           region = "tract:*", # tracts
                           regionin="state:17+county:031", # places, counties, not msas
                           key="8f6a0a83c8a2466e3e018a966846c86412d0bb6e")
    census_group <- census_group %>% 
      select(-ends_with("NA")) %>%
      mutate(census_group = agroup,
             vintage = ayear,
             source = "https://api.census.gov/data/2020/dec/dhc") %>%
      rename_with(~str_replace(.,agroup,"age"))
    # acs_group <- acs_group %>% select(-contains("MA"))
    # acs_group <- acs_group %>% select(-contains("GEO_ID"))
    # acs_group <- acs_group %>% select(-contains("M_1"))
    # acs_group <- acs_group %>% select(-contains("M"))
    # acs_group$year<-ayear 
    # acs_group$GEOID_tract<-paste0(state,county,tract)
    assign(paste(agroup,"tract",ayear,sep="_"),census_group)
    # rm(acs_group)
    # detach(acs_group)
  }
}

apattern_tract <- paste("tract_2020")
alist_tract <- mget(ls(pattern = apattern_tract))
tract_age_sex_race_2020 <- bind_rows(alist_tract)

group_list <- as.data.frame(grouplist) %>% rename(census_group = 1)
group_category <- as.data.frame(groupname) %>% rename(race = 1)
group_categories <- bind_cols(group_list, group_category)

tract_age_sex_race_2020_attr <- tract_age_sex_race_2020 %>%
  left_join(group_categories, by = "census_group") %>%
  pivot_longer(cols=c(age_001N:age_049N), names_to = "age_sex", values_to = "population") %>%
  mutate(geoid_tract_2020 = paste0(state,county,tract),
         col_number = as.integer(substr(age_sex,5,7)),
         sex = case_when(col_number == 1 ~ "Total",
                   col_number > 1 & col_number <=25 ~ "Male",
                   col_number > 25 & col_number <=49 ~ "Female")) %>%
  left_join(variables_age_groups, by="col_number") %>%
  filter(col_number >= 3, col_number != 26) %>%
  group_by(geoid_tract_2020, name_tract = NAME, sex, race, age, vintage, source, col_number) %>%
  summarise(population = sum(population)) %>%
  arrange(geoid_tract_2020,
          sex,
          race,
          col_number) %>%
  select(geoid_tract_2020, name_tract, sex, race, age, population, vintage, source)

# add location, district attributes to table
tracts_2020_cook_county <- dbReadTable(conn = con_inter_spatial,
            Id(schema = "ref", table = "tracts_2020_cook_county")) %>%
  select(geoid_tract_2020, name_district, location)

tract_age_sex_race_2020_attr_locations <- tract_age_sex_race_2020_attr %>% 
  left_join(tracts_2020_cook_county, by="geoid_tract_2020")

# write table to csv for GitHub and inter-census SQL Server database
dbWriteTable(conn = con_inter_census, 
               Id(schema="ref", table="decennial-2020-age-sex-race-ethnicity-by-tract"), 
               overwrite= TRUE,
               tract_age_sex_race_2020_attr_locations)

write_csv(tract_age_sex_race_2020_attr_locations, "C:/Users/christopher.smith/OneDrive - Cook County Health/git_repos/justenvirons/ccdph-data-sets/2020/decennial-2020-age-sex-race-ethnicity-by-tract.csv")

```

