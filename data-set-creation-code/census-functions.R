library(tidycensus)
library(tidyverse)
library(magrittr)
library(janitor)

ccdph_census_population = function(variable){
  all_cook  = get_decennial(geography = "county", 
                                             state = "IL", county = "Cook",
                                             year = 2010, variables = variable) %>%
    pull(value)
  
  ooj  = get_decennial(geography = "place", 
                                        state = "IL", 
                                        year = 2010, variables = variable) %>%
    filter(NAME %in% c("Chicago city, Illinois",
                       "Evanston city, Illinois",
                       "Skokie village, Illinois",
                       "Oak Park village, Illinois"
    )) %>%
    pull(value) %>%
    sum()
  
  ooj2  = get_decennial(geography = "county subdivision", 
                                         state = "IL", county = "Cook",
                                         year = 2010, variables = variable) %>%
    filter(NAME == "Stickney township, Cook County, Illinois") %>%
    pull(value)
  
  ccdph = all_cook - ooj - ooj2
  return(ccdph)
  
}

#example- total population
# ccdph_census_population("P001001")

#example- males under 5 + females under 5 = total under 5
# ccdph_census_population("P012003") + ccdph_census_population("P012027")

#example all SEX BY AGE vars
# sex_by_age_vars = variables_sf1 %>% filter(concept == "SEX BY AGE") %>% pull(name)
# names(sex_by_age_vars) = variables_sf1 %>% filter(concept == "SEX BY AGE") %>% pull(label)

# sex_by_age_vector = sapply(sex_by_age_vars, ccdph_census_population)
# sex_by_age_ccdph = cbind(names(sex_by_age_vector), sex_by_age_vector) %>%
#   as.tibble() %>%
#   set_colnames(c("group", "population")) %>%
#   mutate(sex = str_extract(group, "(Male|Female)"),
#          sex = ifelse(is.na(sex), "Total", sex),
#          age = gsub("^.*!!", "", group),
#          age = ifelse(age %in% c("Total", "Male", "Female"), "Total", age)
#   ) %>%
#   relocate(population, .after = last_col())


ccdph_acs_population = function(variable, year = 2019, survey = "acs5"){
  all_cook  = get_acs(geography = "county", survey = survey,
                      state = "IL", county = "Cook",
                      year = year, variables = variable) %>%
    pull(estimate)
  
  ooj  = get_acs(geography = "place",  survey = survey,
                 state = "IL", 
                 year = year, variables = variable) %>%
    filter(NAME %in% c("Chicago city, Illinois",
                       "Evanston city, Illinois",
                       "Skokie village, Illinois",
                       "Oak Park village, Illinois"
    )) %>%
    pull(estimate) %>%
    sum()
  
  ooj2  = get_acs(geography = "county subdivision",  survey = survey,
                  state = "IL", county = "Cook",
                  year = year, variables = variable) %>%
    filter(NAME == "Stickney township, Cook County, Illinois") %>%
    pull(estimate)
  
  ccdph = all_cook - ooj - ooj2
  return(ccdph)
  
}


