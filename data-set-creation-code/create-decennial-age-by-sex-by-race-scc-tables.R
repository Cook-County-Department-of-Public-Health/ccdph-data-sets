#script to get decennial age/sex by race estimates for suburban cook county (note this is not CCDPH jurisdiction)
#plan: download cook totals and chicago totals, subtract for scc
#note from rishi: I am testing out base r approach, so may look different w/ no dplyr
#created 1/24/22 by rishi.kowalski@cookcountyhealth.org

#load packages
library(tidyverse)
library(jsonlite)
library(censusapi)
library(keyring)

#Census Groups: PCT12H-PCT120 (make sure non hispanic/latino (9/1/22)
cen_groups_tables <- listCensusMetadata(
                      name = "dec/sf1",
                      vintage = cyear,
                      type = "groups") %>%
                     filter(grepl("P12", name))

cgroup <- "P12"
cen_groups_vars <- listCensusMetadata(
  name = "dec/sf1",
  vintage = cyear,
  group = cgroup,
  type = "variables")

#list of groups to iterate over 
cen_grps <- paste0("PCT12", LETTERS[8:15])

#function to call group-level data for CC and Chi, clean and subtract for SCC es
getGroupEst <- function(grp, cyear = "2010") {
  #initialize name for "vars" argument
  vars_to_get <- c("NAME", paste0("group(", grp, ")"))
  print(grp)
  
  #get var names to refer to
  grp_vars <- listCensusMetadata(name = "dec/sf1",
                                 vintage = cyear,
                                 group = grp,
                                 type = "variables")
  
  #vars to pull from census df
  vars_out <- c("NAME", unique(grp_vars$name))
    
  #gen census estimates
  chi_est <- getCensus(name = "dec/sf1",
                       vintage = cyear,
                       vars = vars_to_get,
                       region = "place:14000", #chicago -> place:14000
                       regionin ="state:17", 
                       key = key_get("census_key"))
  
  cc_est <- getCensus(name = "dec/sf1",
                      vintage = cyear,
                      vars = vars_to_get,
                      region = "county: 031", #cook county -> county: 031
                      regionin ="state:17", 
                      key = key_get("census_key"))
  
  #try out do. call to subset and bind data, calculate scc estimate and rebind
  combined_est <- do.call(rbind, lapply(list(chi_est, cc_est), function(x) x[, vars_out]))
  scc_placehold <- data.frame(c("Suburban Cook County, Illinois", combined_est[2, -1] - combined_est[1, -1]))
  names(scc_placehold) <- names(combined_est)
  combined_est <- rbind(combined_est, scc_placehold)
  
  #convert to long
  combined_long <- data.frame(name = rep(unique(combined_est$NAME), times = ncol(combined_est) - 1),
                              variable = rep(names(combined_est[-1]), each = 3), #needs to align w/ unlist (goes column-wise)
                              estimate = unlist(combined_est[-1], use.names = F))
  
  #merge with variable information
  combined_long <- merge(combined_long, grp_vars[, c("name", "label", "concept")], 
                         by.x = c("variable"), by.y = c("name"), all.x = TRUE)
  
  return(combined_long)
}


#get data from all race-ethnic groups and bind
scc_age_race_sex_data <- do.call(rbind, lapply(cen_grps, getGroupEst))

#clean data: concept -> race/eth, label -> sex/age category
scc_age_race_sex_data$sex <- str_split(scc_age_race_sex_data$label, "!+", simplify = T)[, 2]
scc_age_race_sex_data$age <- str_split(scc_age_race_sex_data$label, "!+", simplify = T)[, 3]
scc_age_race_sex_data$race_ethnicity <- gsub("SEX BY AGE \\(|\\)", "", scc_age_race_sex_data$concept)

#subset data: reorder cols and remove label, concept
#remove totals rows
var_order <- c("name", "variable", "sex", "age", "race_ethnicity", "estimate")
scc_age_race_sex_data_out <- scc_age_race_sex_data[scc_age_race_sex_data$age != "", var_order]

write.csv(scc_age_race_sex_data_out, paste0(key_get("rk_code_fpath"), "ccdph-data-sets/decennial-2010-age-sex-race-suburban-cook.csv"), row.names = F)


################################################################
#KB adding pops for ccdph jurisdiction to the file saved above
#code maintained in one script to better understand file creation history

#load functions to pull ccdph specific pops
devtools::source_url("https://github.com/Cook-County-Department-of-Public-Health/ccdph-functions/blob/master/census-functions.R?raw=TRUE")

#load in existing file with scc and chicago pops
current_file <- read_csv("decennial-2010-age-sex-race-suburban-cook.csv")

#isolate variables to pull
vars_to_pull <- current_file %>% pull(variable) %>% unique()

#pull variables to pop values for ccdph
ccdph_values <- sapply(vars_to_pull, ccdph_census_population)

#create dataset for binding
ccdph_values_clean <- data.frame(name = "CCDPH jurisdiction", 
                                 variable = names(ccdph_values), 
                                 sex = NA_character_,
                                 age = NA_character_,
                                 race_ethnicity = NA_character_,
                                 estimate = ccdph_values, row.names = NULL)

#combine with existing file and complete missing labels
updated_file <- current_file %>%
  rbind(ccdph_values_clean) %>%
  arrange(variable) %>%
  group_by(variable) %>%
  fill(age, sex, race_ethnicity) %>%
  ungroup()

#resave file
write_csv(updated_file, "decennial-2010-age-sex-race-suburban-cook.csv")
