#script to calculate income inequality in cook county zips
#county health rankings ratio of income inequality
#https://www.countyhealthrankings.org/explore-health-rankings/measures-data-sources/county-health-rankings-model/health-factors/social-and-economic-factors/income/income-inequality
#created 10/6/22 by rishi.kowalski@cookcountyhealth.org
<<<<<<< HEAD
#updated 01/20/23 by rishi.kowalski@cookcountyhealth.org: add gini coefficient

#load needed packages
require(censusapi)
require(keyring)
=======

#load needed packages
require(censusapi)
>>>>>>> 8463399b8fc91e5203bde41e71bf0b8e9b3f5435

#load zcta data
cook_zcta <- read.csv(paste0(key_get("rk_code_fpath"), "ccdph-data-sets/cook-county-zip-codes.csv"), stringsAsFactors = F)

#call census api, var = B19080; no moes available for this variable: ignore for now
#add median income of zcta for reference: B19013_001E
<<<<<<< HEAD
#1/20/23: add gini coef (ACS Var = B19083)
inc_var <- "B19080"
gini_var <-  "B19083_001E"
med_inc_var <- "B19013_001E"
vars_to_get <- c(med_inc_var, paste0(inc_var, paste0("_00", 1:5), "E"), gini_var)

ayear <- 2020

#get acs variable names for income vars
acs_inc_ineq_vars <- listCensusMetadata(name = "acs/acs5/",
                                        vintage = ayear,
                                        type = "variables",
                                        group = inc_var)
=======
variable <- "B19080"
vars_to_get <- paste0(variable, paste0("_00", 1:5), "E")

ayear <- 2019

#get acs variable names 
acs_inc_ineq_vars <- listCensusMetadata(name = "acs/acs5/",
                                        vintage = ayear,
                                        type = "variables",
                                        group = variable)
>>>>>>> 8463399b8fc91e5203bde41e71bf0b8e9b3f5435


#get income quintile estimates for all IL then subset to relevant zips
inc_ineq_il_zcta <- getCensus("acs/acs5",
                              vintage = "2019",
<<<<<<< HEAD
                              vars = c("NAME", vars_to_get),
=======
                              vars = c("NAME", "B19013_001E", vars_to_get),
>>>>>>> 8463399b8fc91e5203bde41e71bf0b8e9b3f5435
                              region = "zip code tabulation area:*", # zctas
                              regionin = "state:17",
                              key = key_get("census_key"))

inc_ineq_cc_zcta <- subset(inc_ineq_il_zcta, zip_code_tabulation_area %in% cook_zcta$zcta, select = -c(state, NAME)) #62 zips w/ no zcta excluded (list of po boxes)

#force 60141 to N/A
inc_ineq_cc_zcta[which(apply(inc_ineq_cc_zcta[, -1], 1, sum) < 0), -1] <- NA

#name cols
<<<<<<< HEAD
var_names <- c("zcta", "med_inc", paste0("inc_", c("lowest", "second", "third", "fourth"), "_quint_ul"), "inc_top5_pct_ll", "gini_coef")
=======
var_names <- c("zcta", "med_inc", paste0("inc_", c("lowest", "second", "third", "fourth"), "_quint_ul"), "inc_top5_pct_ll")
>>>>>>> 8463399b8fc91e5203bde41e71bf0b8e9b3f5435
names(inc_ineq_cc_zcta) <- var_names

#calculate income inequality ratio
inc_ineq_cc_zcta$inc_ineq_ratio <- with(inc_ineq_cc_zcta, inc_fourth_quint_ul / inc_lowest_quint_ul)

<<<<<<< HEAD
#reorder
inc_ineq_cc_zcta <- inc_ineq_cc_zcta[, c(1:7, 9, 8)]

write.csv(inc_ineq_cc_zcta, paste0(key_get("rk_code_fpath"), "ccdph-data-sets/acs/acs-5yr-income-inequality-by-zcta.csv"), row.names = F)
=======
write.csv(inc_ineq_cc_zcta, paste0(key_get("rk_code_fpath"), "ccdph-data-sets/acs/acs-5yr-income-inequality-by-zcta"), row.names = F)
>>>>>>> 8463399b8fc91e5203bde41e71bf0b8e9b3f5435
