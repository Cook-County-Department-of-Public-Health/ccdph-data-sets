#get decennial single age populations by race for cook co zctas
#created 11/21/22 by rishi.kowalski@cookcountyhealth.org

require(jsonlite)
require(dplyr)
require(censusapi)
require(keyring)

#read in cc zips
cc_zips <- read.csv(paste0(key_get("rk_code_fpath"), "ccdph-data-sets/cook-county-zip-codes.csv")) %>% pull(zcta)

#PCT010 H-0 for non hispanic single race pops
single_age_race_grp_letters <- LETTERS[8:15]


#helper function for getSingleAgeRace()
#pivot census data from wide to long 
pivotLong <- function(grp_df_row) {
  grp_var_zcta_long <- data.frame(zcta = grp_df_row[1], var = names(grp_df_row[-1]), pop = grp_df_row[-1], row.names = NULL)
  return(grp_var_zcta_long)
}

#function to get group-level single-age estimates by race, subset to ccok then join on labels
getSingleAgeRace <- function(grp_letter) {
  #group label
  grpvar <- paste0("PCT12", grp_letter)
  
  #call group labels
  grp_lab_df <- listCensusMetadata(name = "dec/sf1",
                                   vintage = 2010,
                                   type = "variables",
                                   group = grpvar)
  
  #subset out total pop, total pop by sex; label has extra 0s before 12
  names_out <- paste0("PCT012", grp_letter, c("001", "002", "106"))
  
  grp_lab_df <- subset(grp_lab_df, !name %in% names_out)

  #break out var labels to individual variables
  grp_lab_df$sex <- sapply(strsplit(grp_lab_df$label, "\\!+"), "[", 2)
  grp_lab_df$age <- sapply(strsplit(grp_lab_df$label, "\\!+"), "[", 3)
  grp_labs <- subset(grp_lab_df, select = c("name", "sex", "age", "concept"))
  
  #call census vars for group
  grp_var_zcta_il <- getCensus(name = "dec/sf1",
                               vintage = 2010,
                               vars = paste0("group(", grpvar,")"),
                               region = "zip code tabulation area (or part):*",
                               regionin = "state:17",
                               key = key_get("census_key"))
  
  #subset 
  pct_vars <- names(grp_var_zcta_il)[grepl("PCT", names(grp_var_zcta_il))]
  pct_vars <- pct_vars[!pct_vars %in% names_out]
  grp_var_zcta_cc <- subset(grp_var_zcta_il, zip_code_tabulation_area_or_part %in% cc_zips, 
                            select = c("zip_code_tabulation_area_or_part", pct_vars))
  names(grp_var_zcta_cc)[1] <- "zcta"
  grp_var_zcta_cc$zcta <- as.numeric(grp_var_zcta_cc$zcta)
  
  rm(grp_var_zcta_il)
  
  #convert to long
  grp_var_zcta_long <- do.call("rbind", apply(grp_var_zcta_cc, 1, pivotLong))

  #merge with labels and reorder
  grp_var_zcta_long <- merge(grp_var_zcta_long, grp_labs, by.x = "var", by.y = "name", all.x = TRUE)
  grp_var_zcta_out <- grp_var_zcta_long[, c("zcta", "var", "sex", "age", "concept", "pop")]
  
  rm(grp_var_zcta_long)
  
  return(grp_var_zcta_out)
}


dec_sing_age_race_zcta <- do.call("rbind", lapply(single_age_race_grp_letters, getSingleAgeRace))

write.csv(dec_sing_age_race_zcta, paste0(key_get("rk_code_fpath"), "ccdph-data-sets/decennial-2010-age-sex-single-age-race-zcta"), row.names = F)
