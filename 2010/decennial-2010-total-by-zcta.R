decennial_2010_detailed_age_sex_by_zcta <- read_csv(file = "2010/decennial-2010-age-sex-by-zcta.csv")
decennial_2020_detailed_age_sex_by_zcta <- read_csv(file = "2020/decennial-2020-age-sex-by-zcta.csv")

con_inter_census <- dbConnect(odbc::odbc(),
                              Driver   = "SQL Server",
                              Server   = key_get("ccdph_sql_server"),
                              Database = "inter-census")

dbWriteTable(conn = con_inter_census, 
             Id(schema="ref", table="decennial-2010-detailed-age-sex-by-zcta"), 
             overwrite= TRUE,
             decennial_2010_detailed_age_sex_by_zcta)

dbWriteTable(conn = con_inter_census, 
             Id(schema="ref", table="decennial-2020-detailed-age-sex-by-zcta"), 
             overwrite= TRUE,
             decennial_2020_detailed_age_sex_by_zcta)

decennial_2010_total_by_zcta <- decennial_2010_detailed_age_sex_by_zcta %>% 
  group_by(zcta) %>% 
  summarise(pop_total = sum(total_pop),
            pop_in_cook = sum(pop_in_cook)) %>% 
  mutate(pct_in_cook = pop_in_cook/pop_total*100,
         partial = if_else(pct_in_cook == 100,0,1)) %>% 
  drop_na(pop_in_cook) %>% 
  rename(geoid_zcta = zcta)

dbWriteTable(conn = con_inter_census, 
             Id(schema="ref", table="decennial-2010-total-by-zcta"), 
             overwrite= TRUE,
             decennial_2010_total_by_zcta)

decennial_2020_total_by_zcta <- read_csv(file = "2020/decennial-2020-total-zcta.csv")

dbWriteTable(conn = con_inter_census, 
             Id(schema="ref", table="decennial-2020-total-by-zcta"), 
             overwrite= TRUE,
             decennial_2020_total_by_zcta)