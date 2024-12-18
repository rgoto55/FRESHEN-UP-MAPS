################################################################################
################### SCRIPT FOR EXTRACTING HOUSEHOLD INFORMATION ################
################################################################################

# Author: Mo Osman
# Date created: 23-07-2024
# Modified: Rie Goto (September-November 2024)

# In this script, I will extract household information from the Tanzania NPS 
# Wave 5:
# https://microdata.worldbank.org/index.php/catalog/5639/data-dictionary

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "srvyr")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ IN DATA: 

# Data from survey modules: 
hh_info <- read_csv("data-TNPS/raw_data/hh_sec_a.csv")
hh_demographics <- read_csv("data-TNPS/raw_data/hh_sec_b.csv")
hh_education <- read_csv("data-TNPS/raw_data/hh_sec_c.csv")
hh_consumption <- read_csv("data-TNPS/raw_data/consumption_real_y5.csv") 

#-------------------------------------------------------------------------------

# SELECT RELEVANT HOUSEHOLD INFO DATA: 
names(hh_info)
hh_info <- hh_info %>% 
  dplyr::select(y5_hhid, y5_cluster, y5_crossweight, clusterid, strataid, hh_a01_1, hh_a02_1, hh_a04_1, y5_rural, int_month_year) %>% 
  rename(hhid = y5_hhid,
         y5_cluster = y5_cluster, # cluster ID (4709hhs)
         survey_wgt = y5_crossweight,# hh weight for full sample (panel + booster= 4709hhs)
         adm1 = hh_a01_1, # Region
         adm2 = hh_a02_1, # District
         ea = hh_a04_1) %>% # EA code
  mutate(year = case_when(int_month_year == 1 ~ 2020,
                          int_month_year %in% 2:13 ~ 2021,
                          int_month_year == 14 ~ 2022,
                          TRUE ~ NA_real_)) %>% 
  mutate(month = case_when(int_month_year == 1 ~ 12,
                           int_month_year == 2 ~ 1,
                           int_month_year == 3 ~ 2,
                           int_month_year == 4 ~ 3,
                           int_month_year == 5 ~ 4,
                           int_month_year == 6 ~ 5,
                           int_month_year == 7 ~ 6,
                           int_month_year == 8 ~ 7,
                           int_month_year == 9 ~ 8,
                           int_month_year == 10 ~ 9,
                           int_month_year == 11 ~ 10,
                           int_month_year == 12 ~ 11,
                           int_month_year == 13 ~ 12,
                           int_month_year == 14 ~ 1,
                           TRUE ~ NA_real_)) %>% 
  dplyr::select(-int_month_year) %>% 
  mutate(res = case_when(y5_rural == 1 ~ "Rural",
                         y5_rural == 2 ~ "Urban",
                         TRUE ~ NA_character_)) %>% 
  dplyr::select(-y5_rural)

# Join demographic and education data together: 
hh_demographics <- hh_demographics %>% 
  left_join(hh_education %>% 
              dplyr::select(y5_hhid, indidy5, hh_c07) %>% 
              rename(educ_head = hh_c07),
            by = c("y5_hhid", "indidy5"))

rm(hh_education)
  
# Extract details on household head and join to hh_info: 
hh_info <- hh_info %>% 
  left_join(hh_demographics %>% 
              dplyr::select(y5_hhid, hh_b02, hh_b04, hh_b05, educ_head) %>% 
              filter(hh_b05 == 1) %>%
              dplyr::select(-hh_b05) %>%
              rename(hhid = y5_hhid,
                     sex_head = hh_b02,
                     age_head = hh_b04) %>% 
              mutate(sex_head = case_when(sex_head == 1 ~ "Male",
                                          sex_head == 2 ~ "Female",
                                          TRUE ~ NA_character_)),
            by = "hhid")

rm(hh_demographics)

#-------------------------------------------------------------------------------

# CALCULATE CONSUMPTION QUINTILES: 

# Firstly extract total consumption (spatially and temporally adjusted): 
hh_consumption <- hh_consumption %>% 
  rename(hhid = y5_hhid) %>%
  left_join(hh_info %>% dplyr::select(hhid, res),
            by = "hhid") %>% 
  dplyr::select(hhid, hhweight, res, expmR_pae)

# Create tbl_svy object: 
svy_hh_consumption <- hh_consumption %>% 
  as_survey_design(weights = hhweight)

# Calculate consumption quintiles cut-points: 
consumption_quantiles <- svy_hh_consumption %>% 
  summarise(consumption = survey_quantile(expmR_pae, c(0.2, 0.4, 0.6, 0.8)))

urban_quantiles <- svy_hh_consumption %>% 
  filter(res == "Urban") %>%
  summarise(consumption = survey_quantile(expmR_pae, c(0.2, 0.4, 0.6, 0.8)))

rural_quantiles <- svy_hh_consumption %>% 
  filter(res == "Rural") %>%
  summarise(consumption = survey_quantile(expmR_pae, c(0.2, 0.4, 0.6, 0.8)))

# Apply cut-points to data: 
hh_consumption <- hh_consumption %>% 
  mutate(sep_quintile = case_when(expmR_pae < consumption_quantiles$consumption_q20 ~ 1,
                                  expmR_pae >= consumption_quantiles$consumption_q20 & 
                                    expmR_pae < consumption_quantiles$consumption_q40 ~ 2,
                                  expmR_pae >= consumption_quantiles$consumption_q40 & 
                                    expmR_pae < consumption_quantiles$consumption_q60 ~ 3,
                                  expmR_pae >= consumption_quantiles$consumption_q60 & 
                                    expmR_pae < consumption_quantiles$consumption_q80 ~ 4,
                                  expmR_pae >= consumption_quantiles$consumption_q80 ~ 5,
                                  TRUE ~ NA_real_)) %>% 
  mutate(res_quintile = case_when(res == "Urban" & 
                                    expmR_pae < urban_quantiles$consumption_q20 ~ 1,
                                  res == "Urban" & expmR_pae >= urban_quantiles$consumption_q20 & 
                                    expmR_pae < urban_quantiles$consumption_q40 ~ 2,
                                  res == "Urban" & 
                                    expmR_pae >= urban_quantiles$consumption_q40 & 
                                    expmR_pae < urban_quantiles$consumption_q60 ~ 3,
                                  res == "Urban" & 
                                    expmR_pae >= urban_quantiles$consumption_q60 & 
                                    expmR_pae < urban_quantiles$consumption_q80 ~ 4,
                                  res == "Urban" & 
                                    expmR_pae >= urban_quantiles$consumption_q80 ~ 5,
                                  res == "Rural" & 
                                    expmR_pae < rural_quantiles$consumption_q20 ~ 1,
                                  res == "Rural" & 
                                    expmR_pae >= rural_quantiles$consumption_q20 & 
                                    expmR_pae < rural_quantiles$consumption_q40 ~ 2,
                                  res == "Rural" & 
                                    expmR_pae >= rural_quantiles$consumption_q40 & 
                                    expmR_pae < rural_quantiles$consumption_q60 ~ 3,
                                  res == "Rural" & 
                                    expmR_pae >= rural_quantiles$consumption_q60 & 
                                    expmR_pae < rural_quantiles$consumption_q80 ~ 4,
                                  res == "Rural" & 
                                    expmR_pae >= rural_quantiles$consumption_q80 ~ 5,
                                  TRUE ~ NA_real_))
  
rm(consumption_quantiles, urban_quantiles, rural_quantiles, svy_hh_consumption)

#-------------------------------------------------------------------------------

# Join all relevant variables to hh_info: 
hh_info <- hh_info %>% 
  left_join(hh_consumption %>% 
              dplyr::select(hhid, sep_quintile, res_quintile),
            by = "hhid") %>% 
  dplyr::select(hhid, adm1, adm2, res, sep_quintile, res_quintile, age_head, 
                sex_head, year, month)

#-------------------------------------------------------------------------------
# add household size and head occupation
# household size
hhsize <- import(here::here("data-TNPS", "raw_data", "consumption_real_y5.csv")) %>% 
  select(y5_hhid, hhsize) %>% 
  rename(hhid = y5_hhid)
hh_info <- left_join(hh_info, hhsize, by = "hhid")

# head occupation
hh_demographics <- read_csv("data-TNPS/raw_data/hh_sec_b.csv") %>% 
  select(y5_hhid, indidy5, hh_b05, hh_b11) %>% # hh_b05 - relationship with head (head is coded as 1)
  rename(hhid = y5_hhid,
         relation = hh_b05,
         work = hh_b11) # hh_b11 - main occupation
hh_education <- read_csv("data-TNPS/raw_data/hh_sec_c.csv") %>% 
  select(y5_hhid, indidy5, hh_c07) %>% #hh_c07 - highest grade completed
  rename(hhid =y5_hhid,
         educ = hh_c07)
hheducwork <- left_join(hh_demographics, hh_education, by = c("hhid", "indidy5")) %>% 
  filter(relation == 1) %>% 
  select(-c(indidy5, relation)) %>% 
  rename(work_head = work,
         educ_head = educ) %>% 
  mutate(educ_head2 = case_when(
    (educ_head <= 17) ~ 1,
    (educ_head == 18) ~ 2,
    TRUE ~ 3))
hh_info <- left_join(hh_info, hheducwork, by = "hhid")

# Write data
write_csv(hh_info, "data-TNPS/processed_data/tza_nps2021_hh_info.csv")

################################################################################
################################ END OF SCRIPT #################################
################################################################################
