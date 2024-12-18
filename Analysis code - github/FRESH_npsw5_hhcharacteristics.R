################################################################################
################### SCRIPT FOR TNPSW5 ##########################################
################################################################################

# Author: Rie Goto
# Date created: September-December 2024

# setting
install.packages("pacman")
library(pacman)

pacman::p_load(tidyverse, knitr, here, renv, rio, survey, srvyr, summarytools)

#-------------------------------------------------------------------------------
### HOUSEHOLD CHARACTERISTICS ### - Table 2

hh_info <- import(here::here("data-TNPS", "processed_data","tza_nps2021_hh_info.csv")) 
hh4469 <- import(here::here("data-TNPS", "processed_data","hh4469.csv")) 

# count hh in Arusha and Kilimanjaro
names(hh_info)
df <- left_join(hh4469, hh_info, by = "hhid") %>% 
  group_by(adm1) %>% 
  summarise(reg = n_distinct(hhid)) # Arusha 221, Kilimanjaro 149 HHs

#-------------------------------------------------------------------------------
### hh size, female headed hh, education of hh head, occupation of hh head
# national
hh_weight <- import(here::here("data-TNPS", "raw_data", "consumption_real_y5.csv")) %>%
  select(y5_hhid, booster, cluster, strata, hhweight2) %>% 
  rename(hhid = y5_hhid)

hh_infow <- left_join(hh_info, hh_weight, by = "hhid")

hh_infow <- hh_infow %>%
  as_survey_design(ids = cluster, 
                   weights = hhweight2, 
                   strata = strata,
                   nest=TRUE)

# hh size
hh_sum_hhsize <- hh_infow %>%
  summarise(
    mean = survey_mean(hhsize, na.rm = TRUE),
    sd = survey_sd(hhsize, na.rm = TRUE))

# female headed household
hh_sum_fhead <- hh_infow %>% 
  group_by(sex_head) %>% 
  summarise(fhead = survey_prop())

# education of hh head
hh_sum_ehead <- hh_infow %>%
  group_by(educ_head2) %>% 
  summarise(ehead = survey_prop())

# head occupation
hh_sum_whead <- hh_infow %>%
  group_by(work_head) %>% 
  summarise(whead = survey_prop())

## Arusha and Kilimanjaro regions

# hh size
hh_sum_hhsize_ak <- hh_infow %>%
  filter(adm1 == 2 | adm1 ==3) %>% 
  summarise(
    mean = survey_mean(hhsize, na.rm = TRUE),
    sd = survey_sd(hhsize, na.rm = TRUE))

# female headed household
hh_sum_fhead_ak <- hh_infow %>% 
  filter(adm1 == 2 | adm1 ==3) %>% 
  group_by(sex_head) %>% 
  summarise(fhead = survey_prop())

# education of hh head
hh_sum_ehead_ak <- hh_infow %>%
  filter(adm1 == 2 | adm1 ==3) %>% 
  group_by(educ_head2) %>% 
  summarise(ehead = survey_prop())

# head occupation
hh_sum_whead_ak <- hh_infow %>%
  filter(adm1 == 2 | adm1 ==3) %>% 
  group_by(work_head) %>% 
  summarise(whead = survey_prop())

#-------------------------------------------------------------------------------
