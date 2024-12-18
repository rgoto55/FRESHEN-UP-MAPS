################################################################################
################ SCRIPT FOR AFE CALCULATION - TANZANIA NPS WAVE 5 ##############
################################################################################

# Author: Mo Osman
# Date created: 17-06-2024
# Last edited: 14-08-2024
# Modified: Rie Goto 

# In this script, I will calculate AFE for households in the Tanzania NPS-5 survey:
# https://microdata.worldbank.org/index.php/catalog/5639/data-dictionary

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "haven")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

# Specify rounding settings:
options(scipen = 10, digits = 3)

# List of assumptions made for AFE calculations: 

# 1 AFE = 2291kcal/day as calculated using the FAO/WHO/UNU (2004) equations for a 55kg female

# PAL = 1.76 (active/moderately active lifestyle) - reference: table 5.1 FAO/WHO/UNU (2004)

# Average men's weight = 65kg (Assumed)
# Average women's weight = 55kg (Assumed)

# Average energy cost of lactation = 505kcal for first 6-months of lactation
# 460kcal after 6-months of lactation (Chapter 7 in FAO/WHO/UNU (2004))

# Average total energy cost of a preganancy = 77,100kcal (reference: table 6.3 FAO/WHO/UNU (2004))
# Average length of a pregnancy = 280days
# Therefore average daily energy cost during pregnancy = 275kcal/day
# There is no data in the NPS-5 to determine pregnancy trimester

#-------------------------------------------------------------------------------

# READ IN DEMOGRAPHIC AND ANTHROPOMETRIC DATA:

anthropometric <- read_csv("data-TNPS/raw_data/hh_sec_v.csv") %>%
  rename(weight = hh_v05, height = hh_v06) %>%
  select(y5_hhid, indidy5, weight)

demographic <- read_csv("data-TNPS/raw_data/hh_sec_b.csv") %>%
  rename(sex = hh_b02,
         age = hh_b04,
         eat7d = hh_b07, 
         moid = hh_b15_2) %>% # Mother ID
  select(y5_hhid, indidy5, sex, age, eat7d, moid) %>% 
  filter(eat7d == 1) # individuals eating at home in last 7-days

#-------------------------------------------------------------------------------

# IDENTIFY INDIVUDUALS FALLING INTO EACH DEMOGRAPHIC GROUP:

# CHILDREN UNDER 2: 
u2 <- demographic %>% 
  filter(age < 2)

# Get age in months: 
u2_age <- read_csv("data-TNPS/raw_data/npsy5.child.anthro.csv")

u2 <- u2 %>% 
  left_join(u2_age, by= c('y5_hhid', 'indidy5')) %>%
  select(y5_hhid, indidy5, age_months, age) %>% 
  # Many have missing age in-months, if so, calculate using age in years:
  mutate(age_months = ifelse(is.na(age_months), age*12, age_months))

rm(u2_age)

# LACTATING WOMEN: 
demographic_lact <- demographic %>%
  left_join(u2, by = c('y5_hhid', 'indidy5')) %>%
  dplyr::select(y5_hhid, indidy5, sex, age_months, moid, eat7d) %>% 
  # For children under 2, mark the mother as lactating:
  mutate(lact_m = ifelse(age_months <= 23, moid, NA)) %>% 
  mutate(infant_age = ifelse(age_months <= 6, "<6months",
                             ifelse(age_months > 6, ">6months", NA))) %>%
  filter(!is.na(lact_m)) %>% 
  dplyr::select(y5_hhid, lact_m, infant_age) %>% 
  rename(indidy5 = lact_m) %>% 
  # Note that some mothers may have more than one child under 2, therefore 
  # de-duplicate and indicate the number of children in each age category: 
  group_by(y5_hhid, indidy5) %>%
  summarise(under6_months = sum(infant_age == "<6months"),
            over6_months = sum(infant_age == ">6months")) %>% 
  # Join rest of demographic data that is required: 
  left_join(demographic %>% dplyr::select(y5_hhid, indidy5, sex, age),
            by = c('y5_hhid', 'indidy5')) %>%
  filter(age <= 53) # Specify max age as 53.

# PREGNANT WOMEN: 
# Read in pregnancy data:
pregnant <- read_csv("data-TNPS/raw_data/hh_sec_d.csv") %>% 
  dplyr::select(y5_hhid, indidy5, hh_d36a) %>% 
  rename(pregnant = hh_d36a) %>% 
  filter(pregnant == 1) 
  
# Attach demographic data:
demographic_preg <- pregnant %>% 
  inner_join(demographic %>% dplyr::select(y5_hhid, indidy5, age, sex), 
            by = c("y5_hhid", "indidy5")) %>% 
  dplyr::select(-c("pregnant")) 

rm(pregnant)

# DEMOGRAPHIC ALL OTHERS: 
demographic_others <- demographic %>% 
  anti_join(u2, by = c("y5_hhid", "indidy5")) %>% 
  anti_join(demographic_lact, by = c("y5_hhid", "indidy5")) %>% 
  anti_join(demographic_preg, by = c("y5_hhid", "indidy5"))

#-------------------------------------------------------------------------------

# ESTIMATE ENERGY REQUIREMENTS AND AFE's FOR THOSE AGED < 24-months:

# Assign energy requirements for different age groups - SOURCE: 
# Book - Complementary feeding of young Children in Developing Countries, 
# Table 10, page 51.
# WHO 1998, edited by Kenneth Brown, Kathryn Dewey, Lindsay Allen

u2 <- u2 %>%
  mutate(kcalreq = case_when(
    age_months <= 2 ~ 0,   # only breast feeding - no food intake
    age_months >= 3 & age_months <= 5 ~ 76,  # energy from food is 76kcal per day for 3-5 months of age
    age_months >= 6 & age_months <= 8 ~ 269,  # 269kcal per day for 6-8 months of age
    age_months >= 9 & age_months <= 11 ~ 451,   # 451kcal per day for 9-11 months of age
    age_months >= 12 ~ 746)) # 746kcal per day for those aged 12-months - 2years

# AFE calculation for children below 2 years old:
afeu2 <- u2 %>%
  mutate(afe = kcalreq/2291) %>% # 1AFE = 2291kcal
  select(y5_hhid, indidy5, afe)

rm(u2)

#-------------------------------------------------------------------------------

# ESTIMATING TEE FOR THOSE AGED >2YEARS:

tee_calc <- left_join(anthropometric, demographic, by= c('y5_hhid', 'indidy5')) %>%
  group_by(sex, age) %>% # Calculate mean weight using available data for under 15's
  summarise(meanw = mean(weight, na.rm=TRUE)) %>%
  mutate(meanw = ifelse(age >= 15 & sex == 1, 65, # Assumed average weight of men = 65kg
                        ifelse(age >= 15 & sex == 2, 55, meanw))) %>% # Assumed average weight of women = 55kg
  # Remove under 2's as these have already been calculated above: 
  filter(age >= 2) %>% 
  # Set a PAL at 1.76 for all over 18's:
  mutate(PAL = ifelse(age > 18, 1.76, NA))

# TEE FOR CHILDREN (2-18 years old) (formula from table 4.2 and 4.3 in FAO/WHO/UNU (2004)):
tee_calc <- tee_calc %>% 
  mutate(TEE = ifelse(sex == 1 & age <= 18, 1.298 + (0.265 * meanw) - 0.0011 * (meanw^2), 
                      ifelse(sex == 2 & age <= 18, 1.102 + (0.273 * meanw) - 0.0019 * (meanw^2), NA))) %>% 
  mutate(TEE = TEE * 239.005736) # convert to kcal/day: 

# TEE FOR ADULTS (Formula from table 5.2 in FAO/WHO/UNU (2004)):
tee_calc <- tee_calc %>% 
  mutate(BMR = case_when( # Firstly need to calculate BMR for different age categories:
    sex == 1 & age >18 & age <= 30 ~ 15.057 * meanw + 692.2,
    sex == 1 & age >30 & age < 60 ~ 11.472 * meanw + 873.1,
    sex == 1 & age >= 60 ~ 11.711 * meanw + 587.7,
    sex == 2 & age >18 & age <= 30 ~ 14.818 * meanw + 486.6,
    sex == 2 & age >30 & age < 60 ~ 8.126 * meanw + 845.6, 
    sex == 2 & age >= 60 ~ 9.082 * meanw + 658.5,
    TRUE ~ NA)) %>% # Get TEE by multiplying BMR by PAL for over 18's: 
  mutate(TEE = ifelse(age > 18, BMR * PAL, TEE)) # 

#-------------------------------------------------------------------------------

# ENERGY REQUIREMENT FOR LACTATING WOMEN:

afe_lact <- demographic_lact %>% 
  left_join(tee_calc %>% dplyr::select(sex, age, TEE),
            by = c("sex", "age")) %>%
  mutate (TEE = TEE + under6_months*505 + over6_months*460) %>%
  mutate(afe = TEE / 2291) %>% # AFE = Total energy expenditure / 2291kcal/day
  dplyr::select(y5_hhid, indidy5, afe)

rm(demographic_lact)

#-------------------------------------------------------------------------------

# ENERGY REQUIREMENTS FOR PREGNANT WOMEN: 

afe_preg <- demographic_preg %>% 
  left_join(tee_calc %>% dplyr::select(sex, age, TEE),
            by = c("sex", "age")) %>% 
  mutate(TEE = TEE + 275) %>% # Usual energy requirements +275kcal/day: 
  mutate(afe = TEE / 2291) %>%  # AFE = Total energy expenditure / 2291kcal/day
  dplyr::select(y5_hhid, indidy5, afe)

#-------------------------------------------------------------------------------

# CALCULATE AFE FOR ALL OTHER INDIVIDUALS: 
afe_other <- demographic_others %>% 
  left_join(tee_calc %>% dplyr::select(age, sex, TEE), 
            by = c("age", "sex")) %>% 
  # Calculate AFE:
  mutate(afe = TEE / 2291) %>%  # AFE = Total energy expenditure / 2291kcal/day
  dplyr::select(y5_hhid, indidy5, afe)

#-------------------------------------------------------------------------------

# CALCULATE TOTAL AFE PER HOUSEHOLD: 

hh_afe <- bind_rows(afeu2, afe_lact, afe_preg, afe_other) %>% 
  group_by(y5_hhid) %>% 
  summarise(afe = sum(afe, na.rm = TRUE))

rm(list = ls()[!ls() %in% c("hh_afe")])

# WRITE DATA: afe = afe in the households

write_csv(hh_afe, "data-TNPS/processed_data/hh_afe.csv")


################################################################################
############################### END OF SCRIPT ##################################
################################################################################