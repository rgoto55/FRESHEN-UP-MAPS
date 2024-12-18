### FRESH baseline data ####
### R Goto
# Created: November-December 2024

# setting
pacman::p_load(tidyverse, knitr, here, renv, rio, haven, survey, srvyr, summarytools, sas7bdat)

# open files
mA1 <- read_dta(here::here("data-FRESH","mA1_final.dta"))
mA2 <- read_dta(here::here("data-FRESH","mA2_final.dta"))
wealth <- read_dta(here::here("data-FRESH","wealth.dta"))
fresh24hR <- read_sas(here::here("data-FRESH", "fresh_24hr_2.sas7bdat"))
freshHH <- read_dta(here::here("data-FRESH","TZ_FRESH_BL_consumption_quantities.dta"))


# File organisation
# Module A1 - Household definition
# variables
HHID # HH ID
A1_2a # region
A1_2b # district
A1_3a # date of visit

names(mA1)
mA1 <- mA1 %>% 
  select(hhid, region, district, day_visit)

view(dfSummary(mA1))

# Module A2 - Household roster
# variables
A2_ID # Member ID
A2_2 # relationship with HH head
A2_3 # sex
A2_4 # age
A2_5 # education
A2_6 # primary activity in the last 12 months

names(mA2)
view(dfSummary(mA2))

# Module H - HCES data
# variables
HHID # HH ID
Food item/ code # 139 food items (code?)
consumption in gram # household food consumption with adjusting liquid weight and edible portion

# Module DR - 24hR
# variables
HHID # HH ID
A2_ID # Member ID
Food items # code?
Usual intake # estimated usual intake in grams??

names(fresh24hR)
view(dfSummary(fresh24hR))
F24_food <- fresh24hR %>% 
  select(hhid, FoodCode_ingr, Food_amt, ENERGY_Kcal) %>% 
  rename(itemcode = FoodCode_ingr,
         foodamt = Food_amt,
         energy_per100g = ENERGY_Kcal) %>% # kcal per 100g
  mutate(energy_percons = foodamt*energy_per100g/100)

F24foodlist <- read.csv(here::here("data-FRESH","Foodlist_24hR_Rie_food groups.csv"))

F24_food <- left_join(F24_food, F24foodlist, by = "itemcode")

# write csv
write_csv(F24_food, "dada-FRESH/F24_food.csv")

ggplot(F24_food, aes(x=as.factor(itemname), y=foodamt)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("items") + ylab("consumption in grams per AFE") +
  coord_flip()

# get summary

F24_food_sum <- F24_food %>%
  group_by(itemname) %>%
  summarise(median = median(foodamt, na.rm = TRUE),
            qt25 = quantile(foodamt, c(0.25), na.rm=TRUE),
            qt75 = quantile(foodamt, c(0.75), na.rm=TRUE),
            n = n_distinct(foodamt, na.rm=TRUE)) %>% 
  arrange(desc(median))

# get summary of energy kcal
options(scipen = 6, digits=3)
F24_energy <- F24_food %>%
  group_by(hhid) %>%
  summarise(energy_total = sum(energy_percons))

F24_energy_sum <- F24_energy %>% 
  summarise(median = median(energy_total, na.rm = TRUE),
            qt25 = quantile(energy_total, c(0.25), na.rm=TRUE),
            qt75 = quantile(energy_total, c(0.75), na.rm=TRUE))

# write csv
write_csv(F24_energy_sum, "data-FRESH/F24_energy_sum.csv")
write_csv(F24_food_sum, "data-FRESH/F24_food_sum.csv")

#-------------------------------------------------------------------------------
##### FOOD GROUPS #####
# adjust dry weights

# including the conversion factor of dry foods: 
# 254 milk powder, full cream - [(food in g)*0.81] + food in g
# 313 fish, small, dried, fresh water - [(food in g)*0.625] + food in g
# 319 fish, smoked, dried-AP - [(food in g)*0.6925] + food in g

F24_food <- read.csv(here::here("data-FRESH","F24_food.csv"))

F24_foodgrp <- F24_food %>%
  mutate(foodamtg = ifelse(itemcode == 254, (foodamt * 0.81) + foodamt, foodamt)) %>% 
  mutate(foodamtg = ifelse(itemcode == 313, (foodamt * 0.625) + foodamt, foodamt)) %>% 
  mutate(foodamtg = ifelse(itemcode == 319, (foodamt * 0.6925) + foodamt, foodamt))

F24_foodgrp <- F24_food %>%
  mutate(foodamtg = case_when(
    (itemcode == 254) ~ (foodamt * 0.81) + foodamt, 
    (itemcode == 313) ~ (foodamt * 0.625) + foodamt, 
    (itemcode == 319) ~ (foodamt * 0.6925) + foodamt, 
    TRUE ~ foodamt)) 

# get summary
options(scipen = 6, digits=3)

F24_foodgrp_sum <- F24_foodgrp %>%
  group_by(hhid, groupname, groupcode) %>%
  summarise(grpcons = sum(foodamtg), .groups = "drop")

F24_foodgrp_sum <- F24_foodgrp_sum %>% 
  group_by(groupname, groupcode) %>% 
  summarise(median = median(grpcons, na.rm = TRUE),
            qt25 = quantile(grpcons, c(0.25), na.rm=TRUE),
            qt75 = quantile(grpcons, c(0.75), na.rm=TRUE),
            n = n_distinct(grpcons, na.rm=TRUE)) %>% 
  arrange(groupcode)

# get F&V and F&V <400g
F24_foodgrpFV <- F24_foodgrp %>% 
  filter(groupcode == 4 | groupcode == 5) %>% 
  group_by(hhid) %>% 
  summarise(fvamtgFV = sum(foodamtg))

F24_foodgrpFVcons <- F24_foodgrpFV %>% 
  mutate(fvcons = case_when(is.na(fvamtgFV) ~ 0, fvamtgFV > 0 ~ 1, TRUE ~ 0)) %>% 
  mutate(fv400gcons = case_when(is.na(fvamtgFV) ~ 0, fvamtgFV > 400 ~ 1, TRUE ~ 0))

F24_foodgrpFVcons_sum <- F24_foodgrpFVcons %>% 
  summarise(median = median(fvamtgFV, na.rm = TRUE),
            qt25 = quantile(fvamtgFV, c(0.25), na.rm=TRUE),
            qt75 = quantile(fvamtgFV, c(0.75), na.rm=TRUE))

view(dfSummary(F24_foodgrpFVcons)) # <400g n = 1741 (70.6%)

# write csv
write_csv(F24_foodgrp_sum, "FRESH/F24consumption_foodgroup.csv")

#-------------------------------------------------------------------------------


