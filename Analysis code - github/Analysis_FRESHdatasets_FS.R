### FRESH baseline data ####
### R Goto
# Created: November 2024

# setting
pacman::p_load(tidyverse, knitr, here, renv, rio, haven, survey, srvyr, summarytools, sas7bdat)

# open files
mA1 <- read_dta(here::here("FRESH","mA1_final.dta"))
mA2 <- read_dta(here::here("FRESH","mA2_final.dta"))
wealth <- read_dta(here::here("FRESH","wealth.dta"))
fresh24hR <- read_sas(here::here("FRESH", "fresh_24hr_2.sas7bdat"))

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
  select(hhid, FoodCode_ingr, Food_amt) %>% 
  rename(itemcode = FoodCode_ingr,
         foodamt = Food_amt)

F24foodlist <- read.csv(here::here("FRESH","Foodlist_24hR_Rie_food groups.csv"))

F24_food <- left_join(F24_food, F24foodlist, by = "itemcode")

# write csv
write_csv(F24_food, "FRESH/F24_food.csv")

ggplot(F24_food, aes(x=as.factor(itemname), y=foodamt)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("items") + ylab("consumption in grams per AFE") +
  coord_flip()

# get summary
options(scipen = 6, digits=3)
F24_food_sum <- F24_food %>%
  group_by(itemname) %>%
  summarise(median = median(foodamt, na.rm = TRUE),
            qt25 = quantile(foodamt, c(0.25), na.rm=TRUE),
            qt75 = quantile(foodamt, c(0.75), na.rm=TRUE),
            n = n_distinct(foodamt, na.rm=TRUE)) %>% 
  arrange(desc(median))

# write csv
write_csv(F24_food_sum, "FRESH/F24_food_sum.csv")

#-------------------------------------------------------------------------------
##### FOOD GROUPS #####
# adjust dry weights

# including the conversion factor of dry foods: 
# 254 milk powder, full cream - [(food in g)*0.81] + food in g
# 313 fish, small, dried, fresh water - [(food in g)*0.625] + food in g
# 319 fish, smoked, dried-AP - [(food in g)*0.6925] + food in g

F24_food <- read.csv(here::here("FRESH","F24_food.csv"))

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

# write csv
write_csv(F24_foodgrp_sum, "FRESH/F24consumption_foodgroup.csv")
