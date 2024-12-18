################################################################################
########### SCRIPT FOR EXTRACTING AND CLEANING CONSUMPTION QUANTITIES ##########
################################################################################

# Author: Mo Osman
# Date created: 17-06-2024
# Last edited: 14-08-2024
# Modified: Rie Goto in September-November 2024

# Tanzania NPS Wave 5:
# https://microdata.worldbank.org/index.php/catalog/5639/data-dictionary

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "haven", "stringr", "readxl", "ggplot2",
                 "gtExtras")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# READ IN DATA - hh_sec_ja1.csv (4290 HHs): 

# Get edible portions: 
edible_portion <- read_excel("data-TNPS/raw_data/TNPSW5_averages.xlsx", 
                      sheet = "wave5_NCT_v1.0") %>% 
  dplyr::select(itemcode, Edible_factor_in_FCT) %>% 
  rename(item_code = itemcode,
         edible_portion = Edible_factor_in_FCT) %>% 
  mutate(edible_portion = as.numeric(edible_portion))

# Read in food items dictionary and food groups: 
food_items <- read_csv("data-TNPS/raw_data/food-id_groups_ja1_rg.csv") %>% 
  rename(item_code = itemcode)

# Read in unit (1-4) conversion factors: 
unit_conv <- read_csv("data-TNPS/raw_data/unitconv.csv") %>%
  dplyr::select(itemcode, cons_unit, conv_fac) %>% 
  rename(item_code = itemcode,
         unit = cons_unit) 

# Read in new food consumption module: 
tza_food_consumption_ja1 <- read_csv("data-TNPS/raw_data/hh_sec_ja1.csv") %>% 
  dplyr::select(y5_hhid, 
                itemcode, 
                hh_ja01, # Did household consume? 
                hh_ja02_1, # Quantity consumed (UNIT)
                hh_ja02_2, # Quantity consumed (QUANTITY)
                hh_ja02b_1, # How many kg/g/l/ml was the UNIT (estimated)
                hh_ja02b_2, # How many kg/g/l/ml was the QUANTITY (estimated)
                hh_ja09_1, # UNIT (weighted)
                hh_ja09_2) %>% # QUANTITY (weighed)
  rename(hhid = y5_hhid, 
         item_code = itemcode,
         consumed = hh_ja01,
         unit = hh_ja02_1,
         quantity = hh_ja02_2,
         est_metric_unit = hh_ja02b_1, 
         est_metric_quantity = hh_ja02b_2,
         weighed_unit = hh_ja09_1,
         weighed_quantity = hh_ja09_2) %>% 
  filter(consumed == 1) %>% # Only keep entries where household consumed
  select(-consumed) %>% # Drop the consumed column
  left_join(food_items, by = "item_code") %>% # Join food item names
  left_join(edible_portion, by = "item_code") %>%  # Join edible portion ratio
  left_join(unit_conv, by = c("item_code", "unit")) %>%  # Join unit conversion factors
  select(hhid, item_code, itemname, everything())  # Re-order variables

df <- tza_food_consumption_ja1 %>%
  summarise(n = n_distinct(hhid)) # 4103 hhs

rm(edible_portion, food_items)


#-------------------------------------------------------------------------------

# READ IN DATA - hh_sec_j1.csv: 
# Some households used the old food consumption module (hh_sec_j1, 419 HHs) - therefore need to 
# read this data in too for pre-processing:

# Get edible portions:
edible_portionj1 <- read_csv("data-TNPS/raw_data/ediblep_j1.csv") %>% 
  dplyr::select(itemcode, mean_EP) %>% 
  rename(item_code = itemcode,
         edible_portion = mean_EP)

# Read in unit conversion factors: 
# add value for unit pieces (unit 5, pieces) in unitconv_ji.csv

unit_convj1 <- read_csv("data-TNPS/raw_data/unitconv_j1.csv") %>% 
  dplyr::select(itemcode, cons_unit, conv_fac) %>% 
  rename(item_code = itemcode,
         unit = cons_unit)
# add weight in pieces in 15 food items
# 807 eggs (70g), 110 buns (50g), 502 coconuts (780g), 702 citrus fruits (115g), 703 mangoes (200g),
# 701 ripen banana (100g), 302 sweets (50g), 109 bread (100g), 201 cassava fresh (336g), 203 sweet potatoes (457g),
# 205 irish potatoes (139g), 206 cooking banana (161g), 704 sugar cane (1468g), 103 maize cob (300g), 804 chicken (531g)
item_code <- c(807, 110, 502, 702, 703, 701, 302, 109, 201, 203, 205, 206, 704, 103, 804)
unit <- 5
conv_fac <- c(70, 50, 780, 115, 200, 100, 50, 100, 336, 457, 139, 161, 1468, 300, 531)
unit_convj1_pieces  <- data.frame(item_code, unit, conv_fac)
print(unit_convj1_pieces)

# add the weights in pieces
unit_convj1_2 <- rbind(unit_convj1, unit_convj1_pieces) 

# Read in food_items dictionary and food groups: 
food_itemsj1 <- read_csv("data-TNPS/raw_data/food-id_groups_j1_rg.csv") %>% 
  rename(item_code = itemcode)

# Read in food consumption module: 
tza_food_consumption_j1 <- read_csv("data-TNPS/raw_data/hh_sec_j1.csv") %>% 
  dplyr::select(y5_hhid, 
                itemcode,
                hh_j01, # Did household consume? 
                hh_j02_1, # Quantity consumed (UNIT) 
                hh_j02_2) %>%  # Quantity consumed (QUANTITY) 
  rename(hhid = y5_hhid,
         item_code = itemcode,
         consumed = hh_j01,
         unit = hh_j02_1,
         quantity = hh_j02_2) %>%
  filter(consumed == 1) %>%  # Only keep entries where household consumed
  select(-consumed) %>% # Drop the consumed column
  left_join(food_itemsj1, by = "item_code") %>% # Join food item names
  left_join(edible_portionj1, by = "item_code") %>%  # Join edible portion ratio
  left_join(unit_convj1_2, by = c("item_code","unit")) %>%  # Join unit conversion factors
  select(hhid, item_code, itemname, everything())  # Re-order variables

df <- tza_food_consumption_j1 %>%
  summarise(n = n_distinct(hhid)) # 4103 hhs

#-------------------------------------------------------------------------------

# CALCULATE CONSUMPTION QUANTITIES FOR MODULE JA1

# Initially calculate consumption quantities for households reporting consumption
# in metric units (units 1-4):

tza_food_consumption_ja1 <- tza_food_consumption_ja1 %>% 
  mutate(quantity_g = case_when(
    unit == 1 | unit == 2 | unit == 3 | unit == 4 ~ quantity * conv_fac * edible_portion,
    TRUE ~ NA_real_)) %>% 
  dplyr::select(-unit, -quantity, -conv_fac) # Drop the unit and quantity columns

# When other units were reported, food items were either weighed, or if not, the
# metric quantities were estimated:

# Use actual weights preferentially
tza_food_consumption_ja1 <- tza_food_consumption_ja1 %>% 
  rename(unit = weighed_unit) %>% 
  left_join(unit_conv, by = c("item_code", "unit")) %>% 
  mutate(quantity_g = case_when(
    !is.na(weighed_quantity) & is.na(quantity_g) ~ weighed_quantity * conv_fac * edible_portion,
    TRUE ~ quantity_g
  )) %>% 
  dplyr::select(-unit, -weighed_quantity, -conv_fac)

# Then finally if neither of the above were available, use estimated metric quantities: 
tza_food_consumption_ja1 <- tza_food_consumption_ja1 %>% 
  rename(unit = est_metric_unit) %>%
  left_join(unit_conv, by = c("item_code", "unit")) %>%
  mutate(quantity_g = case_when(
    !is.na(est_metric_quantity) & is.na(quantity_g) ~ est_metric_quantity * conv_fac * edible_portion,
    TRUE ~ quantity_g
  )) %>%
  dplyr::select(-unit, -est_metric_quantity, -conv_fac, -edible_portion)

rm(unit_conv)

#-------------------------------------------------------------------------------

# CALCULATE CONSUMPTION QUANTITIES FOR MODULE J1: 

j1_na <- tza_food_consumption_j1 %>% 
  filter(is.na(conv_fac)) %>% 
  dplyr::select(hhid) %>% 
  distinct()

# There were 40 households that had insufficient data to calculate consumption
# quantities, therefore remove these households from the dataset:
tza_food_consumption_j1 <- tza_food_consumption_j1 %>% 
  anti_join(j1_na, by = "hhid")

tza_food_consumption_j1 <- tza_food_consumption_j1 %>% 
  mutate(quantity_g = quantity * conv_fac * edible_portion) %>%
  dplyr::select(hhid, item_code, itemname, groupcode, groupname, quantity_g)

# Append the two datasets together: quantity_g is total household food consumption for 7 days
tza_food_consumption2 <- bind_rows(tza_food_consumption_ja1, tza_food_consumption_j1)

hhid_count <- tza_food_consumption2 %>%
  summarise(total_hhids = n_distinct(hhid)) # total 4469 HHs (4709-4469=240HHs; 200hhs did not answer the question, and 40hhs had insufficient data)

#-------------------------------------------------------------------------------

# WRITE DATA: quantity_g = total estimated household food consumption in grams in last 7 days

write_csv(tza_food_consumption2, "data-TNPS/processed_data/tza_food_consumption_2.csv")

rm(list = ls())

################################################################################
################################# END OF SCRIPT ################################
################################################################################






