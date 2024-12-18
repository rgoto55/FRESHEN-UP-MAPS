################################################################################
################### SCRIPT FOR COMPILING BASE MODEL - TZA NPSW5 ################
################################################################################

# Author: Mo Osman
# Date created: 25-07-2024
# Last edited: 14-08-2024
# Modified: Rie Goto (September-November 2024)

# In this script, I will compile the pre-processed data to produce the base 
# apparent intake model for Tanzania NPS Wave 5:
# https://microdata.worldbank.org/index.php/catalog/5639/data-dictionary

# INSTALL AND LOAD PACKAGES:

rq_packages <- c("readr", "tidyverse", "here")

installed_packages <- rq_packages %in% rownames(installed.packages())
if (any(installed_packages == FALSE)) {
  install.packages(rq_packages[!installed_packages])
}

lapply(rq_packages, require, character.only = T)

rm(list= c("rq_packages", "installed_packages"))

#-------------------------------------------------------------------------------

# CLEAN DATA:

# Firstly clean data by filtering out households that are reporting implausible
# consumption quantities per day/AFE:

# Read in food consumption data and AFE's: 
tza_food_consumption2 <- read_csv("data-TNPS/processed_data/tza_food_consumption_2.csv")

hhid_count <- tza_food_consumption2 %>%
  summarise(total_hhids = n_distinct(hhid))

hh_afe <- read_csv("processed_data/hh_afe.csv") %>% # n=4613
  rename(hhid = y5_hhid)

# Merge data and divide consumption quantities by AFE per day: 
tza_food_consumption2 <- tza_food_consumption2 %>% 
  left_join(hh_afe, by = "hhid") %>% 
  mutate(quantity_gafed = quantity_g/afe/7)

# check distribution
ggplot(tza_food_consumption2, aes(x=as.factor(itemname), y=quantity_gafed)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("items") + ylab("consumption in grams per AFE") +
  coord_flip()

# FILTER OUT EXTREME OUTLIERS:

# To do this, firstly filter out extreme values by applying log10 trans:
tza_food_consumption2 <- tza_food_consumption2 %>% 
  mutate(log_quantity_gafed = log10(quantity_gafed))

# Generate cut points for values that are >+3SDs from the mean intake of each food item:
quant_cutpoints <- tza_food_consumption2 %>% 
  group_by(item_code) %>% 
  summarise(mean_log = mean(log_quantity_gafed, na.rm = TRUE),
            sd_log = sd(log_quantity_gafed, na.rm = TRUE)) %>% 
  mutate(upper_cut = mean_log + 3*sd_log) %>% 
  dplyr::select(item_code, upper_cut)

# Apply the cut points to the data, replacing quantities above the cut-point with NA: 
tza_food_consumption3 <- tza_food_consumption2 %>% 
  left_join(quant_cutpoints, by = "item_code") %>% 
  mutate(gafed = case_when(
    log_quantity_gafed > upper_cut ~ NA_real_,
    TRUE ~ quantity_gafed
  )) %>% 
  dplyr::select(-log_quantity_gafed, -upper_cut)

# Replace NA values with median reported intake: 
tza_food_consumption4 <- tza_food_consumption3 %>% 
  group_by(item_code) %>% 
  mutate(gafed = ifelse(is.na(gafed), median(gafed, na.rm = TRUE), gafed))

tza_food_consumption5 <- tza_food_consumption4 %>% 
  select(hhid, item_code, itemname, groupcode, groupname, gafed)

rm(list = ls())

#-------------------------------------------------------------------------------

# Write data:
write_csv(tza_food_consumption5, "processed_data/tza_food_consumption_5.csv")
#-------------------------------------------------------------------------------

################################################################################
############################### END OF SCRIPT ##################################
################################################################################
