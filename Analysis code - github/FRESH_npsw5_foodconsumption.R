################################################################################
################### SCRIPT FOR TNPSW5 ##########################################
################################################################################

# Author: Rie Goto
# Date created: September-December 2024

# setting
pacman::p_load(tidyverse, knitr, here, renv, rio, survey, srvyr, summarytools)

### food consumption per AFE per day
food_cons <- import(here::here("data-TNPS", "processed_data","tza_food_consumption_5.csv")) %>% 
  rename(itemcode = item_code) %>% 
  mutate(cons = 1)

# count n of households
hhid_count <- food_cons %>%
  summarise(total_hhids = n_distinct(hhid)) #4469hhs with valid food consumption in g/AFE/day

# make a list of 4469 hhs
hh4469 <- food_cons %>% 
  select(hhid, cons) %>% 
  group_by(hhid) %>% 
  count(cons) %>% 
  select(-c(cons, n))

#-------------------------------------------------------------------------------
# get summary of each food consumption in g/AFE/day (unweighted)
options(scipen = 6, digits=3)
cons_sum <- food_cons %>%
  group_by(itemcode, itemname) %>%
  summarise(median = median(gafed, na.rm = TRUE),
            qt25 = quantile(gafed, c(0.25), na.rm=TRUE),
            qt75 = quantile(gafed, c(0.75), na.rm=TRUE),
            n = n_distinct(gafed, na.rm=TRUE)) %>% 
  arrange(desc(median))

# plot to check distributions by food items
ggplot(food_cons, aes(x=as.factor(itemname), y=gafed)) + 
  geom_boxplot(fill="slateblue", alpha=0.2) + 
  xlab("items") + ylab("consumption in grams per AFE") +
  coord_flip()

# write csv
write_csv(cons_sum, "data-TNPS/processed_data/consumption_86foods.csv")
write_csv(hh4469, "data-TNPS/processed_data/hh4469.csv")

#-------------------------------------------------------------------------------
### FOOD CONSUMPTION PER DAY PER AFE BY FOOD ITEMS ### Supple Tables 3-4
# get data
food_cons <- import(here::here("data-TNPS", "processed_data","tza_food_consumption_5.csv")) %>% 
  rename(itemcode = item_code) %>% 
  mutate(cons = 1)

hh_weight <- import(here::here("data-TNPS", "raw_data", "consumption_real_y5.csv")) %>%
  select(y5_hhid, booster, cluster, strata, hhweight2, region) %>% 
  rename(hhid = y5_hhid)

# weighting
food_cons <- left_join(food_cons, hh_weight, by = "hhid")

food_consw <- food_cons %>%
  as_survey_design(ids = cluster, 
                   weights = hhweight2, 
                   strata = strata,
                   nest=TRUE)
# estimated the amount of food consumption by food items
# in national
options(scipen = 4, digits=4)
cons_sumw <- food_consw %>%
  group_by(itemcode, itemname) %>%
  summarise(median = survey_median(gafed, na.rm = TRUE),
            qt = survey_quantile(gafed, c(0.25, 0.75), na.rm=TRUE)) %>% 
  arrange(desc(median))

write_csv(cons_sumw, "data-TNPS/processed_data/cons_sumw.csv")

# Arusha and Kilimanjaro regions
cons_sumw_ak <- food_consw %>%
  filter(region == 2 | region ==3) %>% 
  group_by(itemcode, itemname) %>%
  summarise(median = survey_median(gafed, na.rm = TRUE),
            qt = survey_quantile(gafed, c(0.25, 0.75), na.rm=TRUE)) %>% 
  arrange(desc(median)) 

write_csv(cons_sumw_ak, "data-TNPS/processed_data/cons_sumw_ak.csv")

#-------------------------------------------------------------------------------
# ESTIMATE PERCENTAGES OF HOUSEHOLDS CONSUMING FOOD ITEMS - Supple table 3 and 4
# get data
food_cons <- import(here::here("data-TNPS", "processed_data","tza_food_consumption_5.csv")) %>% 
  rename(itemcode = item_code) %>% 
  mutate(cons = 1)
hh_weight <- import(here::here("data-TNPS", "raw_data", "consumption_real_y5.csv")) %>%
  select(y5_hhid, booster, cluster, strata, hhweight2) %>% 
  rename(hhid = y5_hhid)
hh4469 <- import(here::here("data-TNPS", "processed_data","hh4469.csv"))

# get full food items (86 food items)
tza_food_consumption_ja1 <- read_csv("data-TNPS/raw_data/hh_sec_ja1.csv") %>% 
  rename(hhid = y5_hhid) %>% 
  select(hhid, itemcode) # 4103hhs
tza_food_consumption_j1 <- read_csv("data-TNPS/raw_data/hh_sec_j1.csv") %>% 
  rename(hhid = y5_hhid) %>% 
  select(hhid, itemcode) # 406hhs
cons4709 <- rbind(tza_food_consumption_ja1, tza_food_consumption_j1) #4709hhs
cons4469 <- left_join(hh4469, cons4709, by = "hhid") #4469hhs with full food items
cons4469 <- left_join(cons4469, food_cons, by = c("hhid", "itemcode"))

cons4469w <- left_join(cons4469, hh_weight, by = "hhid")
cons4469w <- cons4469w %>% 
  dplyr::mutate(cons = replace_na(cons, 0))

cons4469w <- cons4469w %>%
  as_survey_design(ids = cluster, 
                   weights = hhweight2, 
                   strata = strata,
                   nest=TRUE)

# estimating the percentage of households to consume each food items
# replacing 'itemcode' manually
# national - all results are kept in cons_sumw_national.csv
cons4469w_byfood <- cons4469w %>% 
  filter(itemcode == 11021) %>% 
  group_by(cons) %>% 
  summarise(per = survey_prop())
View(cons4469w_byfood)

# Arusha and Kilimanjaro regions - all results are kept in cons_sumw_ak.csv
cons4469w_byfood_ak <- cons4469w %>% 
  filter(region == 2 | region ==3) %>% 
  filter(itemcode == 11021) %>% 
  group_by(cons) %>% 
  summarise(per = survey_prop())
View(cons4469w_byfood_ak)

# merge the results
# national - Supplement table 3 - supple_table3_nat.xlsx
cons_sumw <- import(here::here("data-TNPS", "processed_data","cons_sumw.csv"))
cons_sumw_nat <- import(here::here("data-TNPS", "processed_data","cons_sumw_national.xlsx"))
cons_sumw_nat_table <- left_join(cons_sumw, cons_sumw_nat, by = "itemcode")
write_csv(cons_sumw_table, "data-TNPS/processed_data/cons_sumw_nat_table.csv") # supple_table3_nat.xlsx
# Arusha and Kilimanjaro regions - Supplement table 4 - supple_table4_ak.xlsx
cons_sumw_ak <- import(here::here("data-TNPS", "processed_data","cons_sumw_ak.csv"))
cons_sumw_ak_per <- import(here::here("data-TNPS", "processed_data","cons_sumw_ak.xlsx"))
cons_sumw_ak_table <- left_join(cons_sumw_ak, cons_sumw_ak_per, by = "itemcode")

write_csv(cons_sumw_ak_table, "data-TNPS/processed_data/cons_sumw_ak_table.csv") # supple_table4_ak.xlsx


#-------------------------------------------------------------------------------
### INCLUDING WHEAT FLOUR AND OIL FROM WHEAT FLOUR PRODUCTS ### Table 4

food_cons <- import(here::here("data-TNPS", "processed_data","tza_food_consumption_5.csv")) %>% 
  rename(itemcode = item_code)

# wheat flour products are;
# 109 bread (wheat 0.806, oil 0.049), 202 sweets (wheat 0.820, oil 0.122), 1081 wheat flour (wheat 1.000),
# 110 Buns, cakes and biscuits (wheat 0.8186, oil 0.0487), 11011 Buns (wheat 0.8967, oil 0.0935),
# 11021 Cakes and biscuits (wheat 0.8186, oil 0.0487), 1001 cooking oil (oil 1.000)

itemcode <- c(109, 202, 1081, 110, 11011, 11021, 1001)
wheat_conv <- c(0.806, 0.820, 1.000, 0.8186, 0.8967, 0.8186, 0)
oil_conv <- c(0.049, 0.122, 0, 0.0487, 0.0935, 0.0487, 1.000)
wo_conv <- data.frame(itemcode, wheat_conv, oil_conv)

food_cons <- left_join(food_cons, wo_conv, by = "itemcode") %>% 
  mutate(wheat_p = gafed*wheat_conv) %>% # wheat flour in wheat flour products
  mutate(oil_p = gafed*oil_conv) # oil in wheat flour products

# adding total wheat flour and oil consumption
options(scipen = 6, digits=4)
wo_cons <- food_cons %>%
  group_by(hhid) %>%
  summarise(sumw = sum(wheat_p, na.rm = TRUE),
            sumo = sum(oil_p, na.rm = TRUE)) %>% 
  mutate(sumw = if_else(sumw == 0, NA_real_, sumw),
         sumo = if_else(sumo == 0, NA_real_, sumo)) %>% 
  mutate(consw = case_when(is.na(sumw) ~ 0, sumw > 0 ~ 1, TRUE ~ 0),
         conso = case_when(is.na(sumo) ~ 0, sumo > 0 ~ 1, TRUE ~ 0))

# weighting
hh_weight <- import(here::here("data-TNPS", "raw_data", "consumption_real_y5.csv")) %>%
  select(y5_hhid, booster, cluster, strata, hhweight2, region) %>% 
  rename(hhid = y5_hhid)
wo_consw <- left_join(wo_cons, hh_weight, by = "hhid")

wo_consw <- wo_consw %>%
  as_survey_design(ids = cluster, 
                   weights = hhweight2, 
                   strata = strata,
                   nest=TRUE)

# national
# estimate total wheat flour consumption
# median and IQR
wo_consw_sumw <- wo_consw %>%
  summarise(median = survey_median(sumw, na.rm = TRUE),
            qt = survey_quantile(sumw, c(0.25, 0.75), na.rm=TRUE)) 
# percentage
wo_consw_sumw_per <- wo_consw %>% 
    group_by(consw) %>% 
  summarise(per = survey_prop())

# estimate total oil consumption
wo_conso_sumw <- wo_consw %>%
  summarise(median = survey_median(sumo, na.rm = TRUE),
            qt = survey_quantile(sumo, c(0.25, 0.75), na.rm=TRUE))
wo_conso_sumw_per <- wo_consw %>% 
  group_by(conso) %>% 
  summarise(per = survey_prop())

# Arusha and Kilimanjaro regions
# estimate total wheat flour consumption
wo_consw_sumw_ak <- wo_consw %>%
  filter(region == 2 | region ==3) %>% 
  summarise(median = survey_median(sumw, na.rm = TRUE),
            qt = survey_quantile(sumw, c(0.25, 0.75), na.rm=TRUE))
wo_consw_sumw_perak <- wo_consw %>% 
  filter(region == 2 | region ==3) %>% 
  group_by(consw) %>% 
  summarise(per = survey_prop())

# estimate total oil consumption
wo_conso_sumw_ak <- wo_consw %>%
  filter(region == 2 | region ==3) %>% 
  summarise(median = survey_median(sumo, na.rm = TRUE),
            qt = survey_quantile(sumo, c(0.25, 0.75), na.rm=TRUE))
wo_conso_sumw_perak <- wo_consw %>% 
  filter(region == 2 | region ==3) %>% 
  group_by(conso) %>% 
  summarise(per = survey_prop())


#-------------------------------------------------------------------------------
### FOOD CONSUMPTION PER DAY PER AFE BY FOOD GROUPS ###  Table 3
# adjust dry weights

food_cons <- import(here::here("data-TNPS", "processed_data","tza_food_consumption_5.csv")) %>% 
  rename(itemcode = item_code)

# including the conversion factor of dry foods: 
# 903 canned milk / milk powder - [(food in g)*0.81] + food in g
# n = 0 # 809 dry/salted/canned fish and seafoods

foodgroup_cons <- food_cons %>%
  mutate(gafed_gr = ifelse(itemcode == 903, (gafed * 0.81) + gafed, gafed))

cons_foodg <- foodgroup_cons %>%
  group_by(hhid, groupname, groupcode) %>%
  summarise(grpcons = sum(gafed_gr), .groups = "drop")

# create full list of food groups
# get 4469hhs
hh4469 <- import(here::here("data-TNPS", "processed_data","hh4469.csv"))

# making data frame with full list of food groups
hh_foodg <- data.frame(
  hhid = rep(hh4469$hhid, each = 13),    # Repeat each hhid 13 times
  groupcode = rep(1:13, times = nrow(hh4469))  # Repeat groupcode 1-13 for all hhid
)

# merge with food group consumption
hh_foodgcons <- left_join(hh_foodg, cons_foodg, by = c("hhid", "groupcode")) %>% 
  mutate(cons = case_when((grpcons > 0) ~ 1, TRUE ~ 0))

# write csv
write_csv(hh_foodgcons, "data-TNPS/processed_data/foodg_consumption.csv")

# estimate food consumption by food groups
# weighting
hh_weight <- import(here::here("data-TNPS", "raw_data", "consumption_real_y5.csv")) %>%
  select(y5_hhid, booster, cluster, strata, hhweight2, region) %>% 
  rename(hhid = y5_hhid)

hh_foodgconsw <- left_join(hh_foodgcons, hh_weight, by = "hhid")

hh_foodgconsw <- hh_foodgconsw %>%
  as_survey_design(ids = cluster, 
                   weights = hhweight2, 
                   strata = strata,
                   nest=TRUE)

# estimate food consumption by food groups
# national
options(scipen = 6, digits=3)
grpcons_sum <- hh_foodgconsw %>%
  group_by(groupcode) %>% 
  summarise(median = survey_median(grpcons, na.rm = TRUE),
            qt = survey_quantile(grpcons, c(0.25, 0.75), na.rm=TRUE))
grpcons_sum_per <- hh_foodgconsw %>%
group_by(groupcode, cons) %>% 
  summarise(per = survey_prop()) %>% 
  filter(cons == 1)

grpcons_sum_medper <- left_join(grpcons_sum, grpcons_sum_per, by = "groupcode")

write_csv(grpcons_sum_medper, "data-TNPS/processed_data/foodg_consumption_sum.csv")

# Arusha and Kilimanjaro regions
grpcons_sum_ak <- hh_foodgconsw %>%
  filter(region == 2 | region ==3) %>% 
  group_by(groupcode) %>% 
  summarise(median = survey_median(grpcons, na.rm = TRUE),
            qt = survey_quantile(grpcons, c(0.25, 0.75), na.rm=TRUE))
grpcons_sum_per_ak <- hh_foodgconsw %>%
  filter(region == 2 | region ==3) %>% 
  group_by(groupcode, cons) %>% 
  summarise(per = survey_prop()) %>% 
  filter(cons == 1)

grpcons_sum_medper_ak <- left_join(grpcons_sum_ak, grpcons_sum_per_ak, by = "groupcode")

write_csv(grpcons_sum_medper_ak, "data-TNPS/processed_data/foodg_consumption_sum_ak.csv")

#-------------------------------------------------------------------------------
### CALCULATE F&V INTAKES and <400g F&V INTAKES### - Table 3

hh_foodgcons <-import(here::here("data-TNPS/processed_data/foodg_consumption.csv"))

# select F&V
hh_foodgconsFV <- hh_foodgcons %>% 
  filter(groupcode == 4 | groupcode == 5) %>% 
  group_by(hhid) %>%
  summarise(sumfv = sum(grpcons, na.rm = TRUE)) %>% 
  mutate(cons = case_when((sumfv > 0) ~ 1, TRUE ~ 0))

# weighting
hh_weight <- import(here::here("data-TNPS", "raw_data", "consumption_real_y5.csv")) %>%
  select(y5_hhid, booster, cluster, strata, hhweight2, region) %>% 
  rename(hhid = y5_hhid)

hh_foodgconsFVw <- left_join(hh_foodgconsFV, hh_weight, by = "hhid")

hh_foodgconsFVw <- hh_foodgconsFVw %>%
  as_survey_design(ids = cluster, 
                   weights = hhweight2, 
                   strata = strata,
                   nest=TRUE)

### estimate food consumption by food groups
# national
options(scipen = 6, digits=4)
grpconsFV_sum <- hh_foodgconsFVw %>%
  summarise(median = survey_median(sumfv, na.rm = TRUE),
            qt = survey_quantile(sumfv, c(0.25, 0.75), na.rm=TRUE))
grpconsFV_sum_per <- hh_foodgconsFVw %>%
  group_by(cons) %>% 
  summarise(per = survey_prop()) %>% 
  filter(cons == 1)

# Arusha and Kilimanjaro regions
options(scipen = 6, digits=4)
grpconsFV_sum_ak <- hh_foodgconsFVw %>%
  filter(region == 2 | region ==3) %>% 
  summarise(median = survey_median(sumfv, na.rm = TRUE),
            qt = survey_quantile(sumfv, c(0.25, 0.75), na.rm=TRUE))
grpconsFV_sum_per_ak <- hh_foodgconsFVw %>%
  filter(region == 2 | region ==3) %>% 
  group_by(cons) %>% 
  summarise(per = survey_prop()) %>% 
  filter(cons == 1)

### calculate <400g F&V intakes

hh_foodgconsFV400 <- hh_foodgconsFV %>% 
  mutate(fv400 = case_when((sumfv > 400) ~ 1, TRUE ~ 0))

hh_foodgconsFV400w <- left_join(hh_foodgconsFV400, hh_weight, by = "hhid")
  
hh_foodgconsFV400w <- hh_foodgconsFV400w %>%
  as_survey_design(ids = cluster, 
                   weights = hhweight2, 
                   strata = strata,
                   nest=TRUE)
# national
FV400_sum <- hh_foodgconsFV400w %>%
  group_by(fv400) %>% 
  summarise(per = survey_prop()) %>% 
  filter(fv400 == 0)

FV400_sum_ak <- hh_foodgconsFV400w %>%
  filter(region == 2 | region ==3) %>% 
  group_by(fv400) %>% 
  summarise(per = survey_prop()) %>% 
  filter(fv400 == 0)

#-------------------------------------------------------------------------------
### CALCULATE ESTIMATED ENERGY INTAKES ### - Table 3

energy <-import(here::here("data-TNPS/processed_data/tza_nps2021_base_ai.csv")) %>% 
  select(hhid, energy_kcal)
hh4469 <-import(here::here("data-TNPS/processed_data/hh4469.csv"))
hh_energy <- left_join(hh4469, energy, by = "hhid")

# weighting
hh_weight <- import(here::here("data-TNPS", "raw_data", "consumption_real_y5.csv")) %>%
  select(y5_hhid, booster, cluster, strata, hhweight2, region) %>% 
  rename(hhid = y5_hhid)

hh_energyw <- left_join(hh_energy, hh_weight, by = "hhid")

hh_energyw <- hh_energyw %>%
  as_survey_design(ids = cluster, 
                   weights = hhweight2, 
                   strata = strata,
                   nest=TRUE)

# national
options(scipen = 8, digits=6)
energy_sum <- hh_energyw %>%
  summarise(median = survey_median(energy_kcal, na.rm = TRUE),
            qt = survey_quantile(energy_kcal, c(0.25, 0.75), na.rm=TRUE))

# Arusha and Kilimanjaro regions
energy_sum_ak <- hh_energyw %>%
  filter(region == 2 | region ==3) %>% 
  summarise(median = survey_median(energy_kcal, na.rm = TRUE),
            qt = survey_quantile(energy_kcal, c(0.25, 0.75), na.rm=TRUE))

#-------------------------------------------------------------------------------
### FOOD CONSUMPTION BY HEAD EDUCATION AND QUINTILE ### - Tables 5-8
# Food group - Cereals and cereal products (#1), Meat, meat products and fish (#9), Eggs (#10), Fruits and vegetables (#4+#5)

hh_foodgcons <-import(here::here("data-TNPS/processed_data/foodg_consumption.csv"))

# estimate food consumption by food groups
# weighting
hh_weight <- import(here::here("data-TNPS", "raw_data", "consumption_real_y5.csv")) %>%
  select(y5_hhid, booster, cluster, strata, hhweight2, region) %>% 
  rename(hhid = y5_hhid)

hh_info <- import(here::here("data-TNPS", "processed_data","tza_nps2021_hh_info.csv")) %>% 
  select(hhid, res_quintile, educ_head2)

hh_weight <- left_join(hh_weight, hh_info, by = "hhid")

hh_foodgconsw <- left_join(hh_foodgcons, hh_weight, by = "hhid")

hh_foodgconsw <- hh_foodgconsw %>%
  as_survey_design(ids = cluster, 
                   weights = hhweight2, 
                   strata = strata,
                   nest=TRUE)

# estimate food group consumption by education of household head
# national
options(scipen = 6, digits=3)
grpcons_sum <- hh_foodgconsw %>%
  group_by(groupcode, educ_head2) %>% 
  summarise(median = survey_median(grpcons, na.rm = TRUE),
            qt = survey_quantile(grpcons, c(0.25, 0.75), na.rm=TRUE))

grpcons_sum_per <- hh_foodgconsw %>%
  group_by(groupcode, educ_head2, cons) %>% 
  summarise(per = survey_prop()) %>% 
  filter(cons == 1)

# Arusha and Kilimanjaro regions

grpcons_sum_ak <- hh_foodgconsw %>%
  filter(region == 2 | region ==3) %>% 
  group_by(groupcode, educ_head2) %>% 
  summarise(median = survey_median(grpcons, na.rm = TRUE),
            qt = survey_quantile(grpcons, c(0.25, 0.75), na.rm=TRUE))

grpcons_sum_per_ak <- hh_foodgconsw %>%
  filter(region == 2 | region ==3) %>% 
  group_by(groupcode, educ_head2, cons) %>% 
  summarise(per = survey_prop()) %>% 
  filter(cons == 1)


### estimate F&V consumption by education of household head
hh_foodgcons <-import(here::here("data-TNPS/processed_data/foodg_consumption.csv"))

hh_foodgconsFV <- hh_foodgcons %>% 
  filter(groupcode == 4 | groupcode == 5) %>% 
  group_by(hhid) %>%
  summarise(sumfv = sum(grpcons, na.rm = TRUE)) %>% 
  mutate(cons = case_when((sumfv > 0) ~ 1, TRUE ~ 0))

# weighting
hh_weight <- import(here::here("data-TNPS", "raw_data", "consumption_real_y5.csv")) %>%
  select(y5_hhid, booster, cluster, strata, hhweight2, region) %>% 
  rename(hhid = y5_hhid)

hh_info <- import(here::here("data-TNPS", "processed_data","tza_nps2021_hh_info.csv")) %>% 
  select(hhid, res_quintile, educ_head2)

hh_weight <- left_join(hh_weight, hh_info, by = "hhid")

hh_foodgconsFVw <- left_join(hh_foodgconsFV, hh_weight, by = "hhid")

hh_foodgconsFVw <- hh_foodgconsFVw %>%
  as_survey_design(ids = cluster, 
                   weights = hhweight2, 
                   strata = strata,
                   nest=TRUE)

### estimate food consumption by education of household head
# national
options(scipen = 6, digits=4)
grpconsFV_sum <- hh_foodgconsFVw %>%
  group_by(educ_head2) %>% 
  summarise(median = survey_median(sumfv, na.rm = TRUE),
            qt = survey_quantile(sumfv, c(0.25, 0.75), na.rm=TRUE))

grpconsFV_sum_per <- hh_foodgconsFVw %>%
  group_by(educ_head2, cons) %>% 
  summarise(per = survey_prop()) %>% 
  filter(cons == 1)

# Arusha and Kilimanjaro regions
options(scipen = 6, digits=4)
grpconsFV_sum_ak <- hh_foodgconsFVw %>%
  filter(region == 2 | region ==3) %>% 
  group_by(educ_head2) %>% 
  summarise(median = survey_median(sumfv, na.rm = TRUE),
            qt = survey_quantile(sumfv, c(0.25, 0.75), na.rm=TRUE))

grpconsFV_sum_per_ak <- hh_foodgconsFVw %>%
  filter(region == 2 | region ==3) %>% 
  group_by(educ_head2, cons) %>% 
  summarise(per = survey_prop()) %>% 
  filter(cons == 1)



#-------------------------------------------------------------------------------
### FOOD CONSUMPTION BY HEAD EDUCATION AND QUINTILE ### - Tables 5-8
# Food group - Cereals and cereal products (#1), Meat, meat products and fish (#9), Eggs (#10), Fruits and vegetables (#4+#5)

hh_foodgcons <-import(here::here("data-TNPS/processed_data/foodg_consumption.csv"))

# estimate food consumption by food groups
# weighting
hh_weight <- import(here::here("data-TNPS", "raw_data", "consumption_real_y5.csv")) %>%
  select(y5_hhid, booster, cluster, strata, hhweight2, region) %>% 
  rename(hhid = y5_hhid)

hh_info <- import(here::here("data-TNPS", "processed_data","tza_nps2021_hh_info.csv")) %>% 
  select(hhid, res_quintile, educ_head2)

hh_weight <- left_join(hh_weight, hh_info, by = "hhid")

hh_foodgconsw <- left_join(hh_foodgcons, hh_weight, by = "hhid")

hh_foodgconsw <- hh_foodgconsw %>%
  as_survey_design(ids = cluster, 
                   weights = hhweight2, 
                   strata = strata,
                   nest=TRUE)

# estimate food group consumption by education of household head
# national
options(scipen = 6, digits=3)
grpcons_sum <- hh_foodgconsw %>%
  group_by(groupcode, educ_head2) %>% 
  summarise(median = survey_median(grpcons, na.rm = TRUE),
            qt = survey_quantile(grpcons, c(0.25, 0.75), na.rm=TRUE))

grpcons_sum_per <- hh_foodgconsw %>%
  group_by(groupcode, educ_head2, cons) %>% 
  summarise(per = survey_prop()) %>% 
  filter(cons == 1)

# Arusha and Kilimanjaro regions

grpcons_sum_ak <- hh_foodgconsw %>%
  filter(region == 2 | region ==3) %>% 
  group_by(groupcode, educ_head2) %>% 
  summarise(median = survey_median(grpcons, na.rm = TRUE),
            qt = survey_quantile(grpcons, c(0.25, 0.75), na.rm=TRUE))

grpcons_sum_per_ak <- hh_foodgconsw %>%
  filter(region == 2 | region ==3) %>% 
  group_by(groupcode, educ_head2, cons) %>% 
  summarise(per = survey_prop()) %>% 
  filter(cons == 1)


### estimate F&V consumption by education of household head
hh_foodgcons <-import(here::here("data-TNPS/processed_data/foodg_consumption.csv"))

hh_foodgconsFV <- hh_foodgcons %>% 
  filter(groupcode == 4 | groupcode == 5) %>% 
  group_by(hhid) %>%
  summarise(sumfv = sum(grpcons, na.rm = TRUE)) %>% 
  mutate(cons = case_when((sumfv > 0) ~ 1, TRUE ~ 0))

# weighting
hh_weight <- import(here::here("data-TNPS", "raw_data", "consumption_real_y5.csv")) %>%
  select(y5_hhid, booster, cluster, strata, hhweight2, region) %>% 
  rename(hhid = y5_hhid)

hh_info <- import(here::here("data-TNPS", "processed_data","tza_nps2021_hh_info.csv")) %>% 
  select(hhid, res_quintile, educ_head2)

hh_weight <- left_join(hh_weight, hh_info, by = "hhid")

hh_foodgconsFVw <- left_join(hh_foodgconsFV, hh_weight, by = "hhid")

hh_foodgconsFVw <- hh_foodgconsFVw %>%
  as_survey_design(ids = cluster, 
                   weights = hhweight2, 
                   strata = strata,
                   nest=TRUE)

### estimate food consumption by education of household head
# national
options(scipen = 6, digits=4)
grpconsFV_sum <- hh_foodgconsFVw %>%
  group_by(educ_head2) %>% 
  summarise(median = survey_median(sumfv, na.rm = TRUE),
            qt = survey_quantile(sumfv, c(0.25, 0.75), na.rm=TRUE))

grpconsFV_sum_per <- hh_foodgconsFVw %>%
  group_by(educ_head2, cons) %>% 
  summarise(per = survey_prop()) %>% 
  filter(cons == 1)

# Arusha and Kilimanjaro regions
options(scipen = 6, digits=4)
grpconsFV_sum_ak <- hh_foodgconsFVw %>%
  filter(region == 2 | region ==3) %>% 
  group_by(educ_head2) %>% 
  summarise(median = survey_median(sumfv, na.rm = TRUE),
            qt = survey_quantile(sumfv, c(0.25, 0.75), na.rm=TRUE))

grpconsFV_sum_per_ak <- hh_foodgconsFVw %>%
  filter(region == 2 | region ==3) %>% 
  group_by(educ_head2, cons) %>% 
  summarise(per = survey_prop()) %>% 
  filter(cons == 1)

#-------------------------------------------------------------------------------
### FOOD CONSUMPTION BY HEAD EDUCATION AND QUINTILE ### - Tables 5-8
# Food group - Cereals and cereal products (#1), Meat, meat products and fish (#9), Eggs (#10), Fruits and vegetables (#4+#5)

hh_foodgcons <-import(here::here("data-TNPS/processed_data/foodg_consumption.csv"))

# estimate food consumption by food groups
# weighting
hh_weight <- import(here::here("data-TNPS", "raw_data", "consumption_real_y5.csv")) %>%
  select(y5_hhid, booster, cluster, strata, hhweight2, region) %>% 
  rename(hhid = y5_hhid)

hh_info <- import(here::here("data-TNPS", "processed_data","tza_nps2021_hh_info.csv")) %>% 
  select(hhid, res_quintile, res_quintile)

hh_weight <- left_join(hh_weight, hh_info, by = "hhid")

hh_foodgconsw <- left_join(hh_foodgcons, hh_weight, by = "hhid")

hh_foodgconsw <- hh_foodgconsw %>%
  as_survey_design(ids = cluster, 
                   weights = hhweight2, 
                   strata = strata,
                   nest=TRUE)

# estimate food group consumption by education of household head
# national
options(scipen = 6, digits=3)
grpcons_sum <- hh_foodgconsw %>%
  group_by(groupcode, res_quintile) %>% 
  summarise(median = survey_median(grpcons, na.rm = TRUE),
            qt = survey_quantile(grpcons, c(0.25, 0.75), na.rm=TRUE))

grpcons_sum_per <- hh_foodgconsw %>%
  group_by(groupcode, res_quintile, cons) %>% 
  summarise(per = survey_prop()) %>% 
  filter(cons == 1)

# Arusha and Kilimanjaro regions

grpcons_sum_ak <- hh_foodgconsw %>%
  filter(region == 2 | region ==3) %>% 
  group_by(groupcode, res_quintile) %>% 
  summarise(median = survey_median(grpcons, na.rm = TRUE),
            qt = survey_quantile(grpcons, c(0.25, 0.75), na.rm=TRUE))

grpcons_sum_per_ak <- hh_foodgconsw %>%
  filter(region == 2 | region ==3) %>% 
  group_by(groupcode, res_quintile, cons) %>% 
  summarise(per = survey_prop()) %>% 
  filter(cons == 1)


### estimate F&V consumption by education of household head
hh_foodgcons <-import(here::here("data-TNPS/processed_data/foodg_consumption.csv"))

hh_foodgconsFV <- hh_foodgcons %>% 
  filter(groupcode == 4 | groupcode == 5) %>% 
  group_by(hhid) %>%
  summarise(sumfv = sum(grpcons, na.rm = TRUE)) %>% 
  mutate(cons = case_when((sumfv > 0) ~ 1, TRUE ~ 0))

# weighting
hh_weight <- import(here::here("data-TNPS", "raw_data", "consumption_real_y5.csv")) %>%
  select(y5_hhid, booster, cluster, strata, hhweight2, region) %>% 
  rename(hhid = y5_hhid)

hh_info <- import(here::here("data-TNPS", "processed_data","tza_nps2021_hh_info.csv")) %>% 
  select(hhid, res_quintile, res_quintile)

hh_weight <- left_join(hh_weight, hh_info, by = "hhid")

hh_foodgconsFVw <- left_join(hh_foodgconsFV, hh_weight, by = "hhid")

hh_foodgconsFVw <- hh_foodgconsFVw %>%
  as_survey_design(ids = cluster, 
                   weights = hhweight2, 
                   strata = strata,
                   nest=TRUE)

### estimate food consumption by education of household head
# national
options(scipen = 6, digits=4)
grpconsFV_sum <- hh_foodgconsFVw %>%
  group_by(res_quintile) %>% 
  summarise(median = survey_median(sumfv, na.rm = TRUE),
            qt = survey_quantile(sumfv, c(0.25, 0.75), na.rm=TRUE))

grpconsFV_sum_per <- hh_foodgconsFVw %>%
  group_by(res_quintile, cons) %>% 
  summarise(per = survey_prop()) %>% 
  filter(cons == 1)

# Arusha and Kilimanjaro regions
options(scipen = 6, digits=4)
grpconsFV_sum_ak <- hh_foodgconsFVw %>%
  filter(region == 2 | region ==3) %>% 
  group_by(res_quintile) %>% 
  summarise(median = survey_median(sumfv, na.rm = TRUE),
            qt = survey_quantile(sumfv, c(0.25, 0.75), na.rm=TRUE))

grpconsFV_sum_per_ak <- hh_foodgconsFVw %>%
  filter(region == 2 | region ==3) %>% 
  group_by(res_quintile, cons) %>% 
  summarise(per = survey_prop()) %>% 
  filter(cons == 1)

#-------------------------------------------------------------------------------
### FOOD CONSUMPTION BY HEAD EDUCATION AND QUINTILE ### - Tables 5-8
# Food group - Cereals and cereal products (#1), Meat, meat products and fish (#9), Eggs (#10), Fruits and vegetables (#4+#5)

hh_foodgcons <-import(here::here("data-TNPS/processed_data/foodg_consumption.csv"))

# estimate food consumption by food groups
# weighting
hh_weight <- import(here::here("data-TNPS", "raw_data", "consumption_real_y5.csv")) %>%
  select(y5_hhid, booster, cluster, strata, hhweight2, region) %>% 
  rename(hhid = y5_hhid)

hh_info <- import(here::here("data-TNPS", "processed_data","tza_nps2021_hh_info.csv")) %>% 
  select(hhid, res_quintile, res_quintile)

hh_weight <- left_join(hh_weight, hh_info, by = "hhid")

hh_foodgconsw <- left_join(hh_foodgcons, hh_weight, by = "hhid")

hh_foodgconsw <- hh_foodgconsw %>%
  as_survey_design(ids = cluster, 
                   weights = hhweight2, 
                   strata = strata,
                   nest=TRUE)

# estimate food group consumption by quintiles
# national
options(scipen = 6, digits=3)
grpcons_sum <- hh_foodgconsw %>%
  group_by(groupcode, res_quintile) %>% 
  summarise(median = survey_median(grpcons, na.rm = TRUE),
            qt = survey_quantile(grpcons, c(0.25, 0.75), na.rm=TRUE))

grpcons_sum_per <- hh_foodgconsw %>%
  group_by(groupcode, res_quintile, cons) %>% 
  summarise(per = survey_prop()) %>% 
  filter(cons == 1)

# Arusha and Kilimanjaro regions

grpcons_sum_ak <- hh_foodgconsw %>%
  filter(region == 2 | region ==3) %>% 
  group_by(groupcode, res_quintile) %>% 
  summarise(median = survey_median(grpcons, na.rm = TRUE),
            qt = survey_quantile(grpcons, c(0.25, 0.75), na.rm=TRUE))

grpcons_sum_per_ak <- hh_foodgconsw %>%
  filter(region == 2 | region ==3) %>% 
  group_by(groupcode, res_quintile, cons) %>% 
  summarise(per = survey_prop()) %>% 
  filter(cons == 1)


### estimate F&V consumption by quintile
hh_foodgcons <-import(here::here("data-TNPS/processed_data/foodg_consumption.csv"))

hh_foodgconsFV <- hh_foodgcons %>% 
  filter(groupcode == 4 | groupcode == 5) %>% 
  group_by(hhid) %>%
  summarise(sumfv = sum(grpcons, na.rm = TRUE)) %>% 
  mutate(cons = case_when((sumfv > 0) ~ 1, TRUE ~ 0))

# weighting
hh_weight <- import(here::here("data-TNPS", "raw_data", "consumption_real_y5.csv")) %>%
  select(y5_hhid, booster, cluster, strata, hhweight2, region) %>% 
  rename(hhid = y5_hhid)

hh_info <- import(here::here("data-TNPS", "processed_data","tza_nps2021_hh_info.csv")) %>% 
  select(hhid, res_quintile, res_quintile)

hh_weight <- left_join(hh_weight, hh_info, by = "hhid")

hh_foodgconsFVw <- left_join(hh_foodgconsFV, hh_weight, by = "hhid")

hh_foodgconsFVw <- hh_foodgconsFVw %>%
  as_survey_design(ids = cluster, 
                   weights = hhweight2, 
                   strata = strata,
                   nest=TRUE)

### estimate F&V consumption by quintile
# national
options(scipen = 6, digits=4)
grpconsFV_sum <- hh_foodgconsFVw %>%
  group_by(res_quintile) %>% 
  summarise(median = survey_median(sumfv, na.rm = TRUE),
            qt = survey_quantile(sumfv, c(0.25, 0.75), na.rm=TRUE))

grpconsFV_sum_per <- hh_foodgconsFVw %>%
  group_by(res_quintile, cons) %>% 
  summarise(per = survey_prop()) %>% 
  filter(cons == 1)

# Arusha and Kilimanjaro regions
options(scipen = 6, digits=4)
grpconsFV_sum_ak <- hh_foodgconsFVw %>%
  filter(region == 2 | region ==3) %>% 
  group_by(res_quintile) %>% 
  summarise(median = survey_median(sumfv, na.rm = TRUE),
            qt = survey_quantile(sumfv, c(0.25, 0.75), na.rm=TRUE))

grpconsFV_sum_per_ak <- hh_foodgconsFVw %>%
  filter(region == 2 | region ==3) %>% 
  group_by(res_quintile, cons) %>% 
  summarise(per = survey_prop()) %>% 
  filter(cons == 1)

#-------------------------------------------------------------------------------
#-------------------------------------------------------------------------------
