library(here)
library(lubridate)
library(tidyverse)


# Data Load ---------------------------------------------------------------

pisco = read_csv(here::here("data", "raw_data", "kelp_biomass.csv"))[, -1] %>% 
  janitor::clean_names() %>% 
  select(year, month, day, region4, mpa_state_designation, mpa_defacto_designation, site, zone, level, transect, count, sciname, family, target_status, weight_g, weight_kg, length_to_use) 
mpa_traits = read_csv(here::here("data", "raw_data", "mpa_traits.csv"))[, -1] %>% 
  janitor::clean_names() %>% 
  select(-state_region)
sites = read_csv(here::here("data", "raw_data", "PISCO_kelpforest_site_table.csv")) %>% 
  janitor::clean_names() %>% 
  select(site, mpa_name, latitude, longitude, site_status) %>% 
  mutate(mpa_name = str_to_lower(mpa_name)) %>% 
  rename(affiliated_mpa = mpa_name)
settle = read_csv(here::here("data", "raw_data", "settlement_mpa.csv"))[, -1] %>% 
  janitor::clean_names() %>% 
  select(-mpa) %>% 
  select(affiliated_mpa, settlement_mpa_total)
homerange_rf = read_csv(here::here("data", "raw_data", "homerange_rf_predictions.csv")) %>% 
  janitor::clean_names() %>% 
  mutate(homerange = case_when(
    is.na(observed_homerange) ~ predicted_homerange, 
    !is.na(observed_homerange) ~ observed_homerange
  )) %>% 
  select(species, family, common_name, homerange) 
pld_rf = read_csv(here::here("data", "raw_data", "pld_rf_predictions.csv")) %>% 
  janitor::clean_names() %>% 
  mutate(pld = case_when(
  is.na(observed_pld) ~ predicted_pld, 
  !is.na(observed_pld) ~ observed_pld
)) %>% 
  select(species, family, common_name, pld) 


# Home Range Data Merge ---------------------------------------------------

rf_data = full_join(homerange_rf, pld_rf) %>% 
  mutate(pld = case_when(
    is.na(pld) ~ 0,
    TRUE ~ pld
  )) %>% 
  rename(sciname = species) %>%
  mutate(magnitude_homerange = floor(log10(homerange))) %>%
  mutate(month_pld = ceiling(pld / 30))

write_csv(rf_data, here::here("data", "raw_data", "rf_data.csv"))

# PISCO Merge -------------------------------------------------------------

mpas_of_interest = c("anacapa island smca", "anacapa island smr", "harris point smr", "south point smr",
                     "carrington point smr", "gull island smr", "painted cave smca", "scorpion smr",
                     "santa barbara island smr", "naples smca", "campus point smca")

pisco_merge = settle %>% 
  mutate(affiliated_mpa = str_to_lower(affiliated_mpa)) %>% 
  full_join(mpa_traits) %>% 
  full_join(sites) %>% 
  distinct() %>% 
  filter(!is.na(size)) %>% 
  filter(!is.na(site)) %>% 
  full_join(pisco) %>% 
  filter(!is.na(count)) %>% 
  filter(affiliated_mpa %in% mpas_of_interest)

write_csv(pisco_merge, here::here("data", "raw_data", "full_pisco.csv"))
