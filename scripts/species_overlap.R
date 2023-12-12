library(here)
library(lubridate)
library(tidyverse)

rf_data = read_csv(here::here("data", "raw_data", "rf_data.csv"))
pisco = read_csv(here::here("data", "raw_data", "full_pisco.csv")) %>% 
  distinct()
pisco$date <- as.Date(with(pisco, paste(year, month, day, sep = "-")), "%Y-%m-%d")

rf_in_pisco = rf_data %>% 
  filter(sciname %in% pisco$sciname) 
rf_out_pisco = rf_data %>% 
  filter(!sciname %in% pisco$sciname)

rf_species = pisco %>% 
  filter(sciname %in% rf_in_pisco$sciname)
rf_no_species = pisco %>% 
  filter(!sciname %in% rf_in_pisco$sciname)

rf_no = unique(rf_no_species$sciname)

pisco_rf = left_join(rf_species, rf_data) 

write_csv(pisco_rf, here::here("data", "raw_data", "pisco_rf.csv"))


# Species of Interest -----------------------------------------------------

species_num = pisco %>% 
  group_by(sciname) %>% 
  summarize(count = n())

species_mpa_num = pisco %>% 
  group_by(sciname, affiliated_mpa) %>% 
  summarize(count = n()) %>% 
  filter(count > 10) %>% 
  group_by(sciname) %>% 
  mutate(mpa_count = n()) %>% 
  filter(mpa_count > 1)

scinames = species_mpa_num %>% 
  select(sciname) %>% 
  distinct() %>% 
  filter(!is.na(sciname))

write_csv(scinames, here::here("data", "raw_data", "species_of_interest.csv"))

scinames_in_rf = scinames %>% 
  filter(sciname %in% pisco_rf$sciname)
