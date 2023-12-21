library(tidyverse)
library(patchwork)
library(lmtest)


# data prep ---------------------------------------------------------------


data <- read_csv(here::here("data", "raw_data", "pisco_rf.csv")) %>%
  filter(!is.na(homerange)) %>%
  filter(level != "CAN") %>%
  group_by(sciname, length_to_use, year, transect, date, affiliated_mpa, level, zone) %>%
  mutate(
    total_count = sum(count),
    biomass = sum(weight_kg)
  ) %>%
  ungroup() %>%
  dplyr::select(-transect, -level, -zone, -count, -weight_kg, -weight_g, -lat_c, -lon_c) %>%
  distinct() %>%
  group_by(sciname, year, date, affiliated_mpa) %>%
  mutate(
    total_count = sum(total_count),
    total_biomass = sum(biomass, na.rm = TRUE)
  ) %>%
  ungroup() %>%
  distinct() %>%
  mutate(move_combo = paste(magnitude_homerange, month_pld)) %>%
  filter(site_status == "MPA")

scinames <- data %>%
  dplyr::select(sciname, common_name, month_pld, magnitude_homerange) %>%
  distinct()

species_mpa_num <- data %>%
  group_by(sciname, affiliated_mpa) %>%
  summarize(count = n()) %>%
  filter(count > 10) %>%
  group_by(sciname) %>%
  mutate(mpa_count = n()) %>%
  filter(mpa_count > 1) %>%
  left_join(scinames)

sub_data <- data %>%
  filter(sciname %in% species_mpa_num$sciname)%>%
  dplyr::select(-length_to_use, -biomass) %>%
  distinct()


# granger tests -----------------------------------------------------------

c_princeps = sub_data %>% #med homerange, low pld
  filter(sciname == "Caulolatilus princeps")

#good prediction for count
grangertest(total_count ~ temp, order = 1, data = c_princeps)
grangertest(total_count ~ temp, order = 2, data = c_princeps)
grangertest(total_count ~ temp, order = 3, data = c_princeps)

# no relationships
grangertest(total_biomass ~ temp, order = 1, data = c_princeps)

#no relation
o_elongatus = sub_data %>% #low hr high PLD
  filter(sciname == "Ophiodon elongatus")

grangertest(total_count ~ temp, order = 1, data = o_elongatus)

grangertest(total_biomass ~ temp, order = 1, data = o_elongatus)

#good prediction
s_carnatus = sub_data %>% #low hr high PLD
  filter(sciname == "Sebastes carnatus")

grangertest(total_count ~ temp, order = 1, data = s_carnatus)
grangertest(total_count ~ temp, order = 2, data = s_carnatus)
grangertest(total_count ~ temp, order = 3, data = s_carnatus)

grangertest(total_biomass ~ temp, order = 1, data = s_carnatus)

#no relation
s_mystinus = sub_data %>% #med homerange, high pld
  filter(sciname == "Sebastes mystinus")

grangertest(total_count ~ temp, order = 1, data = s_mystinus)

grangertest(total_biomass ~ temp, order = 1, data = s_mystinus)


#no relation
s_pulcher = sub_data %>% #high home range high pld
  filter(sciname == "Semicossyphus pulcher")

# good prediction
grangertest(total_count ~ temp, order = 1, data = s_pulcher)
grangertest(total_count ~ temp, order = 2, data = s_pulcher)
grangertest(total_count ~ temp, order = 3, data = s_pulcher)

#no relations
grangertest(total_biomass ~ temp, order = 1, data = s_pulcher)
