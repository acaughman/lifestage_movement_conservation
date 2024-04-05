library(tidyverse)
library(patchwork)
library(lmtest)


# data prep ---------------------------------------------------------------


data <- read_csv(here::here("data", "raw_data", "pisco_rf.csv")) %>%
  filter(!is.na(homerange)) %>%
  filter(level != "CAN") %>%
  filter(site_status == "MPA")%>% 
  filter(mpa_defacto_designation != "ref") %>%
  dplyr::select(-lat_c, -lon_c, -latitude, -longitude) %>% 
  distinct() %>% 
  group_by(date, affiliated_mpa) %>% 
  mutate(temp = mean(temp, na.rm=TRUE)) %>% 
  ungroup() %>% 
  distinct()%>% 
  group_by(sciname, date, affiliated_mpa) %>%
  mutate(total_biomass = sum(weight_kg, na.rm = TRUE),
         total_count = sum(count, na.rm =TRUE)) %>% 
  ungroup() %>% 
  dplyr::select(-weight_kg, -weight_g, -length_to_use) %>% 
  distinct() %>% 
  group_by(sciname, date, affiliated_mpa) %>% 
  mutate(n = n()) %>% 
  ungroup() %>% 
  dplyr::select(-site, -level, -transect, -zone) %>% 
  distinct() %>% 
  mutate(biomass = total_biomass/n,
         count = total_count /n)%>%
  mutate(move_combo = paste(magnitude_homerange, month_pld)) 

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
  filter(sciname %in% species_mpa_num$sciname) %>% 
  distinct() %>% 
  mutate(move_combo = fct_relevel(as.factor(move_combo), c("-1 1", "-1 2", "-2 0", "-2 2", "-3 2"))) %>% 
  distinct() %>% 
  mutate(sciname = fct_relevel(as.factor(sciname), c("Trachurus symmetricus", "Semicossyphus pulcher", 
                                                     "Sebastes melanops", "Caulolatilus princeps",
                                                     "Sebastes mystinus", "Sebastes paucispinis",
                                                     "Sebastes carnatus", "Ophiodon elongatus")))


# granger tests -----------------------------------------------------------

c_princeps = sub_data %>% #med homerange, low pld
  filter(sciname == "Caulolatilus princeps")

grangertest(count ~ temp, order = 1, data = c_princeps)

grangertest(biomass ~ temp, order = 1, data = c_princeps)

#no relation
o_elongatus = sub_data %>% #low hr high PLD
  filter(sciname == "Ophiodon elongatus")

grangertest(count ~ temp, order = 1, data = o_elongatus)

grangertest(biomass ~ temp, order = 1, data = o_elongatus)

#good prediction
s_carnatus = sub_data %>% #low hr high PLD
  filter(sciname == "Sebastes carnatus")

grangertest(count ~ temp, order = 1, data = s_carnatus)

grangertest(biomass ~ temp, order = 1, data = s_carnatus)

#no relation
s_mystinus = sub_data %>% #med homerange, high pld
  filter(sciname == "Sebastes mystinus")

grangertest(count ~ temp, order = 1, data = s_mystinus)

grangertest(biomass ~ temp, order = 1, data = s_mystinus)


#no relation
s_pulcher = sub_data %>% #high home range high pld
  filter(sciname == "Semicossyphus pulcher")

# good prediction
grangertest(count ~ temp, order = 1, data = s_pulcher)

#no relations
grangertest(biomass ~ temp, order = 1, data = s_pulcher)
