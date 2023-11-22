library(tidyverse)
library(patchwork)
library(sf)

data = read_csv(here::here("data", "processed_data", "pisco_SST.csv")) %>% 
  filter(site_status == "MPA") %>% 
  filter(level != "CAN") %>% 
  group_by(sciname, length_to_use, year, transect, date, affiliated_mpa) %>% 
  mutate(total_count = sum(count)) %>% 
  distinct() %>% 
  mutate(move_combo = paste(magnitude_homerange, month_pld)) %>% 
  mutate() %>% 
  ungroup() 


# Species Summary ---------------------------------------------------------

length(unique(data$sciname))

move_data = data %>% 
  select(sciname, magnitude_homerange, month_pld) %>% 
  distinct()

p1 = ggplot(move_data, aes(magnitude_homerange, month_pld)) +
  geom_jitter(aes(color = as.factor(magnitude_homerange), shape = as.factor(month_pld)), size = 3) +
  theme_bw() +
  labs(x = "Home range Magnitude",
       y = "Month PLD",
       color = "Home range Magnitude",
       shape = "Month PLD",)
p1

ggsave(p1, file = paste0("species_move_summary.pdf"), path = here::here("figs"), height = 8, width = 12)

species_sum = data %>% 
  select(sciname, move_combo) %>% 
  distinct() %>% 
  group_by(move_combo) %>% 
  summarize(count = n())

# MPA Summary -------------------------------------------------------------

ca = st_read(here::here("data", "raw_data", "channel_islands.shp")) %>% 
  janitor::clean_names()

## PA SHP Cite
# UNEP-WCMC and IUCN (2023), Protected Planet: The World Database on Protected Areas (WDPA) and World Database on Other Effective Area-based Conservation Measures (WD-OECM) [Online], November 2023, Cambridge, UK: UNEP-WCMC and IUCN. Available at: www.protectedplanet.net.
mpa1 = st_read(here::here("data", "raw_data", "WDPA", "WDPA_shp_0", "WDPA_WDOECM_Nov2023_Public_1b091e2b51c258eb3b24328044a4a3d6979e00fa2029d7538aef2befd46da362_shp-polygons.shp"))
mpa2 = st_read(here::here("data", "raw_data", "WDPA", "WDPA_shp_1", "WDPA_WDOECM_Nov2023_Public_1b091e2b51c258eb3b24328044a4a3d6979e00fa2029d7538aef2befd46da362_shp-polygons.shp"))
mpa3 = st_read(here::here("data", "raw_data", "WDPA", "WDPA_shp_2", "WDPA_WDOECM_Nov2023_Public_1b091e2b51c258eb3b24328044a4a3d6979e00fa2029d7538aef2befd46da362_shp-polygons.shp"))

mpa_names1 = as.data.frame(unique(mpa1$NAME))
mpa_names2 = as.data.frame(unique(mpa2$NAME))
mpa_names3 = as.data.frame(unique(mpa3$NAME))

mpa1 = mpa1 %>% 
  filter(NAME == "Anacapa Island State Marine Conservation Area" | grepl("Painted", NAME))
mpa2 = mpa2 %>% 
  filter(NAME == "Carrington Point (Santa Rosa Island) State Marine Reserve" |
           NAME == "Gull Island (Santa Cruz Island) State Marine Reserve" |
           NAME == "Harris Point (San Miguel Island) State Marine Reserve"|
           NAME == "Anacapa Island State Marine Reserve"|
           NAME == "Santa Barbara Island State Marine Reserve"|
           NAME == "Scorpion (Santa Cruz Island) State Marine Reserve"|
           NAME == "South Point (Santa Rosa Island) State Marine Reserve")
mpa3 = mpa3 %>% 
  filter(NAME == "Campus Point State Marine Conservation Area" |
  NAME == "Naples State Marine Conservation Area")

mpa_sf = list(mpa1, mpa2, mpa3) %>% 
  reduce(rbind) %>% 
  janitor::clean_names() %>% 
  select(name) %>% 
  mutate(affiliated_mpa = case_when(
    name == "Anacapa Island State Marine Conservation Area" ~ "anacapa island smca",                
    name == "Painted Cave (Santa Cruz Island) State Marine Conservation Area" ~ "painted cave smca",
    name == "Harris Point (San Miguel Island) State Marine Reserve" ~ "harris point smr",    
    name == "Carrington Point (Santa Rosa Island) State Marine Reserve" ~ "carrington point smr",    
    name == "South Point (Santa Rosa Island) State Marine Reserve" ~ "south point smr",         
    name == "Gull Island (Santa Cruz Island) State Marine Reserve" ~ "gull island smr",          
    name == "Scorpion (Santa Cruz Island) State Marine Reserve" ~ "scorpion smr",          
    name == "Santa Barbara Island State Marine Reserve" ~ "santa barbara island smr",             
    name == "Anacapa Island State Marine Reserve" ~ "anacapa island smr",                     
    name == "Naples State Marine Conservation Area" ~ "naples smca",                          
    name == "Campus Point State Marine Conservation Area" ~ "campus point smca"
  ))

mpa_data = data %>% 
  select(affiliated_mpa, settlement_mpa_total, implementation_date, size, 
         habitat_richness, habitat_diversity, prop_rock, site_status, temp) %>% 
  distinct() %>% 
  group_by(affiliated_mpa, settlement_mpa_total, implementation_date, size, 
           habitat_richness, habitat_diversity, prop_rock, site_status) %>% 
  summarize(temp = mean(temp, na.rm=TRUE)) %>% 
  full_join(mpa_sf) %>% 
  st_as_sf()

ca = ca %>% 
  st_transform(crs = st_crs(mpa_data))

p1 = ggplot() +
  geom_sf(data = ca) +
  geom_sf(data = mpa_data, aes(fill = affiliated_mpa)) +
  theme_bw() +
  labs(fill = "MPA Name") +
  scale_fill_viridis_d()

p2 = ggplot() +
  geom_sf(data = ca) +
  geom_sf(data = mpa_data, aes(fill = settlement_mpa_total)) +
  theme_bw() +
  labs(fill = "Total Settlement") +
  scale_fill_viridis_c()

p3 = ggplot() +
  geom_sf(data = ca) +
  geom_sf(data = mpa_data, aes(fill = size)) +
  theme_bw() +
  labs(fill = "MPA Size") +
  scale_fill_viridis_c()

p4 = ggplot() +
  geom_sf(data = ca) +
  geom_sf(data = mpa_data, aes(fill = habitat_richness)) +
  theme_bw() +
  labs(fill = "Habitat Richness") +
  scale_fill_viridis_c()

p5 = ggplot() +
  geom_sf(data = ca) +
  geom_sf(data = mpa_data, aes(fill = habitat_diversity)) +
  theme_bw() +
  labs(fill = "Habitat Diversity") +
  scale_fill_viridis_c()

p6 = ggplot() +
  geom_sf(data = ca) +
  geom_sf(data = mpa_data, aes(fill = temp)) +
  theme_bw() +
  labs(fill = "Average Temp") +
  scale_fill_viridis_c()

plot = (p1 + p6) / (p3 + p2) / (p4 + p5) + plot_annotation(tag_levels = "A")
plot

ggsave(plot, file = paste0("mpa_summary.pdf"), path = here::here("figs"), height = 15, width = 15)
