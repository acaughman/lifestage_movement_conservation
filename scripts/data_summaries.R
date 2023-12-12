library(tidyverse)
library(patchwork)
library(spData)
library(sf)
library(lme4)
library(jtools)
library(marginaleffects)
library(MASS)

data = read_csv(here::here("data", "processed_data", "pisco_SST.csv")) %>% 
  filter(site_status == "MPA") %>% 
  filter(level != "CAN") %>% 
  group_by(sciname, length_to_use, year, transect, date, affiliated_mpa, level, zone) %>% 
  mutate(total_count = sum(count),
         biomass= sum(weight_kg)) %>% 
  ungroup() %>% 
  dplyr::select(-transect, -level, -zone) %>% 
  group_by(sciname, year, date, affiliated_mpa) %>% 
  mutate(total_count = sum(total_count)) %>% 
  distinct() %>% 
  mutate(move_combo = paste(magnitude_homerange, month_pld)) 


# Species Summary ---------------------------------------------------------

scinames = data %>% 
  dplyr::select(sciname, common_name, month_pld, magnitude_homerange) %>% 
  distinct()

# write_csv(scinames, here::here("data", "overlap_species.csv"))

move_data = data %>% 
  dplyr::select(sciname, magnitude_homerange, month_pld) %>% 
  distinct()

p1 = ggplot(move_data, aes(magnitude_homerange, month_pld)) +
  geom_jitter(aes(color = as.factor(magnitude_homerange), shape = as.factor(month_pld)), size = 3) +
  theme_bw() +
  labs(x = "Home Range Magnitude",
       y = "Month PLD",
       color = "Home Range Magnitude",
       shape = "Month PLD") +
  scale_color_viridis_d()
p1

ggsave(p1, file = paste0("species_move_summary.pdf"), path = here::here("figs"), height = 8, width = 10)

species_sum = data %>% 
  dplyr::select(sciname, move_combo) %>% 
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
  dplyr::select(name) %>% 
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
  dplyr::select(affiliated_mpa, settlement_mpa_total, implementation_date, size, 
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
  geom_sf(data = ca, fill = "transparent") +
  geom_sf(data = mpa_data, aes(fill = affiliated_mpa)) +
  theme_bw() +
  labs(fill = "MPA Name") +
  scale_fill_viridis_d()

p2 = ggplot() +
  geom_sf(data = ca, fill = "transparent") +
  geom_sf(data = mpa_data, aes(fill = settlement_mpa_total)) +
  theme_bw() +
  labs(fill = "Total Settlement") +
  scale_fill_viridis_c()

p3 = ggplot() +
  geom_sf(data = ca, fill = "transparent") +
  geom_sf(data = mpa_data, aes(fill = size)) +
  theme_bw() +
  labs(fill = "MPA Size") +
  scale_fill_viridis_c()

p4 = ggplot() +
  geom_sf(data = ca, fill = "transparent") +
  geom_sf(data = mpa_data, aes(fill = prop_rock)) +
  theme_bw() +
  labs(fill = "Proportion Rock") +
  scale_fill_viridis_c()

p5 = ggplot() +
  geom_sf(data = ca, fill = "transparent") +
  geom_sf(data = mpa_data, aes(fill = habitat_diversity)) +
  theme_bw() +
  labs(fill = "Habitat Diversity") +
  scale_fill_viridis_c()

p6 = ggplot() +
  geom_sf(data = ca, fill = "transparent") +
  geom_sf(data = mpa_data, aes(fill = temp)) +
  theme_bw() +
  labs(fill = "Average Temp") +
  scale_fill_viridis_c()

plot = (p1 + p6) / (p3 + p2) / (p4 + p5) + plot_annotation(tag_levels = "A")
plot

ggsave(plot, file = paste0("mpa_summary.pdf"), path = here::here("figs"), height = 15, width = 15)

# time series summary -----------------------------------------------------

species_num = data %>% 
  group_by(sciname) %>% 
  summarize(count = n())

species_mpa_num = data %>% 
  group_by(sciname, affiliated_mpa) %>% 
  summarize(count = n()) %>% 
  filter(count > 10) %>% 
  group_by(sciname) %>% 
  mutate(mpa_count = n()) %>% 
  filter(mpa_count > 1) %>% 
  left_join(scinames)

mpa_num = species_mpa_num %>% 
  group_by(affiliated_mpa) %>% 
  summarize(count_ind = sum(count),
            count_species = n())

counts = species_mpa_num %>% 
  dplyr::select(sciname, magnitude_homerange, month_pld) %>% 
  distinct()

p1 = ggplot(counts, aes(magnitude_homerange, month_pld)) +
  geom_jitter(aes(color = as.factor(magnitude_homerange), shape = as.factor(month_pld)), size = 3) +
  theme_bw() +
  labs(x = "Home Range Magnitude",
       y = "Month PLD",
       color = "Home Range Magnitude",
       shape = "Month PLD") +
  scale_color_viridis_d()
p1

ggsave(p1, file = paste0("species_move_summary_largeN.pdf"), path = here::here("figs"), height = 8, width = 10)

sub_data = data %>% 
  filter(sciname %in% species_mpa_num$sciname) %>% 
  dplyr::select(-count)

sub_count = sub_data %>% 
  dplyr::select(-length_to_use, -weight_g, -weight_kg, -biomass) %>% 
  distinct()

ggplot(sub_count, aes(sciname, total_count, color = move_combo)) +
  geom_boxplot() +
  theme_bw()+
  facet_wrap(~affiliated_mpa, scales = "free") +
  scale_y_log10()+
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_viridis_d() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

ggplot(sub_data, aes(sciname, biomass, color = move_combo)) +
  geom_boxplot() +
  theme_bw()+
  facet_wrap(~affiliated_mpa, scales = "free") +
  scale_y_log10()+
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_viridis_d() +
  theme(axis.text.x = element_text(angle = 45, hjust=1))

ggplot(sub_data, aes(date, biomass, color = sciname)) +
  geom_line(aes(group = length_to_use)) +
  theme_bw()+
  facet_wrap(~affiliated_mpa, scales = "free")+
  scale_color_viridis_d() 

# predictors summary ------------------------------------------------------

ggplot(sub_count) +
  geom_histogram(aes(total_count), bins = 5) +
  facet_grid(month_pld~magnitude_homerange, scales = "free")

ggplot(sub_count, aes(size, total_count, color = move_combo)) +
  geom_jitter(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname, scales = "free") +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "glm", se = FALSE,  method.args = list(family = "poisson")) +
  scale_color_viridis_d()

ggplot(sub_count, aes(as.factor(implementation_date), total_count, color = move_combo)) +
  geom_boxplot(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname, scales = "free") +
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_viridis_d()

ggplot(sub_count, aes(habitat_richness, total_count, color = move_combo)) +
  geom_jitter(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname, scales = "free") +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "glm", se = FALSE,  method.args = list(family = "poisson")) +
  scale_color_viridis_d()

ggplot(sub_count, aes(habitat_diversity, total_count, color = move_combo)) +
  geom_jitter(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname, scales = "free") +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "glm", se = FALSE,  method.args = list(family = "poisson")) +
  scale_color_viridis_d()

ggplot(sub_count, aes(prop_rock, total_count, color = move_combo)) +
  geom_jitter(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname, scales = "free") +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "glm", se = FALSE,  method.args = list(family = "poisson")) +
  scale_color_viridis_d()

ggplot(sub_count, aes(fishing_pressure, total_count, color = move_combo)) +
  geom_jitter(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname, scales = "free") +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "glm", se = FALSE,  method.args = list(family = "poisson")) +
  scale_color_viridis_d()

ggplot(sub_count, aes(as.factor(mpa_defacto_designation), total_count, color = move_combo)) +
  geom_boxplot(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname, scales = "free") +
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_viridis_d()

ggplot(sub_count, aes(temp, total_count, color = move_combo)) +
  geom_jitter(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname, scales = "free") +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "glm", se = FALSE,  method.args = list(family = "poisson")) +
  scale_color_viridis_d()

ggplot(sub_count, aes(date, total_count, color = move_combo)) +
  geom_jitter(alpha = 0.2) +
  theme_bw() +
  facet_wrap(~sciname, scales = "free") +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "glm", se = FALSE,  method.args = list(family = "poisson")) +
  scale_color_viridis_d()

glms = sub_count %>% 
  group_by(move_combo, sciname) %>% 
  do(model = glmer(total_count ~ scale(size) +  
                     temp + 
                     prop_rock +
                     (1|date), 
                   data = ., 
                   family = poisson))

glms$model

# predictors biomass ------------------------------------------------------

ggplot(sub_data) +
  geom_histogram(aes(log(biomass)), bins = 10)+
  facet_grid(month_pld~magnitude_homerange, scales = "free")

ggplot(sub_data, aes(size, biomass, color = move_combo)) +
  geom_jitter(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname, scales = "free") +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d()

ggplot(sub_data, aes(as.factor(implementation_date), biomass, color = move_combo)) +
  geom_boxplot(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname, scales = "free") +
  theme(strip.background = element_rect(fill = "transparent")) +
  #geom_smooth(method = "loess", se = FALSE) +
  scale_y_log10() +
  scale_color_viridis_d()

ggplot(sub_data, aes(habitat_richness, biomass, color = move_combo)) +
  geom_jitter(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname, scales = "free") +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d()

ggplot(sub_data, aes(habitat_diversity, biomass, color = move_combo)) +
  geom_jitter(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname, scales = "free") +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d()

ggplot(sub_data, aes(prop_rock, biomass, color = move_combo)) +
  geom_jitter(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname, scales = "free") +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d()

ggplot(sub_data, aes(fishing_pressure, biomass, color = move_combo)) +
  geom_jitter(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname, scales = "free") +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d()

ggplot(sub_data, aes(as.factor(mpa_defacto_designation), biomass, color = move_combo)) +
  geom_boxplot(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname) +
  theme(strip.background = element_rect(fill = "transparent")) +
  #geom_smooth(method = "loess", se = FALSE) +
  scale_y_log10() +
  scale_color_viridis_d()

ggplot(sub_data, aes(temp, biomass, color = move_combo)) +
  geom_jitter(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname, scales = "free") +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d()

ggplot(sub_data, aes(date, biomass, color = move_combo)) +
  geom_jitter(alpha = 0.5) +
  theme_bw() +
  facet_wrap(~sciname, scales = "free") +
  theme(strip.background = element_rect(fill = "transparent")) +
  geom_smooth(method = "lm", se = FALSE) +
  scale_color_viridis_d()


glms2 = sub_data %>% 
  group_by(sciname, move_combo) %>% 
  do(model = lmer(log(biomass) ~ scale(size) + 
                    temp +
                    prop_rock +
                     (1|date) +
                    (1|length_to_use), 
                   data = .,))

glms2$model
