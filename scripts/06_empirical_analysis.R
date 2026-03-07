library(tidyverse)
library(patchwork)
library(sf)

# RF data ---------------------------------------------------------

rf <- read_csv(here::here("data", "raw_data", "rf_data.csv")) %>%
  mutate(pld_dist = ((pi * (1.33 * (pld)^1.3)^2)) / 100 / 2) %>%
  filter(!is.na(homerange)) %>%
  filter(!is.na(pld)) %>%
  mutate(dominate = case_when(
    round(homerange, 2) < round(pld_dist, 2) ~ "PLD",
    round(homerange, 2) >= round(pld_dist, 2) ~ "Home range"
  ))

dominate_sum <- rf %>%
  group_by(dominate) %>%
  summarize(count = n())

percent_dominate_adult <- dominate_sum$count[1] / sum(dominate_sum$count) * 100

# Data Load and Initial Manipulation --------------------------------------

full_pisco <- read_csv(here::here("data", "raw_data", "full_pisco.csv")) %>%
  distinct() %>%
  # remove canopy
  filter(level != "CAN") %>%
  dplyr::select(-latitude, -longitude) %>% # remove lat, long for site maps below
  distinct() %>%
  # get average count per transect in each MPA and ref per year and species
  group_by(year, affiliated_mpa, sciname, site_status) %>%
  mutate(
    count = mean(count, na.rm = TRUE),
    biomass = mean(weight_kg, na.rm = TRUE)
  ) %>%
  replace_na(list(biomass_per = 0, count_per = 0)) %>%
  ungroup()  %>%
  #remove old grouping variables
  dplyr::select(-weight_kg, -length_to_use, -weight_g, -day, -month, -zone, -transect, -level, -site, -region4) %>%
  distinct() %>%
  # remove species without full scientific names
  filter(!is.na(sciname)) %>%
  filter(!grepl("spp", sciname)) %>%
  # remove data from within 2 years of MPA implementation
  mutate(imp_year = year(mdy(implementation_date)))%>%
  filter(year > imp_year)%>%
  filter(year > imp_year + 2) %>%
  # remove outliers withinspecies
  group_by(sciname) %>%
  mutate(
    q1 = quantile(count, probs = 0.25),
    q3 = quantile(count, probs = 0.75)
  ) %>%
  filter(count < q3 + (2.5 * (q3 - q1))) %>%
  filter(count > q1 - (2.5 * (q3 - q1)))%>%
  group_by(sciname, affiliated_mpa) %>% 
  # remove species without at least 10 data points
  mutate(species_count = n())%>%
  filter(species_count > 3) %>%
  dplyr::select(-species_count, -q1, -q3, -imp_year, -implementation_date)%>%
  # calcluate species average across mpa and site status
  group_by(affiliated_mpa, sciname, site_status) %>%
  mutate(
    avg_count = mean(count, na.rm = TRUE),
    avg_biomass = mean(biomass, na.rm = TRUE)
  )

sub_pisco <- full_pisco %>%
  filter(sciname %in% c(
    "Paralabrax clathratus",
    "Medialuna californiensis",
    "Ophiodon elongatus",
    "Acanthoclinus fuscus",
    "Phanerodon furcatus",
    "Trachurus symmetricus",
    "Hypsypops rubicundus",
    "Embiotoca jacksoni",
    "Caulolatilus princeps",
    "Semicossyphus pulcher"
  )) %>%
  filter(target_status == "Targeted") %>%
  # based on randomforest data or expert opinion
  mutate(movement = case_when(
    sciname == "Paralabrax clathratus" ~ "low / high",
    sciname == "Medialuna californiensis" ~ "med / high",
    sciname == "Ophiodon elongatus" ~ "med / high",
    sciname == "Acanthoclinus fuscus" ~ "low / high",
    sciname == "Phanerodon furcatus" ~ "med / low",
    sciname == "Trachurus symmetricus" ~ "high / high",
    sciname == "Hypsypops rubicundus" ~ "low / high",
    sciname == "Embiotoca jacksoni" ~ "low / low",
    sciname == "Caulolatilus princeps" ~ "low / low",
    sciname == "Semicossyphus pulcher" ~ "low / high"
  )) %>%
  mutate(hr_cat = case_when(
    sciname == "Paralabrax clathratus" ~ "low HR",
    sciname == "Medialuna californiensis" ~ "med HR",
    sciname == "Ophiodon elongatus" ~ "med HR",
    sciname == "Acanthoclinus fuscus" ~ "low HR",
    sciname == "Phanerodon furcatus" ~ "med HR",
    sciname == "Trachurus symmetricus" ~ "high HR",
    sciname == "Hypsypops rubicundus" ~ "low HR",
    sciname == "Embiotoca jacksoni" ~ "low HR",
    sciname == "Caulolatilus princeps" ~ "low HR",
    sciname == "Semicossyphus pulcher" ~ "low HR",
  )) %>%
  filter(!is.na(movement)) %>%
  mutate(affiliated_mpa = fct_reorder(affiliated_mpa, min_dist, .desc = FALSE)) %>%
  mutate(mpa_designation = case_when(
    mpa_defacto_designation == "ref" ~ "reference",
    TRUE ~ "MPA"
  )) %>%
  dplyr::select(-mpa_defacto_designation, -mpa_state_designation)%>%
  mutate(movement = fct_relevel(movement, "low / low", "low / high", "med / low", "med / high", "high / high")) %>%
  mutate(hr_cat = fct_relevel(hr_cat, "low HR", "high HR"))

mpa_sites <- sub_pisco %>%
  filter(mpa_designation == "MPA") %>%
  distinct() %>%
  ungroup() %>%
  rename(mpa_count = avg_count) %>%
  dplyr::select(-mpa_designation, -biomass, -year, -count, -avg_biomass, -site_status) %>%
  distinct()
ref_sites <- sub_pisco %>%
  filter(mpa_designation == "reference") %>%
  distinct() %>%
  ungroup() %>%
  rename(ref_count = avg_count) %>%
  dplyr::select(-mpa_designation, -biomass, -year, -count, -avg_biomass, -site_status) %>%
  distinct()

wider_pisco <- full_join(mpa_sites, ref_sites) %>%
  mutate(in_out = log(mpa_count / ref_count)) %>%
  filter(!is.na(in_out)) %>%
  dplyr::select(sciname, affiliated_mpa, in_out)

sub_pisco <- list(sub_pisco, wider_pisco) %>%
  reduce(full_join) %>% 
  select(affiliated_mpa, size, min_dist, in_out, sciname, movement, site_status, hr_cat) %>% 
  distinct() %>% 
  filter(site_status == "MPA")

sub_pisco %>%
  ungroup() %>%
  dplyr::select(sciname, movement) %>%
  distinct() %>%
  group_by(movement) %>%
  mutate(n = n())


# LMs ---------------------------------------------------------------------

low_low = sub_pisco %>% 
  filter(movement == "low / low")

lm_low_low = lm(in_out ~ size, data = low_low)
summary(lm_low_low)

# low_high = sub_pisco %>% 
#   filter(movement == "low / high")
# 
# lm_low_high = lm(in_out ~ size, data = low_high)
# summary(lm_low_high)

high_high = sub_pisco %>% 
  filter(movement == "high / high")

lm_high_high = lm(in_out ~ size, data = high_high)
summary(lm_high_high)

kb = sub_pisco %>% 
  filter(sciname == "Paralabrax clathratus")
lm_kb = lm(in_out ~ size, data = kb)
summary(lm_kb)

cs = sub_pisco %>% 
  filter(sciname == "Semicossyphus pulcher")
lm_cs = lm(in_out ~ size, data = cs)
summary(lm_cs)

# in/out figs --------------------------------------------------------------

col <- viridis::viridis(n = 9, end = 0.9)[c(1, 3, 9)]

p6 <- ggplot(sub_pisco %>% filter(site_status == "MPA"), aes(size, in_out, color = movement, group = sciname)) +
  geom_hline(aes(yintercept = 0), alpha = 0.5, linetype = "dashed", color = "red") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 2) +
  geom_point(aes(shape = sciname), size = 3, alpha = 0.5) +
  theme_bw(base_size = 16) +
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_manual(values = col) +
  facet_wrap(~hr_cat, scales = "free_y") +
  scale_shape_manual(values = c("circle", "circle", "square", "circle")) +
  guides(shape = "none") +
  labs(y = "log(response)", x =  expression(CA~MPA~Size~(km^2)), color = "Movement (Adult / Larval)")
p6 # make colors match colors from other fig

p8 <- ggplot(sub_pisco %>% filter(site_status == "MPA"), aes(min_dist, in_out, color = movement, group = sciname)) +
  geom_hline(aes(yintercept = 0), alpha = 0.2, linetype = "dashed") +
  geom_smooth(method = "lm", se = FALSE, linewidth = 2) +
  geom_point(aes(shape = sciname), size = 3, alpha = 0.5) +
  theme_bw(base_size = 16) +
  theme(strip.background = element_rect(fill = "transparent")) +
  scale_color_manual(values = col) +
  # facet_wrap(~movement, scales = "free_y") +
  labs(y = "log(response)", x = "MPA Spacing", color = "Movement (Adult/Larval)") + 
  scale_shape_manual(values = c("circle", "circle", "square", "circle")) +
  guides(shape = "none") 
p8 # add icons

# MPA Sup Fig -------------------------------------------------------------

mpas <- full_pisco %>%
  ungroup() %>%
  dplyr::select(affiliated_mpa, size, min_dist) %>%
  distinct()

ci <- st_read(here::here("data", "raw_data", "channel_islands.shp"))
mpa1 <- st_read(here::here("data", "raw_data", "WDPA", "WDPA_shp_0", "WDPA_WDOECM_Nov2023_Public_1b091e2b51c258eb3b24328044a4a3d6979e00fa2029d7538aef2befd46da362_shp-polygons.shp")) %>%
  st_transform(st_crs(ci))
mpa2 <- st_read(here::here("data", "raw_data", "WDPA", "WDPA_shp_1", "WDPA_WDOECM_Nov2023_Public_1b091e2b51c258eb3b24328044a4a3d6979e00fa2029d7538aef2befd46da362_shp-polygons.shp")) %>%
  st_transform(st_crs(ci))
mpa3 <- st_read(here::here("data", "raw_data", "WDPA", "WDPA_shp_2", "WDPA_WDOECM_Nov2023_Public_1b091e2b51c258eb3b24328044a4a3d6979e00fa2029d7538aef2befd46da362_shp-polygons.shp")) %>%
  st_transform(st_crs(ci))

mpa_names1 <- as.data.frame(unique(mpa1$NAME))
mpa_names2 <- as.data.frame(unique(mpa2$NAME))
mpa_names3 <- as.data.frame(unique(mpa3$NAME))

mpa1 <- mpa1 %>%
  filter(NAME == "Anacapa Island State Marine Conservation Area" | grepl("Painted", NAME))
mpa2 <- mpa2 %>%
  filter(NAME == "Carrington Point (Santa Rosa Island) State Marine Reserve" |
           NAME == "Gull Island (Santa Cruz Island) State Marine Reserve" |
           NAME == "Harris Point (San Miguel Island) State Marine Reserve" |
           NAME == "Anacapa Island State Marine Reserve" |
           NAME == "Santa Barbara Island State Marine Reserve" |
           NAME == "Scorpion (Santa Cruz Island) State Marine Reserve" |
           NAME == "South Point (Santa Rosa Island) State Marine Reserve")
mpa3 <- mpa3 %>%
  filter(NAME == "Campus Point State Marine Conservation Area" |
           NAME == "Naples State Marine Conservation Area")

mpa_sf <- list(mpa1, mpa2, mpa3) %>%
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
  )) %>%
  full_join(mpas)

rm(mpa1, mpa2, mpa3, mpa_names1, mpa_names2, mpa_names3)

p1 <- ggplot() +
  geom_sf(data = ci, fill = "transparent") +
  geom_sf(data = mpa_sf, aes(fill = size)) +
  theme_bw(base_size = 16) +
  labs(fill = "MPA Size") +
  scale_fill_viridis_c()

p2 <- ggplot() +
  geom_sf(data = ci, fill = "transparent") +
  geom_sf(data = mpa_sf, aes(fill = min_dist)) +
  theme_bw(base_size = 16) +
  labs(fill = "MPA Spacing") +
  scale_fill_viridis_c()

p3 <- ggplot(mpas) +
  geom_point(aes(size, min_dist)) +
  theme_bw(base_size = 16) +
  labs(x = "MPA Size", y = "MPA Spacing")

plot = (p1 + p2) / p3 + plot_annotation(tag_levels = "A")

ggsave(plot, filename = here::here("figs", "figS12.pdf"), height = 10, width = 15)
