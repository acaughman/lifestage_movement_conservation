library(here)
library(lubridate)
library(tidyverse)
library(sf)
library(raster)
library(fasterize)

# Data Load ---------------------------------------------------------------

pisco = read_csv(here::here("data", "raw_data", "kelp_biomass.csv"))[, -1] %>% 
  janitor::clean_names() %>% 
  dplyr::select(year, month, day, region4, mpa_state_designation, mpa_defacto_designation, site, zone, level, transect, count, sciname, family, target_status, weight_g, weight_kg, length_to_use) 
mpa_traits = read_csv(here::here("data", "raw_data", "mpa_traits.csv"))[, -1] %>% 
  janitor::clean_names() %>% 
  dplyr::select(-state_region)
sites = read_csv(here::here("data", "raw_data", "PISCO_kelpforest_site_table.csv")) %>% 
  janitor::clean_names() %>% 
  dplyr::select(site, mpa_name, latitude, longitude, site_status) %>% 
  mutate(mpa_name = str_to_lower(mpa_name)) %>% 
  rename(affiliated_mpa = mpa_name)
homerange_rf = read_csv(here::here("data", "raw_data", "homerange_rf_predictions.csv")) %>% 
  janitor::clean_names() %>% 
  mutate(homerange = case_when(
    is.na(observed_homerange) ~ predicted_homerange, 
    !is.na(observed_homerange) ~ observed_homerange
  )) %>% 
  dplyr::select(species, family, common_name, homerange) 
pld_rf = read_csv(here::here("data", "raw_data", "pld_rf_predictions.csv")) %>% 
  janitor::clean_names() %>% 
  mutate(pld = case_when(
  is.na(observed_pld) ~ predicted_pld, 
  !is.na(observed_pld) ~ observed_pld
)) %>% 
  dplyr::select(species, family, common_name, pld) 


# Calc Min Dist -----------------------------------------------------------

ci = st_read(here::here("data", "raw_data", "channel_islands.shp"))
mpa1 <- st_read(here::here("data", "raw_data", "WDPA", "WDPA_shp_0", "WDPA_WDOECM_Nov2023_Public_1b091e2b51c258eb3b24328044a4a3d6979e00fa2029d7538aef2befd46da362_shp-polygons.shp")) %>% 
  st_transform(st_crs(ci))
mpa2 <- st_read(here::here("data", "raw_data", "WDPA", "WDPA_shp_1", "WDPA_WDOECM_Nov2023_Public_1b091e2b51c258eb3b24328044a4a3d6979e00fa2029d7538aef2befd46da362_shp-polygons.shp"))%>% 
  st_transform(st_crs(ci))
mpa3 <- st_read(here::here("data", "raw_data", "WDPA", "WDPA_shp_2", "WDPA_WDOECM_Nov2023_Public_1b091e2b51c258eb3b24328044a4a3d6979e00fa2029d7538aef2befd46da362_shp-polygons.shp"))%>% 
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
  )) 

rm(mpa1, mpa2, mpa3, mpa_names1, mpa_names2, mpa_names3)


### Calculate distance between MPAs
mpa_centroids <- st_centroid(mpa_sf)

r <- raster(extent(mpa_sf), nrow = 100, ncol = 100)
ca_raster <- fasterize(ci, r)
ca_points <- ca_raster

xy <- st_coordinates(mpa_centroids)
icell <- cellFromXY(ca_raster, xy)

ca_points[icell[1]] <- 2
ca_points[icell[2]] <- 3
ca_points[icell[3]] <- 4
ca_points[icell[4]] <- 5
ca_points[icell[5]] <- 6
ca_points[icell[6]] <- 7
ca_points[icell[7]] <- 8
ca_points[icell[8]] <- 9
ca_points[icell[9]] <- 10
ca_points[icell[10]] <- 11
ca_points[icell[11]] <- 12

d1 <- gridDistance(ca_points, origin = 2, omit = 1) / 1000
d2 <- gridDistance(ca_points, origin = 3, omit = 1) / 1000
d3 <- gridDistance(ca_points, origin = 4, omit = 1) / 1000
d4 <- gridDistance(ca_points, origin = 5, omit = 1) / 1000
d5 <- gridDistance(ca_points, origin = 6, omit = 1) / 1000
d6 <- gridDistance(ca_points, origin = 7, omit = 1) / 1000
d7 <- gridDistance(ca_points, origin = 8, omit = 1) / 1000
d8 <- gridDistance(ca_points, origin = 9, omit = 1) / 1000
d9 <- gridDistance(ca_points, origin = 10, omit = 1) / 1000
d10 <- gridDistance(ca_points, origin = 11, omit = 1) / 1000
d11 <- gridDistance(ca_points, origin = 12, omit = 1) / 1000

mpa_sf$min_dist <- NA
mpa_sf$max_dist <- NA

mpa_sf$max_dist[1] <- max(d1[icell])
mpa_sf$min_dist[1] <- min(d1[icell[-1]])
mpa_sf$max_dist[2] <- max(d2[icell])
mpa_sf$min_dist[2] <- min(d2[icell[-2]])
mpa_sf$max_dist[3] <- max(d3[icell])
mpa_sf$min_dist[3] <- min(d3[icell[-3]])
mpa_sf$max_dist[4] <- max(d4[icell])
mpa_sf$min_dist[4] <- min(d4[icell[-4]])
mpa_sf$max_dist[5] <- max(d5[icell])
mpa_sf$min_dist[5] <- min(d5[icell[-5]])
mpa_sf$max_dist[6] <- max(d6[icell])
mpa_sf$min_dist[6] <- min(d6[icell[-6]])
mpa_sf$max_dist[7] <- max(d7[icell])
mpa_sf$min_dist[7] <- min(d7[icell[-7]])
mpa_sf$max_dist[8] <- max(d8[icell])
mpa_sf$min_dist[8] <- min(d8[icell[-8]])
mpa_sf$max_dist[9] <- max(d9[icell])
mpa_sf$min_dist[9] <- min(d9[icell[-9]])
mpa_sf$max_dist[10] <- max(d10[icell])
mpa_sf$min_dist[10] <- min(d10[icell[-10]])
mpa_sf$max_dist[11] <- max(d11[icell])
mpa_sf$min_dist[11] <- min(d11[icell[-11]])

rm(d1, d2, d3, d4, d5, d6, d7, d8, d9, d10, d11, mpa_centroids, ca_points, ca_raster, ci, r, xy, icell)

mpa_sf <- mpa_sf %>%
  st_drop_geometry() %>%
  dplyr::select(affiliated_mpa, max_dist, min_dist)

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

pisco_merge = mpa_traits %>% 
  full_join(sites) %>% 
  distinct() %>% 
  filter(!is.na(size)) %>% 
  filter(!is.na(site)) %>% 
  full_join(mpa_sf) %>% 
  full_join(pisco) %>% 
  filter(!is.na(count)) %>% 
  filter(affiliated_mpa %in% mpas_of_interest)

write_csv(pisco_merge, here::here("data", "raw_data", "full_pisco.csv"))
