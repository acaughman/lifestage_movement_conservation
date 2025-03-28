library(tidyverse)
library(sf)
library(terra)

hab_25 = rast(here::here("data", "raw_data", "habitat", "WCDSCI_EXPRESS_25m_0_1200mWD.tif"))
hab_200 = rast(here::here("data", "raw_data", "habitat", "WCDSCI_EXPRESS_200m_0_1200mWD.tif"))

hab_bio = st_read(here::here("data", "raw_data", "habitat", "PMEP_Nearshore_Zones_and_Habitat_V2_bio.gdb"))
hab_sub = st_read(here::here("data", "raw_data", "habitat", "PMEP_Nearshore_Zones_and_Habitat_V2_sub.gdb"))
