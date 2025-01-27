################################################################################
#
# Juan Carlos Villaseñor-Derbez
# juancvd@stanford.edu
# date
#
# Description
#
# Download NOAA AVHRR Optimum Interpolation v2.1 - SST
# Gap-Filled,  0.05° /5km  •  1981-Now
# NOAA/National Centers for Environmental Information (NCEI) 1/4 Degree Daily
# Optimum Interpolation Sea Surface Temperature (OISST) Analysis,
# Version 2.1 - Inter. NOAAs 1/4-degree Daily Optimum Interpolation Sea Surface
# Temperature (OISST) (sometimes referred to as Reynolds SST, which however also
# refers to earlier products at different resolution), currently available as
# version v02r01, is created by interpolating and extrapolating SST observations
# from different sources, resulting in a smoothed complete field.
################################################################################

## SET UP ######################################################################

# Load packages ----------------------------------------------------------------
pacman::p_load(
  here,
  rerddap,
  lubridate,
  tidyverse
)

# Define a function to download data -------------------------------------------
# OISST_sub_dl <- function() {
#   OISST_dat <- griddap(
#     datasetx = "jplMURSST41",
#     url = "https://coastwatch.pfeg.noaa.gov/erddap/",
#     time = c("2014-01-01", "2014-01-10"),
#     latitude = c(-89.99, 89.99),
#     longitude = c(-179.99, 180.0),
#     fields = "analysed_sst"
#   )$data %>%
#     mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>%
#     dplyr::rename(t = time, temp = analysed_sst) %>%
#     dplyr::select(longitude, latitude, t, temp) %>%
#     na.omit()
# }

# Load data --------------------------------------------------------------------
# dl_years <- tibble(
#   date_index = 1:1,
#   start = ymd(c()),
#   end = ymd(c("2014-01-10")))

mur_data = griddap(
  datasetx = "jplMURSST41",
  url = "https://coastwatch.pfeg.noaa.gov/erddap/",
  time = c("2014-01-01", "2014-01-10"),
  latitude = c(-89.99, 89.99),
  longitude = c(-179.99, 180.0),
  fields = "analysed_sst"
)$data %>%
  mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>%
  dplyr::rename(t = time, temp = analysed_sst) %>%
  dplyr::select(longitude, latitude, t, temp) %>%
  na.omit()

# OISST_data <- dl_years %>% 
#   group_by(date_index) %>% 
#   group_modify(~OISST_sub_dl(.x)) %>% 
#   ungroup() %>% 
#   select(longitude, latitude, t, temp)




