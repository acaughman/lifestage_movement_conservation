library(here)
library(rerddap)
library(lubridate)
library(tidyverse)

lat_min <- 33.35
lat_max <- 34.5
lon_min <- -120.75
lon_max <- -118.95

lat <- c(lat_min, lat_max)
lon <- c(lon_min, lon_max)

tm <- c(
  "2003-01-02T12:00:00Z",
  '2019-12-30T12:00:00Z'
)

data <- 'jplMURSST41'
field <- 'analysed_sst'

murSST <- griddap(
  datasetx = data,
  latitude = lat,
  longitude = lon,
  time = tm,
  fields = field
) 

SST = murSST$data 
rm(murSST)

SST = SST %>% 
  dplyr::rename(t = time, temp = analysed_sst) %>%
  na.omit() %>% 
  mutate(latitude = as.character(latitude),
         longitude = as.character(longitude))

write_csv(SST, here::here("data", "raw_data", "SST.csv"))

# # Define a function to download data -------------------------------------------
# OISST_sub_dl <- function(tm, field, dataset, lat, lon) {
#   OISST_dat <- griddap(
#     datasetx = dataset,
#     time = tm,
#     zlev = c(0, 0),
#     latitude = lat,
#     longitude = lon,
#     fields = "field"
#   )$data %>%
#     mutate(time = as.Date(stringr::str_remove(time, "T00:00:00Z"))) %>%
#     dplyr::rename(t = time, temp = field) %>%
#     dplyr::select(longitude, latitude, t, temp) %>%
#     na.omit()
# }
